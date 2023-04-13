/* SLIRP preprocessing extension {{{
 * This code augments the S-Lang preprocessor, giving coders the capability
 * to define their own preprocessing tokens in either C or S-Lang code; it
 * forms the basis of the SLIRP annotation mechanism, but could also be
 * used separately.  In S-Lang scope new preprocessor tokens are registered
 * via preproc_handler_add(): this associates the token with a callback
 * (S-Lang function), indicates how blocks opened with the token must be
 * terminated (e.g. is it a single-line token, or does it require "#end"),
 * and optionally registers an arbitrary number of arguments to pass to
 * the callback each time it's invoked.
 * }}} */

#include "config.h"

#include <stdio.h>
#include <slang.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/stat.h>

#include <slang.h>
#include "version.h"

/* #include "preproc-module.h" */

SLANG_MODULE(preproc);

/* Local types and variables {{{ */

#define MAX_FILE_LINE_LEN 256	/* matches sltoken.c */

typedef int	(*SLprep_Handler)	(char *line, void *data);
static int	SLprep_handler_add	(char *open_pattern,
					char *close_pattern,
      					SLprep_Handler h,
					void *data);

static void	SLprep_handler_remove	(char *open_pattern);
static int	SLprep_handler_exists	(char *open_pattern);
static int	SLprep_handler_passthru	(char *handler_pattern,
					char* passthru_pattern);

typedef struct _PassThru PassThru;
struct _PassThru {
   char *pattern;
   size_t pat_len;
   PassThru *next;
};

typedef struct _Handler_Entry Handler_Entry;
struct _Handler_Entry {
   char *open_pat;		/* open block token */
   size_t open_pat_len;		/* optimization: avoids unnecc strlen calls */
   char *close_pat;		/* close block token */
   size_t close_pat_len;
#define ALLOW_UNTERM	0x01
   unsigned int flags;
   SLprep_Handler handler;
   PassThru *pt_head;		/* stuff which may pass thru sans complaint */
   PassThru *pt_tail;
   void *data;			/* extra data to pass in @ handler invocation */
   Handler_Entry *prev;
   Handler_Entry *next;
};

static int 		preproc_callback_hook 	(char *buf, char prep_char);
static Handler_Entry 	*Handler_Entries_Head;
static Handler_Entry	*Handler_Entries_Tail;
static Handler_Entry	*Current;
static int 		(*Previous_Load_File_Ns_Hook) 	(char *, char *);
static int		(*Previous_Load_File_Hook)	(char *);

static const char* EMPTY_CLOSE_PATTERN = "";

/*}}}*/

/* Custom file loader (adapted from S-Lang C routines in sltoken.c) {{{ */

/* Preprocessor exists hook  {{{ */
static int prep_exists_function (SLprep_Type *pt, char *line)
{
   char buf[MAX_FILE_LINE_LEN], *b, *bmax;
   unsigned char ch;
   unsigned char comment;

   (void) pt;
   bmax = buf + (sizeof (buf) - 1);

   comment = (unsigned char)'%';
   while (1)
     {
	/* skip whitespace */
	while ((ch = (unsigned char) *line),
	       ch && (ch != '\n') && (ch <= ' '))
	  line++;

	if ((ch <= '\n')
	    || (ch == comment)) break;

	b = buf;
	while ((ch = (unsigned char) *line) > ' ')
	  {
	     if (b < bmax) *b++ = (char) ch;
	     line++;
	  }
	*b = 0;

	if (SLang_is_defined (buf))
	  return 1;
     }

   return 0;
}
/* }}} */

/* Preprocessor eval hook {{{ */
static int prep_eval_expr (SLprep_Type *pt, char *expr)
{
   int ret;
   char *end;
   (void) pt;

   end = strchr (expr, '\n');
   if (end == NULL)
     end = expr + strlen (expr);
   expr = SLmake_nstring (expr, (unsigned int) (end - expr));
   if (expr == NULL)
     return -1;

   if ((0 != SLang_load_string (expr))
       || (-1 == SLang_pop_integer (&ret)))
     ret = -1;
   else
     ret = (ret != 0);

   SLfree (expr);
   return ret;
} /* }}} */

typedef struct { char *buf; FILE *fp; SLprep_Type *pp;} File_Client_Data;

static char *read_from_file (SLang_Load_Type *x) /* {{{ */
{
   File_Client_Data *c = (File_Client_Data*)x->client_data;

   while (1) {

	if (fgets (c->buf, MAX_FILE_LINE_LEN, c->fp) == NULL) {

	   /* Have we hit EOF without closing current prepr block? */
	   if (Current != NULL) {

			/* ... ok if block can be a single line */
		if (Current->flags & ALLOW_UNTERM) {
		   Current->flags ^=  ALLOW_UNTERM;
		   Current = NULL;
		}
		else	/* ... otherwise not ok */
		   SLang_verror (SL_SYNTAX_ERROR,
			"Unterminated <#%s> directive", Current->open_pat);
	   }

	   return NULL;
	}

	/* The extra prep_line_ok call made here helps ensure that
	   elision blocks, such as #iffalse/#endif, are properly
	   honored; without it the custom tokens registered by
	   users via the routines herein would pass through the
	   preprocessor (since they begin with '#') */

	if ( SLprep_line_ok(c->buf, c->pp)
			&& SLprep_line_ok("x;", c->pp)
			&& preproc_callback_hook (c->buf, '#') == 1)
	   break;

	x->line_num++;
   }
   return c->buf;
} /* }}} */

/* Local implementation of _SLpath_find_file {{{ */
static char *more_recent (char *a, char *b)
{
   unsigned long ta, tb;
   struct stat st;

   if (a == NULL)
     return b;
   if (b == NULL)
     return a;

   if (-1 == stat (a, &st))
     return b;
   ta = (unsigned long) st.st_mtime;
   if (-1 == stat (b, &st))
     return a;
   tb = (unsigned long) st.st_mtime;

   if (tb >= ta)
     return b;

   return a;
}

/* returns SLmalloced string */
static char *find_file (char *path, char *file)
{
   char *dirfile;
   char *extname;
   char *filebuf;
   char *filesl, *fileslc;
   unsigned int len;

   if (NULL != (dirfile = SLpath_find_file_in_path (path, file)))
     return dirfile;

   /* Not found, or an error occured. */
   if (SLang_get_error())
     return NULL;

   extname = SLpath_extname (file);
   if (*extname != 0)
     return NULL;

   /* No extension.  So look for .slc and .sl forms */
   len = (extname - file);
   filebuf = SLmalloc (len + 5);
   strcpy (filebuf, file);
   strcpy (filebuf + len, ".sl");

   filesl = SLpath_find_file_in_path (path, filebuf);
   if ((filesl == NULL) && SLang_get_error())
     {
	SLfree (filebuf);
	return NULL;
     }
   strcpy (filebuf + len, ".slc");
   fileslc = SLpath_find_file_in_path (path, filebuf);
   SLfree (filebuf);

   dirfile = more_recent (filesl, fileslc);

   if (dirfile != filesl)
     SLfree (filesl);
   if (dirfile != fileslc)
     SLfree (fileslc);

   return dirfile;
}

static char *_SLpath_find_file (char *file)
{
   char *load_path, *path;
   char *dirfile;

   if (file == NULL)
     return NULL;

   load_path = SLpath_get_load_path();
   if ((load_path == NULL) || (*load_path == 0))
	path = ".";
   else
	path = load_path;

   dirfile = find_file (path, file);

   SLang_free_slstring (load_path);

   if (dirfile != NULL)
     {
	file = SLang_create_slstring (dirfile);
	SLfree (dirfile);
	return file;
     }

   SLang_verror (SL_OBJ_NOPEN, "Unable to locate %s on load path", file);
   return NULL;
}
/* }}} */

static int load_file_ns (char *f, char *ns_name) /* {{{ */
{
   File_Client_Data client_data;
   SLang_Load_Type *x;
   char *name, *buf;
   FILE *fp;

   name = _SLpath_find_file (f);
   if (name == NULL)
     return -1;

   if (NULL == (x = SLns_allocate_load_type (name, ns_name)))
     {
	SLang_free_slstring (name);
	return -1;
     }

   buf = NULL;

   if (f != NULL) {
      int v;

      fp = fopen (name, "r");
      v = SLang_load_file_verbose (0);
      if (v)
	{
	   SLang_vmessage ("Loading %s", name);
	   (void) SLang_load_file_verbose (v);
	}
   }
   else
	fp = stdin;

   if (fp == NULL)
     SLang_verror (SL_OBJ_NOPEN, "Unable to open %s", name);
   else if (NULL != (buf = SLmalloc (MAX_FILE_LINE_LEN + 1)))
     {

	SLprep_Type *pp;

	if ( (pp = SLprep_new()) == NULL)
	   return -1;

	SLprep_set_flags(pp, SLPREP_BLANK_LINES_OK | SLPREP_COMMENT_LINES_OK);
	SLprep_set_eval_hook (pp, prep_eval_expr);
	SLprep_set_exists_hook (pp, prep_exists_function);

	client_data.fp  = fp;
	client_data.buf = buf;
	client_data.pp  = pp;
	x->client_data  = (VOID_STAR) &client_data;
	x->read = read_from_file;

	(void) SLang_load_object (x);

	SLprep_delete(pp);
     }

   if ((fp != NULL) && (fp != stdin))
     fclose (fp);

   SLfree (buf);
   SLang_free_slstring (name);
   SLdeallocate_load_type (x);

   if (SLang_get_error())
     return -1;

   return 0;
}

static int load_file(char *f)
{
   return load_file_ns(f, NULL);
} /* }}} */

/* }}} */

/* SLang handler interface {{{ */
typedef struct _SLang_Marshaller {
  SLang_Name_Type *function;
  SLang_Any_Type  **args;
  unsigned int    nargs;
} SLang_Marshaller;

static int
slang_function_invoke(char *line, void *data)
{
   unsigned int arg = 0;
   SLang_Marshaller *m = (SLang_Marshaller*)data;

   if (SLang_start_arg_list() == -1)
	return -1;

   if (line == NULL) {
	if (SLang_push_null() == -1)
	   return -1;
   }
   else if (SLang_push_string(line) == -1)
	return -1;

   if (m->args) {
      for (arg = 0; arg < m->nargs; arg++)
	  if (SLang_push_anytype(m->args[arg]) == -1)
	     break;
   }

   if (SLang_get_error() || SLang_end_arg_list() == -1) {
	SLdo_pop_n(arg);
	return -1;
   }

   /* Temporarily disable our file loaders, since the S-Lang code
      we're about to invoke might cause another file to be loaded. */
   SLang_Load_File_Hook = Previous_Load_File_Hook;
   SLns_Load_File_Hook = Previous_Load_File_Ns_Hook;

   (void) SLexecute_function(m->function);

   SLns_Load_File_Hook = load_file_ns;
   SLang_Load_File_Hook = load_file;

   /* SLang_Error will propagate back to caller */

   return 0;
}

static int pop_string_or_null(char **pstr)
{
   if (SLANG_NULL_TYPE == SLang_peek_at_stack ()) {
	*pstr = NULL;
	return SLang_pop_null ();
   }
   return SLang_pop_slstring (pstr);
}

static int slang_args_extract(unsigned int nargs, SLang_Any_Type ***pargs)
{
   SLang_Any_Type **args;
   SLang_Any_Type *arg;
   unsigned int narg;

   if (nargs <= 0) {
	*pargs = NULL;
	return 0;
   }

   args = (SLang_Any_Type**) SLmalloc( sizeof(char*) * nargs);
   narg = nargs;
   while (narg) {
	if (SLang_pop_anytype(&arg) == -1) {
	   while (nargs > narg)
		SLang_free_anytype(args[--nargs]);
	   SLfree((char*)args);
	   return -1;
	}
	args[--narg] = arg;

   }
   *pargs = args;
   return 0;
}

static void slang_args_free(unsigned int nargs, SLang_Any_Type **args)
{
   while (nargs--)
	SLang_free_anytype(args[nargs]);
   SLfree((char*)args);
}

static void slang_marshaller_free(SLang_Marshaller *m)
{
   if (m != NULL) {
	slang_args_free(m->nargs, m->args);
	SLang_free_function(m->function);
	SLfree((char*)m);
   }
}

static SLang_Marshaller*
slang_marshaller_new(SLang_Ref_Type **slfunc_ref, SLang_Any_Type **args,
      							unsigned int nargs)
{
   SLang_Marshaller *f;
   SLang_Name_Type *slfunc;

   if ( (slfunc = SLang_get_fun_from_ref(*slfunc_ref)) == NULL)
      return NULL;

   SLang_free_ref(*slfunc_ref);
   *slfunc_ref = NULL;

   if ( (f = (SLang_Marshaller*) SLmalloc(sizeof(SLang_Marshaller))) == NULL) {
	SLang_free_function(slfunc);
	return NULL;
   }

   f->function	= slfunc;
   f->args	= args;
   f->nargs	= nargs;

   return f;
}

static SLang_Marshaller* slang_function_pop(unsigned int num_args_to_omit)
{
   SLang_Marshaller *f;
   SLang_Any_Type **args = NULL;
   SLang_Ref_Type *func_ref = NULL;
   unsigned int nargs = SLang_Num_Function_Args - num_args_to_omit - 1;

   if (slang_args_extract(nargs,&args) == 0
      		 && SLang_pop_ref(&func_ref) == 0
		 && (f = slang_marshaller_new(&func_ref, args, nargs)))
	return f;

   if (args) slang_args_free(nargs,args);
   if (func_ref) SLang_free_ref(func_ref);

   return NULL;
}

static int usg_err(int expected_nargs, const char *usage_str)
{
   if (SLang_Num_Function_Args < expected_nargs) {
	SLang_verror (SL_USAGE_ERROR, "Usage: %s", usage_str);
	return -1;
   }
   return 0;
}

/*}}}*/

/* Passthru interface {{{*/
static int passthru(Handler_Entry *h, char *line)
{
   PassThru *pt = h->pt_head;

   while (pt) {
	if (!strncmp(line, pt->pattern, pt->pat_len))
	   return 1;
	pt = pt->next;
   }
   return 0;
}

/* }}} */

/* General handler interface {{{*/

#define	IS_WHITE(c)	((c == ' ' || c == '\t') ? 1 : 0)

static Handler_Entry* handler_is_defined(char **buf)
{
   char *pattern = *buf;
   Handler_Entry *h = Handler_Entries_Head;

   while (h) {

	/* FIXME: use hash table if preproc module ever goes standalone */
	if (!strncmp(pattern, h->open_pat , h->open_pat_len)) {

	   pattern += h->open_pat_len;

	   /* Check false match: next char may ONLY be open paren or whitesp */
	   if (isspace((int)*pattern) || *pattern == '(' || *pattern == 0) {

		while (IS_WHITE(*pattern))
		   pattern++;
		*buf = pattern;
		break;
	   }
	}
	h = h->next;
   }
   return h;
}

static int handler_invoke (char *line, Handler_Entry *e)
{
   return e->handler(line, e->data);
}

static void handler_free(Handler_Entry *e)
{
   if (e->handler == slang_function_invoke)
	slang_marshaller_free( (SLang_Marshaller*) e->data );

   SLfree(e->open_pat);
   SLfree(e->close_pat);
   SLfree((char*)e);
}
/*}}}*/

/* Public S-Lang API {{{ */

static void handler_add_wrapper(void)
{
   char *op= NULL, *cp= NULL;
   SLang_Marshaller *m;

   if (usg_err(3,"status = prep_handler_add("
	    	"open_pat, close_pattern, func_ref [, arg1, ...] )")
	|| NULL == (m = slang_function_pop(2))
	|| -1 == pop_string_or_null(&cp)
	|| -1 == SLang_pop_slstring(&op))

	return;

   SLang_push_integer( SLprep_handler_add( op, cp, slang_function_invoke, m));
}

static void handler_remove_wrapper(void)
{
   char *open_pattern = NULL;

   if (usg_err(1,"prep_handler_remove(open_pattern)") ||
					-1 == SLang_pop_slstring(&open_pattern))
	return;

   SLprep_handler_remove(open_pattern);
   SLang_free_slstring(open_pattern);
}

static void handler_exists_wrapper(void)
{
   char *open_pattern = NULL;

   if (usg_err(1,"status = prep_handler_exists(open_pattern)") ||
					-1 == SLang_pop_slstring(&open_pattern))
	return;

   SLang_push_integer( SLprep_handler_exists(open_pattern) );
   SLang_free_slstring(open_pattern);
}

static void handler_passthru_wrapper(void)
{
   char *h_pattern = NULL;
   char *pt_pattern = NULL;

   if (usg_err(2,"prep_handler_passthru(handler_pattern, passhtru_pattern)") ||
		-1 == SLang_pop_slstring(&pt_pattern) ||
		-1 == SLang_pop_slstring(&h_pattern))
	return;

   SLang_push_integer( SLprep_handler_passthru(h_pattern, pt_pattern));
   SLang_free_slstring(h_pattern);
   SLang_free_slstring(pt_pattern);
}

static void activate(void)
{
   Previous_Load_File_Hook = SLang_Load_File_Hook;
   Previous_Load_File_Ns_Hook = SLns_Load_File_Hook;
   SLns_Load_File_Hook = load_file_ns;
   SLang_Load_File_Hook = load_file;
}

static void deactivate(void)
{
   Handler_Entry *e, *n = Handler_Entries_Head;

   while (n) {
	e = n;
	n = n->next;
	handler_free(e);
   }

   SLang_Load_File_Hook = Previous_Load_File_Hook;
   SLns_Load_File_Hook = Previous_Load_File_Ns_Hook;
}

static void allow_unterminated(void)
{
   if (Current != NULL)			  /* support single-line blocks for  */
	Current->flags |= ALLOW_UNTERM;	  /* tokens otherwise requiring #end */
}

#define I SLANG_INT_TYPE
#define V SLANG_VOID_TYPE
#define S SLANG_STRING_TYPE

static SLang_Intrin_Fun_Type Intrin_Funcs [] =
{
   MAKE_INTRINSIC_0("preproc_activate", activate, V),
   MAKE_INTRINSIC_0("preproc_deactivate", deactivate, V),
   MAKE_INTRINSIC_0("preproc_handler_add", handler_add_wrapper, V),
   MAKE_INTRINSIC_0("preproc_allow_unterminated", allow_unterminated, V),
   MAKE_INTRINSIC_0("preproc_handler_remove", handler_remove_wrapper, V),
   MAKE_INTRINSIC_0("preproc_handler_exists", handler_exists_wrapper, V),
   MAKE_INTRINSIC_0("preproc_handler_passthru", handler_passthru_wrapper, V),
   SLANG_END_INTRIN_FUN_TABLE
};


static SLang_IConstant_Type Module_IConstants [] =
{
   MAKE_ICONSTANT("_preproc_module_version", MODULE_VERSION_NUMBER),
   SLANG_END_ICONST_TABLE
};

static char *Module_Version_String = MODULE_VERSION_STRING;
static SLang_Intrin_Var_Type Intrin_Vars[] =
{
   MAKE_VARIABLE("_preproc_module_version_string", &Module_Version_String, SLANG_STRING_TYPE, 1),
   MAKE_VARIABLE("PREPROC_EMPTY_CLOSE_PATTERN", &EMPTY_CLOSE_PATTERN, S, 1),

   SLANG_END_INTRIN_VAR_TABLE
};

#undef I
#undef V
#undef S
/*}}}*/

/* Module interface {{{ */
static int preproc_callback_hook (char *buf, char prep_char) /*{{{*/
{
   int ret = 1;			/* let line pass through, sans inspection */

   if (Current != NULL) {

	Handler_Entry *e = Current;

	if (*buf == prep_char) {

	   buf++;

	   if (e->close_pat != NULL && !strncmp(buf, e->close_pat,
		 					e->close_pat_len)) {

		if (isspace( (int) *(buf + e->close_pat_len)))
		   Current = (Handler_Entry *)(buf = NULL);
		else
		   return 1;		/* malformed preprocessor block */
	   }
	   else if (Current->flags & ALLOW_UNTERM) {

		/* Terminate token block implicitly ... */
		if (handler_invoke(NULL, e) != 0)
		   return 1;

		/* turn off permission for next block 2b unterminated ... */
		Current->flags ^= ALLOW_UNTERM;
		Current = NULL;

		/* ... and see if this line needs preprocessing */
		return preproc_callback_hook(--buf, prep_char);
	   }
	   else if (passthru(e, buf))
		buf--;
	   else
	   {
		SLang_verror (SL_SYNTAX_ERROR,
			"Unterminated <#%s> directive", Current->open_pat);
		return 1;
	   }
	}

	ret = handler_invoke(buf, e);
   }
   else if (*buf == prep_char) {

	buf++;

	if ((Current = handler_is_defined(&buf)) != NULL) {
	   ret = handler_invoke(buf, Current);
	   if (Current->close_pat == NULL)
		Current = NULL;
	}
   }

   return ret;

} /*}}}*/

int init_preproc_module_ns (char *ns_name) /*{{{*/
{
   SLang_NameSpace_Type *ns;

   ns = SLns_create_namespace (ns_name);
   if (ns == NULL)
	return -1;

   if (-1 == SLns_add_intrin_fun_table (ns, Intrin_Funcs, "__PREPROC_MODULE__"))
	return -1;

   if (-1 == SLns_add_iconstant_table (ns, Module_IConstants, NULL))
     return -1;

   if (-1 == SLns_add_intrin_var_table (ns, Intrin_Vars, NULL))
	return -1;

   return 0;
} /*}}}*/

void deinit_preproc_module (void)
{
   deactivate();
}

/*}}}*/

/* {{{*/
static int SLprep_handler_add (char *open_pattern,
			char *close_pattern,
      			SLprep_Handler h,
			void *data)
{
   Handler_Entry *e;

   if (open_pattern == NULL || h == NULL)
	return -1;

   if ( NULL == (e = (Handler_Entry*) SLcalloc (1, sizeof(Handler_Entry))))
	return -1;

   /* Deep copy the block open/close patterns */
   e->open_pat_len = strlen(open_pattern);
   if (NULL == (e->open_pat = SLmalloc( e->open_pat_len + 1))) {
	SLfree((char*)e);
	return -1;
   }
   strcpy(e->open_pat, open_pattern);

   if (close_pattern == NULL)			/* NULL ok, just default it */
	close_pattern = "end";

   if (strcmp(close_pattern, EMPTY_CLOSE_PATTERN)) {
	e->close_pat_len = strlen(close_pattern);
	if (NULL == (e->close_pat = SLmalloc(e->close_pat_len + 1))) {
	   SLfree(e->open_pat);
	   SLfree((char*)e);
	   return -1;
	}
	strcpy(e->close_pat, close_pattern);
   }

   e->data = data;					/* may be NULL */
   e->handler = h;

   if (Handler_Entries_Head == NULL)
	Handler_Entries_Head = e;
   else {
	e->prev = Handler_Entries_Tail;
	Handler_Entries_Tail->next = e;
   }

   Handler_Entries_Tail = e;

   return 0;
}

static void SLprep_handler_remove (char *pattern)
{
   Handler_Entry* e = handler_is_defined(&pattern);
   if (e != NULL) {

	if (e->next == NULL)
	   Handler_Entries_Tail = e->prev;
	else
	   e->next->prev = e->prev;

	if (e->prev == NULL)
	   Handler_Entries_Head = e->next;
	else
	   e->prev->next = e->next;

	handler_free(e);
   }
}

static int SLprep_handler_exists (char *pattern)
{
   return (handler_is_defined(&pattern) != NULL);
}

static int SLprep_handler_passthru(char *handler_pattern, char* passthru_pattern)
{
   Handler_Entry *e = handler_is_defined(&handler_pattern);

   if (e != NULL) {

	PassThru *pt = (PassThru*) SLmalloc (sizeof(PassThru));
	if (pt == NULL) return -1;

	pt->pat_len = strlen(passthru_pattern);
	if (NULL == (pt->pattern = SLmalloc( pt->pat_len + 1))) {
	   SLfree((char*)pt);
	   return -1;
	}

	strcpy(pt->pattern, passthru_pattern);
	pt->next = NULL;

	if (e->pt_head == NULL)
	   e->pt_head = e->pt_tail = pt;
	else {
	   e->pt_tail->next = pt;
	   e->pt_tail = pt;
	}

	return 0;
   }

   return -1;
}

/*}}}*/
