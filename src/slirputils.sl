%  slirputils.sl:	SLIRP utility routines {{{
%
%  This file is part of SLIRP, the (Sl)ang (I)nte(r)face (P)ackage.
%
%  Copyright (c) 2003-2009 Massachusetts Institute of Technology
%  Copyright (C) 2002 Michael S. Noble <mnoble@space.mit.edu>
%
%  This software was partially developed by the MIT Center for Space
%  Research under contract SV1-61010 from the Smithsonian Institution.
%  
%  Permission to use, copy, modify, distribute, and sell this software
%  and its documentation for any purpose is hereby granted without fee,
%  provided that the above copyright notice appear in all copies and
%  that both that copyright notice and this permission notice appear in
%  the supporting documentation, and that the name of the Massachusetts
%  Institute of Technology not be used in advertising or publicity
%  pertaining to distribution of the software without specific, written
%  prior permission.  The Massachusetts Institute of Technology makes
%  no representations about the suitability of this software for any
%  purpose.  It is provided "as is" without express or implied warranty.
%  
%  THE MASSACHUSETTS INSTITUTE OF TECHNOLOGY DISCLAIMS ALL WARRANTIES
%  WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
%  MERCHANTABILITY AND FITNESS.  IN NO EVENT SHALL THE MASSACHUSETTS
%  INSTITUTE OF TECHNOLOGY BE LIABLE FOR ANY SPECIAL, INDIRECT OR
%  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
%  OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
%  WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. }}}

require ("slirpconf");
variable SLIRP_VERSION = 20000;
variable SLIRP_VERSION_STRING = "pre2.0.0-35";
% Front Matter: type, variable and forward declarations {{{ 

typedef struct {
   name,
   language,
   int_consts,
   long_consts,
   double_consts,
   string_consts,
   intrin_vars,
   private_vars,
   intrin_arrays,
   inlines,		% general inlined code (for body of translation unit)
   inlines_init,	% inlined code for module initialization fragment
   functions,
   prototypes,
   unresolved_macros
} Interface;

variable SlirpController = struct { % {{{

  gendir,		% directory into which code will be generated
  genstem,		% path-qualified stem of filenames for generated code

  infname,		% name of input file currently being parsed
  infp,			% FILE pointer to same
  outfp,		% current output FILE pointer
  parse_file,
  default_file_parser,
  default_language,
  parse_struct,
  line,			% current line of current input file

  prep_accept,		% stack of #if/#endif preproc directive truth values

  debug,
  verbose_help,
  autotype,
  cfront,		% indicates whether pure C wrappers should be generated
  emit_usage,
  usage_strings,
  assume_const_strings,
  always_pop_args,

  vectorize,		% attempt to vectorize every func, or not?
  openmp,		% will vectorized code be parallelized with OpenMP?
  openmp_flags,		% what compile/link flags to specify for OpenMP

  argc,
  tablevel,		% pretty printing mechanism
  eof,			% FIXME: work around slang 2.2 bug, fread eof not seen 
  decl,			% current declaration being constructed/examined/parsed

  funcprefix,		% generate code for all funcs which match this regexp
  renames,		% maps C function names to different S-Lang names

  interface,		% currently active interface (global or class namespace)
  global_interface,
  classes,
  cplusplus,		% indicates some input was, and all output must be, C++
  types,
  opaque_types,
  num_reserved_types,

  modname,		% name of module for which code is being generated
  obj_code,
  include_paths,	% -I options specified at invocation
  lib_paths,		% -L options specified at invocation
  lib_names,		% -l options specified at invocation
  ldflags,		% -ldflags options specified at invocation
  input_files,		% name(s) of source file(s) specified at invocation
  input_paths,		% path(s) to them (supplemented by -I include_paths)
  input_headers,	% subset of input_files, i.e. which are C/C++ headers
  include_headers,	% should input_headers be #included in wrapper code?

  fortran_strlen,	% where to place (hidden C-->Fortran) string len args
  fortran_wrapped,	% > 0 --> at least one input file was FORTRAN
			% > 1 --> include pack.c to handle string arrays

  fortran_wrapcommon,	% indicates whether common blocks will be wrapped

  have_refs,		% bit flag indicating whether generated module permits
			% passing SLang_Ref_Type args to wrappers:
			% 0x1 : refs.c needs to be injected into module
			% 0x2 : whether generated code will map scalars
			%	implicitly to references (e.g to satisfy
			%	FORTRAN pass-by-ref semantics, w/out
			%	requiring explicit use of '&' at call time
			% 0x4 : whether ref_get_size() is called at runtime

  have_null_term_str_arrays,

  print_interface,
  genstubs,
  stubs_fname,
  geninit,		% indicates whether module init fragment s/b emitted
  listopaques,		% indicates whether list of opaque types s/b emitted
  stripping_comments,
  gendebug,		% indicates whether debugging code will be generated

  root,			% root of tree from which SLIRP is running
  inject_lib,           % Where fils to inject are stored
  version,		% current SLIRP version
  rcfile,		% name of SLIRP resource file in use (if any)
  process_name,

  ignore,		% filename into which ignored symbols list is output
  ignore_fp,		% ... and its file pointer

  tmap_infile,		% name of file to which typemap will be dumped
  tmap_outfile,		% name of file form which typemap will be read 
  wrapper_prefix,	% initial part of generated function name

  callbacks

};  % }}}

public variable SC = @SlirpController,	% Slirp controller instance
		C = 1,			% language types
		CPLUSPLUS = 2,
		FORTRAN	= 3,
		EMPTY = "",
		VOID = "void",
		ROW_MAJOR = 0x0,	% array layouts in memory
		COLUMN_MAJOR = 0x4,
		NEWLINE = '\n', TAB = '\t', SPACE = ' ',
		ARG_SEPARATOR = ", ",
		ASTERISK="*",
		DEFS_COMMENT_CHAR =';',
		DEVNULL="/dev/null",
		PRE_GEN_CB  = "pre_gen",
		POST_GEN_CB = "post_gen",
		POST_INIT_CB = "post_init",
		PRE_FUNC_TABLE_CB = "pre_func_table",
		IN_FUNC_TABLE_CB = "in_func_table",
		EXIT_CB = "exit_cb",
		FOLD_OPEN = "/* {{{ */", FOLD_CLOSE = "/* }}} */";

private define instruct();
define get_typemap();
define finalize_ignore_list();
define slirp_include_rc();
define list_opaque_types();
define generate_prototype();
private define generate_makefile();
public define get_token();
public define get_line();
define swallow_rest_of_C_block();
define init_cfront();
define init_genstubs();

define dprintf()			% generic diagnostic output
{
   variable args = __pop_args(_NARGS);
   () = fprintf(stderr,__push_args(args));
   () = fprintf(stderr,"\n");
   () = fflush(stderr);
}

define lprintf()			% diagnostic messages, with line info
{
   variable args = __pop_args(_NARGS);
   () = fprintf(stderr,__push_args(args));
   () = fprintf(stderr," (near %s:%d)\n", SC.infname, SC.line);
   () = fflush(stderr);
}

define tprintf()			% diagnostic messages, with indentation
{ 
   variable args = __pop_args(_NARGS);
   loop(SC.tablevel)
      () = printf("\t");
   () = printf(__push_args(args));
   () = printf("\n");
   () = fflush(stdout);
}

define warn()
{
   variable args = __pop_args(_NARGS);
   () = fprintf(stderr,"Warning: ");
   () = fprintf(stderr,__push_args(args));
   () = fprintf(stderr,"\n");
   () = fflush(stderr);
}

public define Empty_Assoc_Array()	{ return  Assoc_Type[Any_Type, NULL]; }

SC.debug = 0;
SC.autotype = 1;
SC.line = 1;
SC.prep_accept = [1];			% outermost block: all code is accepted
SC.opaque_types = Struct_Type[0];
SC.callbacks = Empty_Assoc_Array();
SC.types = Empty_Assoc_Array();
SC.funcprefix = "^[a-zA-Z]+";
SC.wrapper_prefix = "sl_";
SC.cfront = 0;
SC.default_language = C;
SC.fortran_strlen = 0;
SC.fortran_wrapcommon = 1;
variable accepts_null_args = Empty_Assoc_Array();
variable returns_struct = Empty_Assoc_Array ();
variable discard_line_strings = {};

% }}}

% Linked lists {{{

variable List = struct { head, tail }; 
variable List_Elem = struct { name, value, next, prev };

define _list_append(list,node)	% O(1) insertion
{
   if (list.head == NULL) {
	list.head = node;
	list.tail = node;
   }
   else {
	node.prev = list.tail;
	list.tail.next = node;
	list.tail = node;
   }
}

define _list_delete(list,node)	% O(1) removal
{
   if (node.prev != NULL)
	node.prev.next = node.next;
   else
	list.head = node.next;

   if (node.next == NULL) list.tail = node.prev;
}

define list_node_new(name, value)
{
   variable node = @List_Elem;
   node.name = name;
   node.value = value;
   return node;
}

define add_const(hash, name, value)
{
   hash[SC.interface.name + name] = value;
}
% }}}

% File I/O and tokenization {{{

define abort()
{
   variable args = __pop_args(_NARGS);
   if (SC.infname == NULL)
	dprintf(__push_args(args));
   else
	lprintf(__push_args(args));	% give line info for header files
   exit(1);
}

define slirp_emit()
{
   variable args = __pop_list(_NARGS);
   () = fprintf(SC.outfp, __push_list(args));
}

define debug_emit ()
{
   variable args = __pop_list (_NARGS);
   if (1) return;
   slirp_emit (__push_list (args));
}

define emit_inlines(inlines)
{
   if (length(inlines)) slirp_emit("\n");
   foreach(inlines) { variable il = (); slirp_emit("%s",il); }
}

define register_callback()
{
   variable cb = struct{func, args};
   cb.args = __pop_args(_NARGS - 2);
   cb.func = ();

   variable kind = ();
   variable list = SC.callbacks[kind];
   if (list == NULL)
	list = Struct_Type[0];

   SC.callbacks[kind] = [cb, list];
}

define invoke_callbacks(kind)
{
   foreach(SC.callbacks[kind]) {
	variable cb = ();
	(@cb.func) ( __push_args(cb.args) );
   }
}

define warn_ignore(func, reason)
{
   if (SC.ignore_fp == NULL) {

	if (SC.ignore == "stderr")
	   SC.ignore_fp = stderr;
	else if (SC.ignore == "stdout")
	   SC.ignore_fp = stdout;
	else {

	   if (SC.ignore == "notrunc") {
		variable mode = "a";
		SC.ignore = "ignored.txt";
	   }
	   else
		mode = "w";

	   SC.ignore_fp = fopen(SC.ignore, mode);
	}
   }

   % Trick: call as (NULL, func, reason) to simulate NULL return value
   () = fprintf(SC.ignore_fp,"\n*** Ignored: %s\n*** Reason: %s\n",
							func, reason);

   assoc_delete_key(SC.interface.functions, func);
}

define warn_novec()
{
   variable description = __pop_args (_NARGS - 1), func = ();
   if (SC.interface.name != EMPTY) func = SC.interface.name + "::" + func;
   warn("%s not vectorized: %s", func, sprintf(__push_args(description)));
}

define eof_error(f)
{
   abort("unexpected EOF in "+ f);
}

#ifnexists atoi
public define atoi(str)	{ return int( atof(str) ); }
#endif

#ifnexists any
public define any(arr)  { return length( where(arr != 0) ); }
#endif

public define get_rest_of_line(fp)
{
   variable l = get_line(fp,0);
   while (l[-1] == '\\')		% support C-style line continuation
	l = l + get_line(fp,0);
   return l;
}

define swallow_C_style_comment(fp)
{
    variable line = get_rest_of_line(fp);

    SC.stripping_comments = 1;

    while(line[-1] != '/' && line[-2] != '*') {
	line = get_line(fp,1);
	if (line == NULL)
	    break;
    }

    SC.stripping_comments = 0;
}

define backup(fp, n)
{
   if (fseek(fp,-n,SEEK_CUR) == -1)
	verror("fseek error, near line: %S", SC.line);
}

private define read_byte (fp, chp)
{
   variable n = fread (chp, Char_Type, 1, fp);
   if ((n != -1) && (@chp == '\n')) SC.line++;
   return n;
}

define get_char (fp)
{
   variable ch;
   if (-1 == read_byte (fp, &ch))
     eof_error(_function_name);
   return ch;
}

define swallow_to(fp, delim)
{
   while (delim != get_char (fp))
     ;
}

private define next_char(fp)
{
   variable ch = get_char(fp);
   backup(fp,1);
   if (ch == '\n') SC.line--;
   return ch;
}

private define strip_comments(s, fp, is_token)
{
   if (SC.stripping_comments)
	return s;

   SC.stripping_comments = 1;

   forever {

	variable bc = is_substr(s,"/*");
	if (bc > 1 && s[bc-2] == '/')		% cull C++ false positives
	   bc = 0;

	if (bc) {
	   variable ec = string_match(s,"*\\/",bc);
	   if (ec) {
#ifeval _slang_version < 20000
		ec = ec + bc - 1;		% work around slang1 bug
#endif
		s = strtrim(strcat(substr(s,1,bc-1),substr(s,ec+2,-1)));
	   }
	   else {
		s = strtrim(substr(s,1,bc-1));
		forever {
		   swallow_to(fp, '*');
		   if (next_char (fp) == '/')
		     {
			() = get_char (fp);
			break;
		     }
		}
	   }
	}
	else {
	   bc = is_substr(s,"//"); 	% Handle C++ comments, too, even tho
	   if (bc) {			% they don't belong in C code proper
		if (is_token && next_char(fp) != '\n')
		() = get_rest_of_line(fp);
		s = strtrim(substr(s,1,bc-1));
	   }
	   break;
	}
   }

   SC.stripping_comments = 0;
   return s;
}

public define get_line(fp, okNULL)
{
   variable l;

   do {
	if (fgets(&l, fp) == -1) {
	   if (okNULL)
		return NULL;
	   else
		eof_error(_function_name);
	}

	SC.line++;

	% Compress whitespace (only kind of whitespace remaining is a space)
	l = strcompress(l," \t\r\n\f");

	% Ignore empty lines and Scheme comments (which s/b only w/in .defs)
	if (l != EMPTY && l[0] != DEFS_COMMENT_CHAR)
	   l = strip_comments(l, fp, 0);
   } while (l == EMPTY);		% return only non-blank lines

   return l;
}

define is_comment(fp, ch)
{
   if (ch == '/')
     {
	ch = get_char (fp);
	switch (ch)
	  { case '*' :	swallow_C_style_comment(fp); return 1; }
	  { case '/' :	() = get_rest_of_line(fp); return 1; }

        backup(fp, 1);
	if (ch == '\n') SC.line--;
   }

   return 0;
}

define eat_white(fp)
{
  forever
     {
	variable ch = get_char(fp);

	% Treat comments as whitespace (we support both C and C++ style)
	if (is_comment(fp, ch))
	  ch = get_char(fp);

	ifnot (isspace (ch))
	  break;
     }

   backup(fp, 1);
}

define eat_comment (fp)
{
   variable ch;
   
   forever
     {
	ch = get_char (fp);
	switch (ch)
	  { case ' ' : continue; }
	  { case '/' : 
	       if (next_char (fp) == '*')
		 {	
		    () = get_char (fp);
		    % read to the end of comment, which may not be EOL
		    forever
		      {
			 swallow_to (fp, '*');
			 if (next_char (fp) == '/')
			   {			    
			      () = get_char (fp);
			      return;
			   }
		      }		  
		 }	     
	     else if (next_char (fp) == '/')
	       {
		  % C++ style comment
		  swallow_to (fp, '\n');
	       }
	     else
	       {
		  % we should probably never be here ?
		  backup (fp, 1);
		  return;;
	       }	     
	  }
	  {
	     % no C / C++ comment
	     backup (fp, 1);
	     return;
	  }
     }   
}

public variable Single_Char_Tokens = Assoc_Type[Char_Type, 0];
public define add_tokens(hash, space_delimited_string)
{
   foreach (strtok(space_delimited_string, " \t")) {
	variable s = ();
	hash[s]++;			% increment reference count
   }
}
add_tokens(Single_Char_Tokens, "{ } : ( ) ! ;");

public define remove_tokens(hash, space_delimited_string)
{
   foreach (strtok(space_delimited_string, " \t")) {
	variable s = ();
	variable value = hash[s];
	if (value > 0) {
	   hash[s] = value - 1;		% decrement reference count
	}
   }
}

public define get_token(fp)
{
   % Simple tokenizer, looking mainly for whitespace-delimited
   % sequences.  Comments are automatically stripped out.

   variable ch, token = EMPTY;

   forever
     {
	if (-1 == read_byte (fp, &ch))	% eat leading whitesp
	  return token, SC.eof = 1;

	ifnot (isspace (ch))
	  break;
     }

   forever
     {
	variable strch = char(ch);

	if (Single_Char_Tokens[strch]) {

	   if (is_comment(fp, ch))
	     return get_token(fp);

	   if (token == EMPTY)
	     return strch;

	   backup(fp, 1);
	   if (ch == '\n') SC.line--;
	   break;
	}

	if (ch == '#')		% skip space after #, so as to combine
	  eat_white(fp);		% # and next literal into single token

	token = strcat (token, strch);

	if (SC.debug > 1)
	   dprintf("   get_token: token grown to <%S>", token);

 	if (-1 == read_byte (fp, &ch)) { SC.eof = 1; break; }
	if (isspace (ch))
	  break;
     }

   token = strip_comments(token, fp, ch != NEWLINE);
   if (token == EMPTY)
	return get_token(fp);

   return token;
}

public variable Legal_Const_Expr_Operator = Assoc_Type[ Char_Type, 0 ];
add_tokens(Legal_Const_Expr_Operator, "! ~ + - * & ( ) % / > < ^ | , ? " +
      					"!= && || >> >= << <= ==");

public define get_const_expr_token();
public define get_const_expr_token(fp)
{
   % Like get_token, except EOL is significant and operators are tokens
   % Useful for evaluating preprocessor conditional expressions

   variable ch, token = EMPTY;

   do {

	if (-1 == read_byte (fp, &ch))	% eat leading whitesp
	   return NULL, SC.eof = 1;

   } while (ch == ' ' || ch == '\t' || ch == '\f' || ch == '\r');

   forever {

      if (ch == NEWLINE)
	{
	   if (token != EMPTY) { SC.line--; backup(fp, 1); break; }
	   return NULL;
	}

	variable strch = char(ch);

	if (Legal_Const_Expr_Operator[strch]) {

	   if (token != EMPTY) { backup(fp, 1); break; }

	   token = strch;
	   ch = get_char(fp);
	   if (ch == ' ' || ch == '\t' || ch == '' || ch == '\r')
		return token;

	   if (ch == NEWLINE) { SC.line--; backup(fp, 1); return token; }

	   variable maybe_operator = token + char(ch);
	   if (Legal_Const_Expr_Operator[maybe_operator])
	      return maybe_operator;
	   else {
		if (maybe_operator == "/*")
		   token = maybe_operator;	% fall through to strip comment
		else {
		   backup(fp, 1);
		   return token;
		}
	   }
	}
	else {
	   if (ch == '\\') {
	      if ((-1 == read_byte (fp, &ch)) || (ch != NEWLINE))
		break;
	   }
	   else
		token = strcat(token, strch);
		if (Legal_Const_Expr_Operator[token])
		   break;
	}

 	if (-1 == read_byte (fp, &ch))
	   break;

	if (ch == ' ' || ch == '\t' || ch == '\f' || ch == '\r')
	   break;
   }

   token = strip_comments(token, fp, 1);
   if (token == EMPTY)
	return get_const_expr_token(fp);

   return token;
}

define unget_token(fp, tok)
{
   variable l = strbytelen(tok);
   if (l > 1) l++;
   backup(fp, l);
}

define next_token(fp) {
   variable prev = ftell(fp);
   variable l = SC.line;
   variable t = get_token(fp);
   () = fseek(fp, prev - ftell(fp), SEEK_CUR);
   SC.line = l;
   return t;
}

define open_block(fp,block_open_delimiter)
{
   eat_white(fp);
   variable l = SC.line;
   if (get_char(fp) != block_open_delimiter) {
      backup(fp, 1);
      SC.line = l;
      return 0;
   }
   return 1;
}

define open_C_block(fp)	   { return open_block(fp,'{'); }

define close_block(fp,block_close_delimiter)
{
   eat_white(fp);
   if (get_char(fp) != block_close_delimiter)
	abort("malformed block: %c expected",block_close_delimiter);
}

define close_C_block(fp) { close_block(fp,'}'); }

define swallow_rest_of_C_block(fp)
{
   do {
	variable token = get_token(fp);
	if (token == "{")
	   swallow_rest_of_C_block(fp);
   } while (token != "}");
}

define swallow_C_block(fp)
{
   swallow_to(fp, '{');
   swallow_rest_of_C_block(fp);
}

define get_struct_or_union(fp);	       %  recursion
define get_struct_or_union(fp)
{
   !if (open_C_block(fp)) {
	variable type = get_token(fp);
	!if (open_C_block(fp)) {
	   variable token = get_token(fp);
	   if (token != ";") {
		swallow_to(fp,';');		% simple typedef of other struct
		return type + " " + token;
	   }
	   return EMPTY;
	}
   }

   forever {
	token = get_token(fp);
	switch(token)
	{ case "struct" or case "union" : () = get_struct_or_union(fp); }
	{ case "}": break; }
   }
   token = get_token(fp);

   if (token != ";") {
	swallow_to(fp,';');
	return token;		% typedef name
   }

   return EMPTY;
}

define extract_enumerator(fp)
{
   eat_white(fp);
   variable name = "", need_value = 0, need_name = 1;

   forever {

	variable ch = get_char(fp);
	switch(ch)
	   { case ',' or case '}': backup(fp, 1); return name;	}
	   { case '=':
	      % if (need_value)
		% lprintf ("Confusing syntax seen for enum %S", name);
	      need_name = 0;
	      need_value = 1;
	   }
	   { case NEWLINE: ifnot (need_value) return name;}
           { case SPACE or case TAB :
	      need_name = 0;
	      eat_white (fp);
	      continue;
	   }
           { case '#' :
	      get_rest_of_line(fp);
	      eat_white (fp);
	   }
      	{
	 case '\'': % match those kind of things SDLK_0 = '0'
	   forever
	     {
		$0 = get_char (fp);
		if ($0 == '\\') % SDLK_BACKSLASH = '\\'
		  () = get_char (fp);
		if ($0 == '\'') % SDLK_QUOTE = '\''
		  break;
	     }	   
	}
	{
	 case '(':
	   while (get_char(fp) != ')');
	}
	   { case '/': 

	      variable l = SC.line;
	      switch (get_char(fp))
		{ case '*' : swallow_C_style_comment(fp); return name; }
		{ case '/' : () = get_rest_of_line(fp); return name; }

	      backup(fp, 1);	% else clause
	      SC.line = l;
	      continue;
	   }
      
	   {
	      if (need_name)
		name = strcat(name,char(ch));
	      else
		{
		   need_value = 1;
		}
	   }
   }
}

define more_enum_values(fp) {

   eat_white(fp);

   variable ch = get_char(fp);
   if (ch == ',') return 1;

   backup(fp, 1);
   if (ch == '\n') SC.line--;
   return (ch == '#');		% allow for preproc directives w/in definition
}

define get_match(s, which)
{ 
   variable pos, len;
   (pos, len) = string_match_nth(which);
   if (len == 0) return EMPTY;
   return s[[pos:pos+len-1]];
}

define get_matchc(s, which) { return strcompress(get_match(s,which)," \t"); }

define group_pointer_qualifiers_with_type(decl)
{
   % Assist parsing of identifier declarations, by ensuring that pointer
   % or reference qualifiers are grouped with the identifier type (not name)

   % First convert array specifiers of indeterminate size to generic pointers
   forever  {
	variable pos, len;
	pos = string_match(decl,"\\([_A-Za-z]+\\)[ \t]*\\[[ \t]*\\]\\(.*\\)",1);
	!if (pos) break;
	decl = sprintf("%s* %s%s", decl[[:pos-2]], get_match(decl, 1),
						  get_match(decl, 2));
   }

   (decl,) = strreplace(decl, " &", "&", 99);
   (decl,) = strreplace(decl, "\t&", "&", 99);
   (decl,) = strreplace(decl, " *", "*", 99);
   (decl,) = strreplace(decl, "\t*", "*", 99);
   forever  {
	pos = string_match(decl, "[*&][_A-Za-z]+", 1);
	!if (pos) break;
	decl = sprintf("%s %s", decl[[:pos-1]], decl[[pos:]]);
   }
   return decl;
}

define trim_around(string, pattern)
{
   variable patterns;
   if (pattern == "(" || pattern == ")")
	patterns = [ sprintf("\\([ \t]+\\)\\(%s\\)",pattern), 
   			 sprintf("\\(%s\\)\\([ \t]+\\)",pattern) ];
   else
	patterns = [ sprintf("\\([ \t]+\\)\\(\\%s\\)",pattern), 
   			 sprintf("\\(\\%s\\)\\([ \t]+\\)",pattern) ];

   foreach (patterns) {
	variable pat = (), substr;
	while (string_match(string, pat, 1)) {
		substr = get_match(string, 0);
		(string, ) = strreplace(string, substr, pattern, 1);
	}
   }
   return string;
}
% }}}

% Interface manipulators {{{
define save_interface(new_language)
{
   variable prev = @SC.interface;

   SC.interface.name = EMPTY;
   SC.interface.language = new_language;
   SC.interface.int_consts = Empty_Assoc_Array();
   SC.interface.long_consts = Empty_Assoc_Array();
   SC.interface.double_consts = Empty_Assoc_Array();
   SC.interface.string_consts = Empty_Assoc_Array();
   SC.interface.intrin_vars = Empty_Assoc_Array();
   SC.interface.private_vars = Empty_Assoc_Array();
   SC.interface.inlines = String_Type[0];
   SC.interface.inlines_init = String_Type[0];
   % intrin_arrays intentionally kept NULL
   SC.interface.functions  = Empty_Assoc_Array();
   SC.interface.prototypes = Empty_Assoc_Array();
   SC.interface.unresolved_macros = Empty_Assoc_Array();

   return prev;
}

define restore_interface(to)
{
   variable prev = @SC.interface;

   SC.interface.name = to.name;
   SC.interface.language = to.language;
   SC.interface.int_consts = to.int_consts;
   SC.interface.long_consts = to.long_consts;
   SC.interface.double_consts = to.double_consts;
   SC.interface.string_consts = to.string_consts;
   SC.interface.intrin_vars = to.intrin_vars;
   SC.interface.private_vars = to.private_vars;
   SC.interface.functions = to.functions;
   SC.interface.inlines = to.inlines;
   SC.interface.inlines_init = to.inlines_init;
   SC.interface.prototypes = to.prototypes;
   SC.interface.unresolved_macros = to.unresolved_macros;

   return prev;
}

private define print_consts(kind, consts)
{
   variable n = 0;
   if (length(consts)) {
	tprintf("Public %s:", kind);

	foreach(consts) using ("keys") {
	   variable name = ();
	   tprintf("   %S",name);
	   n++;
	}
   }
   return n;
}

define print_interface(interface, description) % {{{
{
   tprintf(""); tprintf(description); SC.tablevel++;

   SC.cfront = 1;				% reflect C types in arglists
   variable thing, numfuncs = 0, overload;
   tprintf("Public Wrapper Functions:");
   foreach (interface.functions) {
	thing = ();
	thing.retval.lname = EMPTY;
	thing.pop_args = 0;			% ensure arglists are non-void
	!if (length(thing.overloads)) {
	   tprintf("   %s;", generate_prototype(thing));
	   numfuncs++;
	}
	else {
	   variable overloads = 0;
	   foreach(thing.overloads) {
		variable ol = ();
		ol.pop_args = 0;		% ensure arglists are non-void
		ol.gname = ol.slname + string(overloads); 
		ol.retval.lname = EMPTY;
		tprintf("   %s;", generate_prototype(ol));
		numfuncs++;
		overloads++;
	   }
	}
   }

   variable numvars = print_consts("/ Intrinsic Variables",
	 						interface.intrin_vars);
   variable numconsts = print_consts("String Consts", interface.string_consts);
   numconsts += print_consts("Integer Consts", interface.int_consts);
   numconsts += print_consts("Long Consts", interface.long_consts);
   numconsts += print_consts("Double Consts", interface.double_consts);

   SC.tablevel--; tprintf("");

   return (numfuncs, numvars, numconsts);
} % }}}

define print_public_interface() % {{{
{
   variable nfuncs, nvars, nconsts;
   (nfuncs, nvars, nconsts) = print_interface(SC.interface,"Global Namespace");
   foreach(SC.classes) using ("values") {
	variable class = ();
	print_interface(class.interface, "Class: " + class.name);
	nconsts += ();
	nvars   += ();
	nfuncs  += ();
   }
   tprintf("Code Generation Summary:\n"+
	"\tFunctions (includes overloads, excludes destructors): %d\n"+
	"\tVariables : %d\n"+
	"\tConstants : %d\n\n",nfuncs, nvars, nconsts);

} % }}}

private define generate_stub(funcmap) % {{{
{
   if (funcmap.inlined) return;

   variable retval = funcmap.retval, body;
   if (retval.ltype != VOID)
	body = sprintf("return (%s%s) 0;", retval.const, retval.type);
   else
	body = EMPTY;

   retval.lname = EMPTY;

   slirp_emit("%s { %s }\n", generate_prototype(funcmap), body);
} % }}}

define generate_stubs(interface) % {{{
{
   variable thing, overload;
   foreach (interface.functions) {
	thing = ();
	if (thing.inlined)
	   continue;
	thing.pop_args = 0;			% ensure arglists are non-void
	!if (length(thing.overloads)) 
	   generate_stub(thing);
	else {
	   foreach(thing.overloads) {
		variable ol = ();
		ol.pop_args = 0;		% ensure arglists are non-void
		generate_stub(ol);
	   }
	}
   }
} % }}}

define nullify_struct(s) % {{{
{
  array_map(Void_Type, &set_struct_field, s, get_struct_field_names(s), [NULL]);
} % }}}

define struct_map(type, struct_arr, field) % {{{
{
#ifeval _slang_version < 20100
   !if (length(struct_arr)) return (@Array_Type)(type, 0);
#else
   !if (length(struct_arr)) return @Array_Type(type, 0);
#endif
   return array_map(type, &get_struct_field, struct_arr, field);
} % }}}

SC.interface = @Interface;
() = save_interface(C);
SC.global_interface = SC.interface;

define iterate_over_functions() % {{{
{
   variable args = __pop_args(_NARGS - 2), action = (), container = ();
   foreach(container.interface.functions) using ("values")
	(@action)((), __push_args(args));
} % }}}

define iterate_over_classes() % {{{
{
   variable args = __pop_args(_NARGS - 1), action = ();
   foreach(SC.classes) using ("values") {
	variable class = ();
	(@action)(class, __push_args(args));
   }
} % }}}

% }}}

% Initialization / Finalization {{{

define load_user_rc() % {{{
{
   %  This routine loads module-specific extensions, such as additional
   %  typemaps, macro customizations, ignored functions, etcetera.  The
   %  content will be sought in
   %
   %	SC.rcfile 		name of SLIRP resource file (a slang script)
   % or
   %	$SLIRPRC 		environment variable naming the resource file
   %
   %  If the -rc switch is not specified then SC.rcfile defaults to
   %  "./slirprc" (file in local directory).  If SC.rcfile does not
   %  exist then $SLIRPRC is checked.

   if (SC.tmap_infile != NULL)			% support loading arbitrary
	slirp_include_rc(SC.tmap_infile);	% typemaps via cmdline switch

   if (SC.rcfile == NULL) SC.rcfile = "./slirprc";
   if (stat_file (SC.rcfile) == NULL)
	SC.rcfile = getenv("SLIRPRC");

   if (SC.rcfile != NULL && stat_file (SC.rcfile) != NULL)
	() = evalfile(SC.rcfile);

   if (SC.listopaques) {
	list_opaque_types();
	exit(0);
   }
} % }}}

define optarg() % {{{
{
   variable default = NULL;
   if (_NARGS == 1) default = ();
   if (__argc > SC.argc + 2) {
	SC.argc++;
	return __argv[SC.argc];
   }
   return default;
} % }}}

private define is_build_arg(arg, option_name, array, test) % {{{
{
   % Check compile/link flags (used mainly w/in optional emitted Makefile)
   variable option_name_len = strlen(option_name);
   if (strncmp(arg, option_name, option_name_len)) return 0;

   variable value = arg[ [option_name_len : ] ];
   if (value == EMPTY) {
	value = optarg();
	if (value == NULL) value = EMPTY;
	% Attempt to treat quoted values sensibly
	if (value[0] == '"' || value[0] == '\'') {
	   variable terminator = value[0];
	   while (value[-1] != terminator) 
		value = strcat(value, optarg());
	}
	if (test && stat_file(value) == NULL)
	   value = EMPTY;
   }

   if (array != NULL && value != EMPTY)
	eval( sprintf("%s = [ %s, \"%s\"]", array, array, value) );

   return 1;
}  % }}}

define make_rename_regexp(pattern) { return "^" + pattern + "\\(.*\\)"; }
 
define emit_version() { % {{{
   () = fprintf (stdout, "SLIRP Version: %s\n", SLIRP_VERSION_STRING);
   () = fprintf (stdout, "SLang Library Version: %s\n", _slang_version_string);
} % }}}

define process_args() % {{{
{
   variable genmakef = 0;
   SC.argc = 1;
   while (SC.argc < __argc) {

     variable arg = __argv[SC.argc];
     switch(arg)

      { is_build_arg(arg, "-I", "SC.include_paths", 1) : }
      { is_build_arg(arg, "-L", "SC.lib_paths", 0) : genmakef = 1; }
      { is_build_arg(arg, "-ldflags", "SC.ldflags", 0) : genmakef = 1; }
      { is_build_arg(arg, "-l", "SC.lib_names", 0) : genmakef = 1; }
      % swallow -D / -U compiler #defines
      { is_build_arg(arg, "-D", NULL, 0) : ; }
      { is_build_arg(arg, "-U", NULL, 0) : ; }

      { case "-cfront":

	   SC.cfront = 1;
	   SC.wrapper_prefix = "c__";
	   init_cfront();
      }
      { case "-const_strings":	SC.assume_const_strings = 1; }
      { case "-c++": SC.cplusplus = 1; SC.default_language = CPLUSPLUS; }
      { case "-d" : 		SC.gendebug = 1; }
      { case "-fortran_strlen": SC.fortran_strlen = atoi(optarg(0)); }

      % Use an explicit string to choose which functions to
      % wrap instead of the regular expression given above
      { case "-fprefix" :

	   SC.funcprefix = "^" + optarg("");
	   if (SC.funcprefix == "^")
		dprintf ("-fprefix ignored, <func_prefix> "+
			      			"argument not supplied");
      }

      { case "-rename" or case "-mapnames" :

	   if (__argc > SC.argc+3) {
		variable orig = make_rename_regexp(__argv[SC.argc+1]);
		variable new  = __argv[SC.argc+2];
		if (new == "NULL") new = EMPTY;
		SC.argc += 2;
		SC.renames[orig] = new;
	   }
	   else {
		if (__argc > SC.argc+2) SC.argc++;  % skip orphaned argument
		() = fprintf (stderr,"-mapnames ignored: "+
			      	"Usage: -mapnames <regexp> <replacement>\n");
	   }
      }

      { not strncmp(arg, "-g", 2):

	   if (strlen(arg) > 3 && arg[2] == '=')
		SC.debug = integer(arg[[3:]]);
	   else
		SC.debug = 1;

	   _traceback = 1;
	   SC.gendebug = 1;
      }
      { case "-h" or case "-help" or case "--help" : instruct(0,0); }
      { case "-vh" or case "-vhelp" or case "--vhelp" : instruct(0,1); }
      { case "-ignore": SC.ignore = optarg(); }
      % openmp and vectorize fields are used as bit flag hints in VecSpec
      % & usage string routine; do not change here w/out looking there
      { case "-openmp" : SC.openmp = 2; SC.vectorize = 1; }
      { case "-otree" : SC.listopaques = 1; }
      { case "-m" : SC.modname = optarg(); }
      { case "-make" : genmakef = 1; }

      { case "-nocom" : SC.fortran_wrapcommon = 0; }
      % Use S-Lang arg transfer mechanism, instead of explicit arg popping
      { case "-nopop" : SC.always_pop_args = 0; SC.emit_usage = 0; }

      { case "-noinit" : SC.geninit= 0; }
      { case "-noincludes" : SC.include_headers = 0; }
      { case "-noautotype" : SC.autotype = 0; }
      { case "-print" : SC.print_interface = 1; }
      { case "-rc" : SC.rcfile = optarg(); }
      { case "-refscalars" : SC.have_refs |= 0x2; }
      { case "-stdout"  : SC.genstem  = "stdout"; }
      { case "-stubs"   : 
		SC.genstubs = 1;
		SC.wrapper_prefix = EMPTY;
		init_genstubs();
      }
      { case "-tmapin"  : SC.tmap_infile  = optarg(); }
      { case "-tmapout" : SC.tmap_outfile =  optarg(); }
      { case "-vec": SC.vectorize = 1; }
      { case "--version": emit_version(); exit(0); }
      % Note that -v option is intercepted within slshext.c
      { break; }		% Anything is considered header file input

      SC.argc++;
   }

   if (genmakef)
	register_callback(EXIT_CB, &generate_makefile);

}  % }}}

private define instruct(exitvalue, verbose) % {{{
{
   variable msg =
   "SLIRP: The (SL)ang (I)nte(R)face (P)ackage\nA S-Lang module generator "+
   "for C, C++, and FORTRAN.\nVersion: " + SLIRP_VERSION_STRING + "\n\n"+
   "Usage: slirp [OPTIONS]  headerfile [ headerfile ...] [object_file ...]\n"+
   "\n-c++                mandate that input be interepreted as C++"+
   "\n-cfront             generate standalone C wrappers (.cc and .h files),"+
   "\n                    instead of S-Lang bindings, for C++ interface"+
   "\n-const_strings      treat 'char*' return values as 'const char*'"+
   "\n-d                  include slirp_debug_pause() in generated module";

   if (verbose) msg+=
   "\n-fprefix <pref>     emit code only for functions which begin with an"+
   "\n                    exact match to <pref>, instead of default regexp";

   msg +=
   "\n-fortran_strlen <i> indicate where hidden string len args are placed in"+
   "\n                    wrappers of Fortran calls accepting CHARACTER args"+
   "\n                    0 = append length args to end of arg list (default)"+
   "\n                    1 = insert length args directly after CHARACTER arg"+
   "\n-g[=integer]        echo debug output to terminal, of varying verbosity"+
   "\n-h -help --help     this message";

   if (verbose) msg +=
   "\n-ignore <option>    tune the emission of ignored symbol messages:"+
   "\n                       use 'notrunc' to append to ignored symbol list"+
   "\n                              (default: truncate ignored symbol list)"+
   "\n                       or specify an alternate output file name";

   msg +=
   "\n-I  <dir>           add <dir> to search path for headers during code"+
   "\n                    generation; will also be reflected in -make output"+
   "\n-L  <dir>           add <dir> to search path at link time; for -make"+
   "\n-l  <lib>           add <lib> to library list at link time; for -make"+
   "\n-ldflags <flags>    use when -L or -l are inappropriate; for -make"+
   "\n-m  <name>          override default module name with <name>"+
   "\n-make               generate a best-effort Makefile for compiling module"+
   "\n                    (-make is also implied by -L, -l, and -ldflags)";

   if (verbose) msg +=
   "\n-mapnames <R> <S>   (deprecated) synonym for -rename";

   msg +=
   "\n-nocom              do not wrap common blocks"+
   "\n-noincludes         do not #include input headers within generated code"+
   "\n-noinit             do not generate an initialization fragment";

   if (verbose) msg +=
   "\n-nopop              avoid explicit pop of func arguments, if possible"+
   "\n                    (also turns off generation of Usage: statements)"+
   "\n-noautotype         disable mapping of unknown types to opaque ptr types";

   msg +=
   "\n-openmp             emit OpenMP #pragmas to parallelize vectorized code"+
   "\n                    (implies -vec, i.e. turns on vectorization)";

   if (verbose) msg +=
   "\n-otree              print hierarchical listing of all opaque types"+
   "\n                    defined by the current invocation, then exit"+
   "\n-print              print interface for code which would be generated,"+
   "\n                    but don't actually generate any code";

   msg +=
   "\n-rc  <file>         load customizations from <file>, rather than from"+
   "\n                    first of ./slirprc or $SLIRPRC";

   if (verbose) msg +=
   "\n-refscalars         support passing scalars as array/ref arguments"+
   "\n                    (default: on for FORTRAN bindings, off for C/C++)"+
   "\n-rename   <R> <S>   map C functions with prefixes matching the S-Lang"+
   "\n                    regexp R to S-Lang funcs beginning with string S"+
   "\n                    (set S=NULL to map regexp prefix to empty string)";

   msg +=
#ifeval _slang_version > 20000
   % "\n-sldb               run in interactive SLDB debugger (S-Lang2 only)" +
#endif
   "\n-stdout             emit code to stdout, instead of files";

   if (verbose) msg +=
   "\n-stubs              generate stub (empty) implementations for input"+
   "\n                    header files, allowing the module interface to be"+
   "\n                    exercised without linking in the underlying library"+
   "\n                    or any of its dependencies"+
   "\n-tmapout <file>     save a copy of the active typemap table to <file>"+
   "\n-tmapin  <file>     evalfile() additional typemaps from <file>";

   msg +=
   "\n-vec                attempt to vectorize every function"+
   "\n--version           output version information"+
   "\n-v                  show verbose loading messages"+
   "\n-vh -vhelp --vhelp  verbose help (--help plus rarely used options)\n\n"+
   sprintf("Default search path: %s\n", get_slang_load_path());
   
   () = fprintf(stderr, msg);

   exit(exitvalue);
}  % }}}

private define process_name() % {{{
{
   variable pname = __argv[0];
#ifdef WIN32
   pname = strtrans(pname, "\\", "/");
#endif
   return pname;
} % }}}

public define slirp_initialize() % {{{
{
   SC.version		= SLIRP_VERSION_STRING;

   if (__argc < 2)
      instruct(1,0);

   SC.gendir		= ".";
   SC.tablevel		= 0;
   SC.emit_usage	= 1;
   SC.usage_strings	= String_Type[0];
   SC.always_pop_args   = 1;
   SC.vectorize		= 0;
   SC.openmp		= 0;
   SC.openmp_flags	= SLIRP_OPENMP_FLAGS;
   SC.listopaques	= 0;
   SC.geninit		= 1;
   SC.include_headers   = 1;
   SC.genstubs		= 0;
   SC.gendebug		= 0;
   SC.num_reserved_types= 0;
   SC.input_headers	= String_Type[0];
   SC.input_files	= String_Type[0];
   SC.obj_code		= String_Type[0];
   SC.input_paths	= Empty_Assoc_Array();
   SC.include_paths	= String_Type[0];
   SC.lib_paths		= String_Type[0];
   SC.ldflags		= String_Type[0];
   SC.lib_names		= String_Type[0];
   SC.renames		= Empty_Assoc_Array();
   SC.ignore		= "ignored.txt";
   SC.process_name	= process_name();

   SC.cplusplus 	= 0;
   SC.classes		= Empty_Assoc_Array();
   SC.have_refs 	= 0x0;
   SC.fortran_wrapped 	= 0;
   SC.have_null_term_str_arrays = 0;
   SC.assume_const_strings = 0;

   _traceback = -1;
   _slangtrace = 1;

   process_args();

   % Save include paths, then morph it into a searchable path list
   foreach(SC.include_paths) {
	variable p = ();
	SC.input_paths[p] = EMPTY;
   }
   SC.include_paths = strjoin(SC.include_paths, char(path_get_delimiter()));
   preproc_activate();

   require("slirptypes");

   SC.have_refs = SC.have_refs & 0x2;	% Builtin annotations define refs
   load_user_rc();
   finalize_ignore_list;
} % }}}

public define slirp_get_version()	{ return SC.version; }

public define slirp_check_version(version) % {{{
{
   if (SC.version >= version) return;
   abort("SLIRP version required is %S, but you have %S",version,SC.version);
} % }}}

public define close_file(fp) % {{{
{
   % FIXME: workaround slang2.2 bug where fclose(stdout) causes infinite loop
   if (fp != NULL && fp != stdout) {
	() = fflush(fp);
	() = fclose(fp);
   }
} % }}}

private define is_object_code(file) % {{{
{
   return (path_extname(file) == ".o");
} % }}}

public define default_file_extension() % {{{
{
   if (SC.cplusplus)
      return ".cc";
   return ".c";
}  % }}}

public define create_output_file() % {{{
{
   variable fname, extension, thisfp = stdout;

   switch(_NARGS)
	{ case 1: fname = (); extension = default_file_extension(); }
	{ case 2: (fname , extension) = (); }
	{ error("incorrect number/type of arguments"); }

   if (extension == NULL) extension = EMPTY;

   if (SC.genstem != "stdout") {

	fname = sprintf("%s%s%s", SC.genstem, fname, extension);
	thisfp = fopen(fname,"w+");

	if (thisfp == NULL) {
	   () = fprintf(stderr,"Warning: could not create file: "+fname+
			 "\n\t\tI will try to emit to stdout instead");
	   thisfp = stdout;
	}
   }
   else fname = "stdout";

   return thisfp, fname;
}  % }}}

variable _handlers = Empty_Assoc_Array();

define slirp_find_file_in_path (path, f)
{
   variable delim = path_get_delimiter ();

   if (path == NULL)
     return NULL;
   foreach (strtok (path, char(delim)))
     {
        variable dir = ();
        variable file = path_concat (dir, f);

	variable st = stat_file (file);
	if ((st == NULL) || (stat_is ("dir", st.st_mode)))
	  continue;

	return file;
     }

   return NULL;
}

public define init_file_parser(originalf) % {{{
{
   variable fp = NULL, absolute = path_is_absolute (originalf);
   variable file_extension, handler, input, fullpath;

   SC.eof = 0;
   SC.stripping_comments = 0;
   SC.parse_file = SC.default_file_parser;

   SC.interface.language = SC.default_language;
   if (SC.default_language == C)
	SC.parse_struct = &get_struct_or_union;

   SC.line = 1;

   if (is_object_code(originalf)) {
	SC.obj_code = [ SC.obj_code, originalf ];
	return NULL;
   }

   if (absolute)
	fullpath = originalf;
   else {
	fullpath = slirp_find_file_in_path(SC.include_paths, originalf);
	if (fullpath == NULL)
	   fullpath = originalf;		% let it fail
	else
	   absolute = path_is_absolute (fullpath);
   }

   % For a language to be supported it must have an entry in the
   % _handlers array, each of which may operate in one of two ways:
   %
   %	- Feed a series of C prototypes back through the default
   %	  SLIRP C/C++ parsing machinery (which will then generate
   %	  the necessary FuncMapping table entries).  In this case
   % 	  the handler is expected to return the name of the file
   %	  containing the C prototypes.  The default handlers for
   %	  C and C++ operate in this fashion.
   %
   %	- Generate FuncMapping entries directly, by installing a
   %	  custom file parser into SC.parse_file, and returning a
   %	  pointer to the opened file.  The FORTRAN handler uses
   %	  this approach.
   %
   %	Either would be suitable for creating additional language
   %	handlers.

   if (stat_file (fullpath) != NULL) {

	if (originalf == DEVNULL)
	   handler = _handlers[".h"];
	else {
	   file_extension = strlow(path_extname(originalf));
	   handler = _handlers[file_extension];
	}

	if (handler == NULL)
	   dprintf("No language handler installed for: %s",originalf);
	else {
	   input = @handler(fullpath);
	   switch (typeof(input))
	   % Binary read mode used here for portability (e.g on Windows)
	   { case String_Type: fp = fopen(input,"rb"); }
	   { case File_Type:   fp = input; input = fullpath; }
	}
   }

   if (fp != NULL) {

	SC.infname = originalf;

	% Default modulename to stem of first file encountered
	if (SC.modname == NULL)
	   SC.modname = path_sans_extname(path_basename(originalf));

	% If this file specifies a new search path, remember it
	variable path = path_dirname(fullpath);
	if (fullpath != DEVNULL)
	   SC.input_paths[path] = EMPTY;	% assoc array culls duplicates


	% Use a plain, instead of associative, array for header names, to
	% preserve the order in which they were specified at invocation
	if (absolute and fullpath != DEVNULL)
	   input = path_basename(input);

	!if (length( where (SC.input_files == input)))
	   SC.input_files = [ SC.input_files, input];

	if (SC.genstem == NULL)
	   SC.genstem = SC.gendir + "/" + SC.modname;
   }
   else 
	dprintf("\nSkipped (unable to open / translate): %S", originalf);

   return fp;
}  % }}}

% }}}

% Opaque handling code {{{
define inject_file(fname)
{
   variable file = path_concat (SLIRP_INJECT_DIR, fname);
   variable buf, fp = fopen(file, "r");
   if (fp == NULL)
     throw OpenError, "Unable to open $file for injection"$;

   while (-1 != fgets(&buf, fp))
	if (-1 == fputs(buf, SC.outfp))
	   verror("I/O Error injecting line: <%s>\n"+
		  "from file: %s into generated code", buf, fname);

   () = fclose(fp);
}

private define friendly_remove(file)
{
   if (remove(file) != 0 && errno != ENOENT)
	message("Warning: unable to remove() file: %s",file);
}

define generate_module_init(module_defines_new_opaque_types)
{
   variable mname = SC.modname, iface = SC.interface;

   if (SC.cplusplus)
	slirp_emit("BEGIN_DECLS\n");

   slirp_emit("#define SLIRP_VERSION_STRING %s\n", SLIRP_VERSION_STRING);
   slirp_emit("#define SLIRP_VERSION_NUMBER %d\n", SLIRP_VERSION);
   slirp_emit("SLANG_MODULE(%s);\n", mname);
   slirp_emit("int init_%s_module_ns(char *ns_name)\t%s\n{\n", mname, FOLD_OPEN);
   slirp_emit("   SLang_NameSpace_Type *ns = NULL;\n\n"+
	"   if (slang_abi_mismatch()) return -1;\n"+
	"   if (ns_name != NULL) {\n"+
	"	ns = SLns_create_namespace (ns_name);\n"+
	"       if (ns == NULL ||\n"+
	"          (slns = SLmalloc(strlen(ns_name)+1)) == NULL)\n"+
	"          return -1;\n"+
	"       strcpy(slns, ns_name);\n   }\n\n");

   if (SC.gendebug)
	slirp_emit("   slirp_debug_pause((char*)\"%s\");\n\n", mname);

   if  ((SC.have_refs & 0x1) && not ((SC.have_refs & 0x4)))
          slirp_emit("   (void) &ref_get_size; /* avoid compile warn if unused */\n");

   emit_inlines(SC.interface.inlines_init);
   slirp_emit("\n");

   if (SC.num_reserved_types)
	slirp_emit("   if (allocate_reserved_opaque_types() == -1) return -1;\n",
	      								mname);

   if (module_defines_new_opaque_types)
	slirp_emit("   if (allocate_%s_opaque_types() == -1) return -1;\n\n",mname);

   slirp_emit("#ifdef HAVE_OPAQUE_IVARS\n");
   slirp_emit("   if (-1 == set_opaque_ivar_types(%s_Opaque_IVars) ||\n"+
        "       -1 == SLns_add_intrin_var_table(ns,%s_Opaque_IVars,NULL))\n",
								mname, mname);
   slirp_emit("	return -1;\n");
   slirp_emit("#endif\n\n");

   slirp_emit("   if (");

   if (length(iface.int_consts))
	slirp_emit(" -1 == SLns_add_iconstant_table(ns,%s_IConsts,NULL) ||\n",mname);

   if (length(iface.long_consts))
	slirp_emit(" -1 == SLns_add_lconstant_table(ns,%s_LConsts,NULL) ||\n",mname);

   if (length(iface.double_consts))
	slirp_emit("\t-1 == SLns_add_dconstant_table(ns,%s_DConsts,NULL) ||\n",mname);

   if (length(iface.string_consts) || length(iface.intrin_vars) ) {
	slirp_emit("\t-1 == SLns_add_intrin_var_table(ns,%s_IVars,NULL)  ||\n",mname);
	if (iface.intrin_arrays != NULL)
	   slirp_emit("\t-1 == make_intrinsic_arrays(ns)	||\n",mname);
   }

   slirp_emit("\t-1 == SLns_add_intrin_fun_table (ns,%s_Funcs,(char*)\"__%s__\"))\n"+
					"\treturn -1;\n", mname, mname);

   invoke_callbacks(POST_INIT_CB);

   slirp_emit("\n   return 0;\n} %s\n", FOLD_CLOSE);
   if (SC.cplusplus)
	slirp_emit("END_DECLS\n");
}

define generate_opaque_code()
{
   !if (length(SC.opaque_types)) return 0;

   variable non_reserved = SC.opaque_types[ [SC.num_reserved_types:]];
   variable external = struct_map(Integer_Type, non_reserved, "external");
   variable internal = non_reserved[ where (not(external)) ];
   external = non_reserved[ where(external) ];
   variable tmap, num_new_opaques = length(internal);


   if (length(external)) {
	% These declarations permit compilation of one module when 
	% generated against the typemaps of another module.
	foreach(external) {
		tmap = ();
		slirp_emit("extern SLtype %s;\n",tmap.typeid);
	}
	slirp_emit("#define SLIRP_EXTERN	extern\n");
   }
   else
	slirp_emit("#define SLIRP_EXTERN\n");

   inject_file("opaques.c");

   if (length(accepts_null_args))
	inject_file("nullargs.c");

   variable allocations = EMPTY;
   foreach (internal) {

	variable ot = (), sltype = ot.sltype;

	% Instantiate each new/unique type that will be added by this module
	slirp_emit("\nSLtype %s_Type = 0;", sltype);

 	variable parent = ot.parent;
	if (parent == NULL)
	   parent = 0;
	else
	   parent = parent.typeid;

	allocations = strcat(allocations, sprintf(
		"\n   %s_Type = allocate_opaque((char*)\"%s\","+
		"\n\t\t(FINALIZER) %S, \n\t\t(INITIALIZER) %S,\n"+
		"\t\t(SLtype) %S, %s, %s);"+
		"\n   if (%s_Type == SLANG_UNDEFINED_TYPE) return -1;\n",
		sltype, sltype + "_Type", ot.finalizer, ot.initializer,
		parent, ot.sget, ot.sput, sltype));
   }

   if (length(SC.classes))
	inject_file("c++.c");

   if (num_new_opaques) {

	allocations = strcat(

		sprintf("   if (%s_Type) return 0;\n\n", internal[0].sltype),

		sprintf("   Slirp_Opaque_Types = (Slirp_Type**) "+
			"SLrealloc((char*)Slirp_Opaque_Types,\n\t\t(1 + "+
			"Slirp_Num_Opaque_Types + %d) * sizeof(Slirp_Type*));"+
			"\n\n", num_new_opaques),

		"   if (Slirp_Opaque_Types == NULL) return -1;\n",

		allocations);

	slirp_emit("\n\nstatic int allocate_%s_opaque_types(void) %s\n",
						SC.modname, FOLD_OPEN);
	slirp_emit("{\n%s\n   return 0;\n} %s\n",allocations, FOLD_CLOSE);
   }

   return num_new_opaques;
}
% }}}

private define emit_assoc_array(array, format) % {{{
{
   foreach(array) using ("keys") {
	variable elem = ();
	slirp_emit(format, elem);
   }
} % }}}

private define register_hdr(h) % {{{
{
   if (h == DEVNULL) return;

   !if (length( where (SC.input_headers == h)))
	SC.input_headers = [ SC.input_headers, h];
} % }}}

private define generate_makefile() % {{{
{
   variable default_fname = "Makefile", fname = default_fname;
   foreach( [default_fname, "makefile" ]) {
	variable existing = ();
	if (stat_file(existing) != NULL) {
	   fname = SC.modname + ".mf";
	   break;
	}
   }

   SC.outfp = fopen(fname, "w+");
   if (SC.outfp == NULL) {
	dprintf("Error: could not generate makefile: %s",fname);
	return;
   }

   dprintf("\nStarter make file generated to '%s'", fname);
   if (fname == default_fname)
	fname = EMPTY;
   else {
	dprintf("(because %s already exists. If this is unexpected)",existing);
	dprintf("(then delete %s and regenerate it with SLIRP)",existing);
	fname = " -f " + fname;
   }
   dprintf("Type 'make%s' to build module, and note that:", fname);
   dprintf("   - you may need to edit the make file before linking");
   dprintf("   - you may want to adjust the default debug/optimization flags");

   slirp_emit("# This file was auto-generated by SLIRP version %S, ",SC.version);
   slirp_emit("using the command:\n#\n");
   %slirp_emit("#     %S ", SC.process_name);
   slirp_emit("#     ", SC.process_name);

   foreach (__argv) {
	variable  arg = ();
	slirp_emit("'%S' ",arg);
   }
   slirp_emit("\n\n\n");

   slirp_emit("MODNAME = %s\n", SC.modname);
   slirp_emit("SELF = %s\n",fname);
   slirp_emit("GLUE_FILE_EXT = %s\n", default_file_extension());
   if (SC.cplusplus) 
	slirp_emit("LINK = $(CXX_LINK)\n");
   else
	slirp_emit("LINK = $(C_LINK)\n");

   slirp_emit("SL_SCRIPTS = ");
   % If the build directory contains a <modname>.sl script then we
   % assume it should be installed along with <modname>-module.so
   if (stat_file(SC.genstem + ".sl") != NULL)
	slirp_emit(SC.genstem + ".sl");
   slirp_emit("\n");

   slirp_emit("LFLAGS = ");
   % Slang-1 array_map doesn't like empty arrays, so we test for that
   if (length(SC.lib_paths))
	array_map(Void_Type, &slirp_emit, " -L%s", SC.lib_paths);
   if (length(SC.lib_names))
	array_map(Void_Type, &slirp_emit, " -l%s", SC.lib_names);

   foreach(SC.ldflags) {
	variable f = ();
	if (is_object_code(f))
	   SC.obj_code = [ SC.obj_code, f ];
	else
	   slirp_emit(" %s", f);
   }

   if (SC.fortran_wrapped)
	slirp_emit("\nMISC_LIBS = $(FCLIBS)");

   slirp_emit("\nIFLAGS = ");
   emit_assoc_array(SC.input_paths, " -I%s");

   slirp_emit("\nSRC = ");
   array_map(Void_Type, &slirp_emit, " %s", SC.input_files);

   slirp_emit("\n\nOBJ = %s_glue.o ", SC.modname);
   slirp_emit(strjoin (SC.obj_code, " "));
   slirp_emit("\n");

   %slirp_emit("SLIRP = %s -n %s -m %s", SC.process_name, __argv[0], SC.modname);
   slirp_emit("SLIRP = slsh -n %s -m %s", __argv[0], SC.modname);

   if (SC.openmp)
	slirp_emit(" -openmp\nOMPFLAGS = %s", SC.openmp_flags);
   else if (SC.vectorize)
	slirp_emit(" -vec");

   slirp_emit("\nSTUBS = ");
   if (SC.genstubs)
	slirp_emit(SC.stubs_fname + "\n");
   slirp_emit("\n\n");

   inject_file("unix.mf");
   close_file(SC.outfp);
}
% }}}

% Default Language handlers {{{
autoload("f2cprotos", "slirpf2c");
private define c(file) { register_hdr(file); file; }
private define cplusplus(file) { register_hdr(file); SC.cplusplus = 1; file; }
private define fortran(file) { return f2cprotos(file); }
_handlers[".h"]	 = &c;			% these are case-insensitive
_handlers[".hh"] = &cplusplus;
_handlers[".f"]  = &fortran;
_handlers[".for"]  = &fortran;
% }}}

provide("slirputils");
