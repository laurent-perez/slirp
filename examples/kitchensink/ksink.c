#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "slang.h"
#include "ksink.h"
#include "../common.h"

int KSINK_IGNORABLE_INT = 999;
int KSINK_IGNORABLE_INT2 = 1998;

long ksink_sum(long augend, long addend) /* {{{ */
{ 
   return augend + addend;
} /* }}} */

double ksink_mult(double op1, double op2) /* {{{ */
{ 
   return op1 * op2;
} /* }}} */

void ksink_mult2(double op1, double op2, double *result) /* {{{ */
{ 
   *result = op1 * op2;
} /* }}} */

void ksink_datum_destroy(KDatum *datum) /* {{{ */
{
   if (datum) {
	free(datum->name);
	free(datum);
   }
} /* }}} */

KDatum* ksink_datum_new(char *name, double value) /* {{{ */
{
   KDatum *datum = (KDatum*)malloc(sizeof(KDatum));
   if (datum) {
	if (!name) name = "unknown";
	datum->name = (char*) malloc (strlen(name) + 1);
	strcpy(datum->name,name);
	datum->value = value;
   }
   return datum;
} /* }}} */

void datum_copy(KDatum *in, KDatum * * out) /* {{{ */
{
   if (in == NULL)
	*out = NULL;
   else
	*out = ksink_datum_new(in->name, in->value);
} /* }}} */

KIntP ksink_make_array_i (void) /* {{{ */
{
   static int array[] = {10,259,341};
   return array;
} /* }}} */

#define CHECK_NULL(a)  if ((a) == NULL) \
	{ fprintf(stderr,"Premature exit, after attempted NULL pointer ref\n");\
	  return; }

void ksink_print_array_i (long nelems, KIntP array) /* {{{ */
{
   int i;
   CHECK_NULL(array);
   for (i=0; i < nelems; i++)
	printf("ksink_print_array_i: element %d = <%d>\n",i+1,array[i]);
   printf("\n");
   fflush(stdout);
} /* }}} */

void ksink_print_array_f (long nelems, float *array) /* {{{ */
{
   int i;
   CHECK_NULL(array);
   for (i=0; i < nelems; i++)
	printf("ksink_print_array_f: element %d = <%f>\n",i+1, array[i]);
   printf("\n");
   fflush(stdout);
} /* }}} */

KDatum** ksink_make_array_datum (void) /* {{{ */
{
   static KDatum* array[3];
   array[0] = ksink_datum_new("one",1);
   array[1] = ksink_datum_new("zero",0);
   array[2] = ksink_datum_new("niner",9);
   return array;
} /* }}} */

void ksink_print_array_datum (long nelems, KDatum **array) /* {{{ */
{
   int i;
   CHECK_NULL(array);
   for (i=0; i < nelems; i++) {
	CHECK_NULL(array[i]);
	printf("ksink_print_array_datum: elem %d (name,value) = (%s,%f)\n",
					i,array[i]->name, array[i]->value);
   }
   fflush(stdout);
} /* }}} */

char** ksink_make_array_s (void) /* {{{ */
{
   static char *array[] = {"Winnie","The","Pooh"};
   return array;
} /* }}} */

void ksink_print_array_s (long nelems, char **array) /* {{{ */
{
   int i;
   for (i=0; i < nelems; i++)
	printf("ksink_print_array_s: element %d = <%s>\n",i+1,array[i]);
   printf("\n");
   fflush(stdout);
} /* }}} */

char** ksink_make_array_nts (void) /* {{{ */
{
   static char *array[] = {"This", "Is", "A", "Null", "Terminated",
					"Array", "Of", "Strings", NULL};
   return array;
} /* }}} */

void ksink_print_array_nts (char **array) /* {{{ */
{
   int i = 0;
   while(*array)
	printf("ksink_print_array_nts: element %d = <%s>\n",++i,*array++);
   printf("\n");
   fflush(stdout);
} /* }}} */

void ksink_set_ref_i (int *i) /* {{{ */
{
   *i = -9191;
} /* }}} */

static const char* error_strings[] = {"good","bad","ugly","horrible"};

void ksink_print_error(KErrorCode err) /* {{{ */
{
   if (err == KSINK_GOOD)
	printf("ksink_print_error: Success!\n");
   else
	printf("ksink_print_error: Error: message = <%s>\n",error_strings[err]);
   fflush(stdout);
} /* }}} */

void ksink_ignorable_func(void) /* {{{ */
{
   printf("ksink_ignorable_func: uh-oh, should NOT be callable from S-Lang!\n");
   fflush(stdout);
} /* }}} */

void* ksink_voidstar_echo(void *input) /* {{{ */
{
   return input;
} /* }}} */

static void recover_unrecoverable_error_hook(char *error_msg) /* {{{ */
{
   fputs(error_msg,stderr);	/* support function, which lets scripts */
   fputs("\r\n", stderr);	/* catch "unrecoverable" errors within  */
   fflush (stderr);		/* an ERROR_BLOCK */
   SLang_restart(0);

   /* SLang_Error = SL_USAGE_ERROR; */
   SLang_set_error(SL_USAGE_ERROR);

} /* }}} */

void ksink_toggle_error_hook(void) /* {{{ */
{
   static void (*previous_error_hook)(char *);

   if (SLang_Error_Hook == recover_unrecoverable_error_hook)
	SLang_Error_Hook = previous_error_hook;
   else {
	previous_error_hook = SLang_Error_Hook;
	SLang_Error_Hook = recover_unrecoverable_error_hook;
   }
} /* }}} */

ptrdiff_t ksink_subtract_ptrs(void *p1, void *p2) /* {{{ */
{
   return (char*)p2 - (char*)p1;
} /* }}} */

size_t ksink_sizeof_int(void) /* {{{ */
{
   return sizeof(int);
} /* }}} */

void ksink_int_accepter(KInt i, KInt j, KInt k) /* {{{ */
{
   printf("ksink_int_accepter:\n\ti=%d\n\tj=%d\n\tk=%d\n",i,j,k);
   fflush(stdout);
} /* }}} */

void ksink_swap_double(double *i, double *j) /* {{{ */
{
   double tmp;
   if (i == NULL || j == NULL) return;
   tmp = *i;
   *i = *j;
   *j = tmp;
} /* }}} */

int ksink_equals_array_d (double *a, long nelems, double *b) /* {{{ */
{
   while(nelems--)
	if (a[nelems] != b[nelems]) return 0;
   return 1;
} /* }}} */

int ksink_copy_string(char *src, char *dest, long len) /* {{{ */
{
   if (dest == NULL || len < 0) return 0;
   (void) strncpy(dest, src, len);
   dest[len] = 0;

   /* This function returns a value only to exercise the use of a #typedef
      w/in a #retmap(omit) to swallow the return vaule of a function */
   return (int) strlen(dest);
} /* }}} */

void ksink_arg_dropper_2(unsigned long ul, double d) /* {{{ */
{
   printf("ksink_arg_dropper_2:\n\tl=%ld\td=%3.3f\n",ul,d);
   fflush(stdout);
} /* }}} */

void ksink_arg_dropper_1(unsigned long ul) /* {{{ */
{
   printf("ksink_arg_dropper_1:\n\tl=%ld\n",ul);
   fflush(stdout);
} /* }}} */

void  ksink_close(int fd)	{ close(fd); }
int   ksink_ftell(FILE *fp)	{ return ftell(fp); }

FILE* ksink_fopen(char *name, char *mode, /* {{{ */
      		unsigned long *size,
		unsigned short *nlinks)
{
   struct stat buf;

   if (stat(name, &buf) != 0) { *size = 0; *nlinks = 0; return NULL; }

   *size = (unsigned long) buf.st_size;
   *nlinks = (unsigned short) buf.st_nlink;

   return fopen(name, mode);
} /* }}} */

KParams* ksink_params_new(unsigned long id, double params[3]) /* {{{ */
{
   KParams *par;
   
   if (NULL == (par = (KParams*) malloc(sizeof(KParams))))
	return NULL;

   par->p[0] = params[0];
   par->p[1] = params[1];
   par->p[2] = params[2];
   par->id = id;
   return par;
} /* }}} */

const char* ksink_params_str(KParams *pars)
{
   static char params_str[81];
   if (pars == NULL) return NULL;
   snprintf(params_str, 80, "id = %ld,   p1=%f   p2=%f   p3=%f",
			pars->id, pars->p[0], pars->p[1], pars->p[2]);
   return params_str;
}

extern  struct KDummy* ksink_dummy_new()
{
   struct KDummy *s = (struct KDummy* ) malloc( sizeof(struct KDummy));
   if (s != NULL) {
	s->i = 1;
	s->f = 99.99;
   }
   return s;
}

extern int ksink_dummy_ok(struct KDummy *s)
{
   if (s != NULL && s->i == 1 && s->f == 99.99)
	return 0;
   return 1;
}
