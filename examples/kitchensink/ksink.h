#include <stddef.h>
#include <unistd.h>

#define		KSINK_FOO	(1.11111111)
#define		KSINK_BAR	193
#define		KSINK_BAZ	"baz"
#define		DECL(t)		extern t	/* tests macro handling */

typedef struct { char *name; double value; } KDatum;
typedef struct { double p[3]; unsigned long id; } KParams;
struct KDummy { int i; float f; };

typedef void   		   		*VoidStar;

typedef enum { KSINK_GOOD=0, KSINK_BAD, KSINK_UGLY, KSINK_HORRIFIC } KErrorCode;

typedef	int*/* COMMENT EMBEDDED HERE FOR TESTING PURPOSES*/KIntP;

DECL(long)		ksink_sum		(long augend, long addend);
DECL(double) 		ksink_mult		(double op1, double op2);

DECL(void)		ksink_mult2		(double op1,
						double op2,
						double *result);

DECL(KDatum)		*ksink_datum_new	(char *name, double value);
/* Next 2 protos exercise the default/automatic struct --> opaque mapping */
DECL(KParams*)		ksink_params_new   (unsigned long id, double params[3]);
DECL(const char*)	ksink_params_str	(KParams *p);
DECL(void)		datum_copy		(KDatum *in,KDatum**copy);
DECL(void) 		ksink_datum_destroy	(KDatum *datum);
DECL(void) 		ksink_set_ref_i		(int *i);

DECL(KIntP)		ksink_make_array_i	(void);
DECL(void)		ksink_print_array_i	(long nelems, KIntP
	array);
/* Next prototype uses [] array syntax, to better exercise parser  */
DECL(void)		ksink_print_array_f	(long nelems, float array [  ]);
DECL(char**)		ksink_make_array_s	(void);
/* Peculiar spacing used in next prototype, again to better exercise parser */
DECL(void)		ksink_print_array_s	( 	 long	nelems  ,				                			char **array);
DECL(char**)		ksink_make_array_nts	(void);
DECL(void)		ksink_print_array_nts	(char **array);

DECL(KDatum**)	ksink_make_array_datum	(void);
DECL(void)		ksink_print_array_datum	(long nelems,KDatum **array);
DECL(void)		ksink_print_error	(KErrorCode err);

DECL(void)		ksink_ignorable_func	(void);
DECL(void)		dummy1			(void);
DECL(void)		dummy2			(void);
DECL(void)		dummy3			(void);

DECL(VoidStar)	ksink_voidstar_echo	(VoidStar input);
DECL(void)		ksink_arg_dropper_1	(unsigned long int ul);
DECL(void)		ksink_arg_dropper_2	(unsigned long int ul /*comment1*/,double d/*comment2*/);
DECL(void)		ksink_toggle_error_hook	(void);
DECL(ptrdiff_t)	ksink_subtract_ptrs	(void *p1, void *p2);
DECL(size_t)		ksink_sizeof_int		(void);
DECL(void)		ksink_swap_double	(double *i, double *j);
DECL(void)		ksink_close		(int fd);
DECL(int) 		ksink_ftell		(FILE *fp);
DECL(FILE*)		ksink_fopen		(char *name, char *mode,
						unsigned long  *size,
						unsigned short *nlinks);

typedef int KInt;
extern	void		ksink_int_accepter	(KInt i,KInt j,KInt k);

extern	int		ksink_equals_array_d(double*DA1,long nelems,double*DA2);
extern	int		ksink_copy_string	(char *src,char *dest,long len);

extern  struct KDummy*	ksink_dummy_new(void);
extern  int		ksink_dummy_ok(struct KDummy* s);

#define			KSINK_IGNORABLE_MACRO	55
extern	int		KSINK_IGNORABLE_INT;
extern	int		KSINK_IGNORABLE_INT2;
#define			KSINK_IGNORABLE_MACRO2	110
typedef struct _ks_struct ks_struct;	/* ignore: this exercises type system */
