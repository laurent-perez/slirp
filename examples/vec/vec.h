#ifndef __cplusplus
extern double 	sin		(double x);
extern double 	sqrt		(double x);
extern double	cos		(double x);
extern double	hypot		(double x, double y);
extern size_t	strlen		(const char *s);
#else
#include <math.h>
#endif


#ifdef __cplusplus
extern "C" {
#endif

extern char*	tag		(unsigned int id);
extern double	mult		(double x, double y);
extern void 	vmult		(double *x, int len, double *y, double *result);
extern void	stupid		(int, int, int, int, int, float,
					int, double, char*, int, float, int);

/* These funcs exercise ND array vectorization */
extern void 	add2d(double x[2][2], double y[2][2]);
extern void 	add2dp(double **x, double **y);
extern double** make2dp(double seed);
extern void     print_and_free_2dp(double**);
extern void	sub1_2d(int *matrix, int numrows, int numcols);

/* These funcs exercise opaque vectorization */
typedef struct { int n; } Number;
extern Number*	Number_new(int n);
extern void 	Number_set(Number *n, int i);
extern int	Number_get(Number *n);

#ifdef __cplusplus
}
#endif
