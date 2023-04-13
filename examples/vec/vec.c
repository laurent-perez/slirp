#include <stdlib.h>
#include <stdio.h>
#include "vec.h"

#define TAG_SIZE 13
char* tag(unsigned int id)
{
   char *t = (char*) malloc(TAG_SIZE);
   if (t == NULL) return "";
   snprintf(t, TAG_SIZE, "tag-%u", id);
   return t;
}

double mult(double x, double y)	{ return x * y; }

void vmult(double *x, int len, double *y, double *result)
{
   if (x == NULL || y == NULL || result == NULL) return;
   while (len--)
      result[len] = x[len] * y[len];
}

void stupid(int a, int b, int c, int d, int e, float f, int g,
				double h, char* i, int j, float k, int l)
{
   double value = a + b + c + d + e + f + g + h + j + k + l;
   if (i != NULL) value = value + *i;
}

void add2d(double x[2][2], double y[2][2])
{
  x[0][0] += y[0][0];
  x[0][1] += y[0][1];
  x[1][0] += y[1][0];
  x[1][1] += y[1][1];
}

void add2dp(double **x, double **y)	/* implied 2x2 array dims */
{
   add2d( (double (*)[2]) x, (double (*)[2]) y);
}

double** make2dp(double seed)
{
   double (*d)[2]  = (double (*)[2]) malloc(4 * sizeof(double));
   d[0][0] = d[0][1] = d[1][0] = d[1][1] = 1.0 * seed;
   return (double**)d;
}

void print_and_free_2dp(double **x)
{
   int i, j;
   double *d = (double*) x;
   for (i=0; i<2; i++)
	for (j=0; j<2; j++)
	   printf("d[%d][%d] = %f\n", i, j, d[i*2+j]);
   fflush(stdout);
   free(x);
}

void sub1_2d(int *matrix, int numrows, int numcols)
{
   int c;
   while (numrows--)
      for (c=0; c < numcols; c++)
	*matrix++ -= 1;
}

Number* Number_new(int value)
{
   Number *num = (Number*) malloc(sizeof(Number));
   num->n = value;
   return num;
}

void Number_set(Number *num, int value)
{
   num->n = value;
}

int Number_get(Number *num)
{
   return num->n;
}
