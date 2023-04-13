#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

static struct timeval prev,curr;

void tic() {
  gettimeofday(&prev, 0);
}

double toc() {
  double t;
  gettimeofday(&curr, 0);
  t = curr.tv_sec - prev.tv_sec;
  t += (curr.tv_usec - prev.tv_usec) / 1e6;
  return t;
}

typedef struct _s { unsigned int stride; } Stride;

int main(int argc, char **argv)
{
   register unsigned int rstride = 1;
   Stride s, *s2 = (Stride*)malloc( sizeof(Stride) ) ;
   unsigned int i, stride = 1, NELEM = 500000, whichtest=1, *arr, *arrp;

   /* This program utilizes various ways of indexing over arrays, */
   /* in an attempt to see if such matters for modern C compilers */

   argc--;
   while (argc) {
	if (!strncmp(argv[argc],"-t=",3))
	   whichtest = atol(argv[argc]+3);
	else if (!strncmp(argv[argc],"-s=",3))
	   NELEM = atol(argv[argc]+3);
	argc--;
   }

   printf("NELEM = %d\n",NELEM);
   if ( (arr = (unsigned int*) malloc (NELEM * sizeof(unsigned int))) == NULL) {
	printf("Not enough memory to allocate %d unsigned int elements\n",NELEM);
	exit(1);
   }

   memset(arr, 0, NELEM * sizeof(unsigned int));
   arrp = arr;

   switch(whichtest) {
      
   case 1: 
	tic();
	for (i=0; i < NELEM; i++)
	    arr[i] = i;
	printf("arr[i] = i \t\t\t: %f sec (arr[-1]=%d)\n",toc(),arr[NELEM-1]);
	break;

   case 2:	

	tic();
	for (i=0; i < NELEM; i++)
	   *arrp++ = i;
	printf("*arrp++ = i \t\t\t: %f sec (arr[-1]=%d)\n",toc(),arr[NELEM-1]);
	break;

   case 3:

	s.stride = stride;
	tic();
	for (i=0; i < NELEM; i++)
	   { *arrp = i; arrp += s.stride; }
	printf("*arrp= i; arrp += s.stride\t: %f sec (arr[-1]=%d)\n",
	      						toc(),arr[NELEM-1]);
	break;

   case 4:

	s2->stride = stride;
	tic();
	for (i=0; i < NELEM; i++)
	   { *arrp = i; arrp += s2->stride; }
	printf("*arrp= i; arrp += s->stride\t: %f sec (arr[-1]=%d)\n",
	      						toc(),arr[NELEM-1]);
	break;

   case 5:

	tic();
	for (i=0; i < NELEM; i++)
	   { *arrp = i; arrp += stride; }
	printf("*arrp= i; arrp += stride\t: %f sec (arr[-1]=%d)\n",
	      						toc(),arr[NELEM-1]);
	break;

   case 6:

	tic();
	for (i=0; i < NELEM; i++)
	   { *arrp = i; arrp += rstride; }
	printf("*arrp= i; arrp += stride (reg)\t: %f sec (arr[-1]=%d)\n",
	      						toc(),arr[NELEM-1]);
	break;

   case 7:
	tic();
	for (i=0; i < NELEM; i++)
	    arr[i*stride] = i;
	printf("arr[i*stride] = i\t\t: %f sec (arr[-1]=%d)\n",
	      						toc(),arr[NELEM-1]);
  	break;

   default:
	break;
   }

   return 0;
}
