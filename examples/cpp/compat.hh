
#ifndef __cplusplus_ansi__
  #if ! ( __cplusplus < 199711L )
  #define __cplusplus_ansi__
  #elif defined(__GNUG__) && (__GNUG__ > 2)
  #define __cplusplus_ansi__
  #endif
#endif

#ifdef __cplusplus_ansi__ 

#include <iostream>
#include <climits>
#include <cfloat>
#include <string>

using namespace std;

#else

#include <iostream.h>
#include <limits.h>
#include <float.h>
#include <stddef.h>
#define bool unsigned char
typedef char* string;

#endif

