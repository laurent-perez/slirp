/* This file exercises SLIRPs -stubs and preprocessing capabilities */

#define MACRO1 1
#define MACRO2 1

int func1(char* name);

#undef MACRO2
#define MACRO2 2
#define MACRO3 3

int func2(double *double_pointer, unsigned int len);

#ifdef MACRO3
const char* func3(void);
#endif

#undef MACRO3
#ifdef MACRO3
float func3(void);
#endif

int func4(double arr[3], unsigned int len); 

#define MACRO3 3
#define MACRO4 4

#if MACRO2 > 1
   #ifndef MACRO1
	void func5(void);
	#undef MACRO3
   #elif (5!=3 && !defined(MACRO5)) 
	int func6(void);
   #endif
#endif

#if 1
#if MACRO2 > 1 && !defined(MACRO4)
   void func7(void);
   #undef MACRO3
#elif MACRO3 < 3
	void func8(void);
#elif 2+3<=5
	int func9(void);
#endif
#endif

#if /* pre comment */1/* post comment */
  #if 0
	bad stuff!
	void func10(void);
  #endif

  #undef MACRO4

  /* Some C preprocessors honor the use of the ? : and , operators in this
   * context; SLIRP does not, and evaluates the entire expression to 0 */
  #if MACRO1 ? foo : bar
	void func11(void);
  #endif
  #if 0
	#if 1,0
	   void func12(void);
	#endif
  #endif
#endif

#define MAKE_DOUBLE 1
#if defined(MAKE_DOUBLE) || defined(DOUBLE)
typedef double RealType1;
#else
typedef float RealType1;
#endif

RealType1 real_func1(void);

#undef MAKE_DOUBLE
#ifdef MAKE_DOUBLE
typedef double RealType2;
#else
typedef float RealType2;
#endif

RealType2 real_func2(void);

/* This tests parsing of continued lines */
#if (defined(MACRO3) && !defined(MAKE_DOUBLE)) ||  \
(defined(MACRO1) && defined(MACRO2))
RealType1 real_func3(void);
#endif

typedef float blah;

#if (!defined(MAKE_DOUBLE) && \
		!defined(MACRO3)) ||  \
	(!defined(MACRO2) && \
					!defined(MACRO1))
RealType1 real_func4(void);	/* this should not be wrapped */
#endif

#if defined(NO_SUCH_THING)
	void DrawText(char *text,
#else
	void DrawText2(char *text,
#endif
		const float x, const float y);
