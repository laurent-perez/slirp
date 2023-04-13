#include "compat.hh"

inline void stringlist_print(int cnt, string* slist)
{
   if ((cnt > 0) && slist)
	for (int rr =0; rr < cnt; rr++)
	   cout << rr << ")  ["<< slist[rr] << "]" << endl;
}

inline double sum2d(double x[2][2])
{
   return x[0][0] + x[0][1] + x[1][0] + x[1][1];
}

// Dummies included to exercise parts of the generated code
inline void dummy(int* i) { (void) i; }
inline void dummy(float* f) { (void) f; }

class ClassClass {
public:
   ClassClass() { value = 999.99; }
   ClassClass(float v) { value = v; }
   float get_value() { return value; }
   int i[32];
private:
   float value;
};

// Note that if classes defined with struct keyword appear BEFORE
// any only-legal-in-C++ token then it will be interpreted as a C
// struct (and handled accordingly), rather than as a C++ class.
// In such cases the -c++ switch can be used to force C++ semantics.
struct StructClass {
   StructClass() { value = 999.99; }
   StructClass(float v) { value = v; }
   float value;
};

typedef struct StructClass StructClassAlias;

float sca_get_value(StructClassAlias& sca) { return sca.value; }

struct Forward;		// verify that forward decls are parsed properly

typedef ClassClass ClassClassAlias;

float cca_get_value(ClassClassAlias& cca) { return cca.get_value(); }

// Exercise parser on multiple variables defined on single line
int misc_x,misc_y,		misc_z;

// Now on 1D-array- and string-valued global variables
float farr[32];
const char *string_var = "In the year 1492, Columbus sailed the ocean blue";
const char *string_arr[] = {
   "string1",
   "string2",
   "string3",
};

// Accept inline keyword either as first token or after return type
inline void set_farr(int idx, int value) { farr[idx] = value; }
float inline get_farr(int idx) { return farr[idx]; }
