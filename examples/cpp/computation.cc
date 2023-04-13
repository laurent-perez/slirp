
#include "computation.hh"

void Computation::mult(double *array, unsigned long size)
{
   // Iterate over array, multiplying each element by result of computation
   if (array == NULL) return;
   double result = compute();
   while (size--) array[size] = array[size]*result;
}

void invoke_zero_arg_methods(Computation *c)
{
   cout << "Computation: 	";
   if (c) {
      cout <<  c->name() << endl;
      cout << "Id:		" << c->id() << endl;
      cout << "Result:		" << c->compute();
   }
   else
      cout <<  " NULL";

   cout << endl;
}

void change(double & d, double newval) { d = newval; }

void output(short s)	{ cout << "short value = "  << s << endl; }
void output(int i)	{ cout << "int value = "    << i << endl; }
void output(double d)	{ cout << "double value = " << d << endl; }
bool istrue(bool b)	{ return (b != 0); }

void output(double *arr, unsigned long size)
{
   if (arr == NULL) { cout << "NULL" << endl; return; };
   for (unsigned long i=0; i < size; i++)
	cout << "double_value[" << i << "] = " << arr[i] << endl;
}

void output(unsigned long size, double *d1, double *d2)
{
   if (d1 == NULL) { cout << "NULL" << endl; return; };
   for (unsigned long i=0; i < size; i++) {
	cout << "d1[" << i << "] = " << d1[i] << endl;
	if (d2)
	   cout << "d2[" << i << "] = " << d2[i] << endl;
   }
}

void output(int *arr, unsigned long size)
{
   if (arr == NULL) { cout << "NULL" << endl; return; };
   for (unsigned long i=0; i < size; i++)
	cout << "int_value[" << i << "] = " << arr[i] << endl;
}

double* make_array_d(unsigned long size)
{
   double *arr = new double[size];
   while (size--)
      arr[size] = size;
   return arr;
}

void validate_state(Computation *c, State s)
{
   if (c->_state != s) {
	cerr << "Warning: " << c->name() << " computation state != ";
	cerr << s << endl; 
   }
}
