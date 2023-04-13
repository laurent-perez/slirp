import("stubs");
() = evalfile("../examples-common");

if (func1("dummy") != 0) print("func1() failed");
if (func2([1.1, 2.2, 3.3], 3) != 0) print("func2() failed");
if (func3() != NULL) print("func3() failed");

print("MACRO1 = %S",MACRO1);
print("MACRO2 = %S",MACRO2);
print("MACRO3 = %S",MACRO3);

check_ignored("MACRO4");
check_ignored("_THIS_AND_THAT_3");
check_ignored("func5");
check_ignored("func7");
check_ignored("func8");
check_ignored("func10");
check_ignored("func11");
check_ignored("func12");


!if (func4([1.1, 2.2, 3.3], 3)) print("func4() failed");
if (func6() != 0) print("func6() failed");
if (func9() != 0) print("func9() failed");
if (MULTIPLE_DEF1 != 100) print("MULTIPLE_DEF1 was wrapped incorrectly");
if (MULTIPLE_DEF2 != 101) print("MULTIPLE_DEF2 was wrapped incorrectly");
if (STRING_MACRO  != "dummy") print("STRING_MACRO was wrapped incorrectly");
if (REAL_MACRO  != 99.99) print("REAL_MACRO was wrapped incorrectly");

if (Vec_dot(Vec_new(), Vec_new()) != 0.0) print("Vec_dot () failed_");

if (typeof(real_func1()) != Double_Type)
   print("RealType1 not properly wrapped as double (#ifdef problem?)");

if (typeof(real_func2()) != Float_Type)
   print("RealType2 not properly wrapped as float (#ifdef problem?)");

#ifexists real_func3
   if (typeof(real_func3()) != Double_Type)
	print("real_func3 not wrapped properly as double (#ifdef problem?)");
#else
   print("real_func3 undefined: multi-line macro parsing problem?");
#endif

#ifexists real_func4
   print("real_func4 should not be wrapped: multi-line macro parsing problem?");
#endif

#ifexists DrawText
   print("DrawText should NOT be wrapped: check prep_get_token()");
#endif

#ifnexists DrawText2
   print("DrawText2 should HAVE been wrapped: check prep_get_token()");
#endif
