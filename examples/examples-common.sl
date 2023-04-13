define dump_stack(func)
{
   variable x = _stkdepth();
   if (x) {
	() = fprintf(stderr,"Stack Contents After Calling <%S>:\n",func);
	_print_stack();
	_pop_n(_stkdepth());
	() = fprintf(stderr,"\n");
   }

   () = fflush(stderr);
}

private define printarr(arr)
{
   variable i, j, dims, dimensionality;
   (dims, dimensionality, ) = array_info(arr);

   switch(dimensionality)
	{ case 1: dims = [dims,1]; reshape(arr, dims); }
	{ case 2: }
	{ () = printf("%S\n",arr);  return; }

   for (i=0; i < dims[0]; i++) {
	for(j=0; j < dims[1]; j++)
	   () = printf("%S ",arr[i,j]);
	() = printf("\n");
   }

   () = fflush(stdout);

   if (dimensionality == 1)
      reshape(arr,[dims[0]]);
}

define print()
{
   if (_NARGS)  {
	variable extra_args = __pop_args(_NARGS-1);
	variable arg1 = ();
	switch(typeof(arg1))
	{ case String_Type: () = printf(arg1, __push_args(extra_args)); }
	{ case Array_Type:  printarr(arg1); return; }
	{ () = printf("%S", arg1); }
   }
   () = printf("\n");
   () = fflush(stdout);
}

define _try()
{
   variable args = __pop_args(_NARGS - 1);
   variable func = ();

#ifeval _slang_version < 20000
   ERROR_BLOCK { _clear_error(); print; }
   @func(__push_args(args));
#else
   variable excep;
   try (excep) { @func(__push_args(args)); }
   catch AnyError: { vmessage(excep.message); print; }
#endif

   dump_stack(func);
}

define check_ignored(thing)
{
   if (is_defined(thing))
	print("Error: %s NOT properly ignored by code generator",thing);
}

define array_neqs(arr1, arr2)
{
   return  length ( where (arr1 != arr2) );
}
