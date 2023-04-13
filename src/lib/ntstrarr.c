static void push_null_term_str_array(char **arr, char *funcname, int free_arr)
{
   int i;
   char **copy = arr;
   SLang_Array_Type *sarr;

   while (*copy++) ;
   i = copy - arr - 1;

   sarr = SLang_create_array (SLANG_STRING_TYPE, 0, NULL, &i, 1);
   if (sarr != NULL) {
	   
	char **slstr = (char**)sarr->data;
	while (i--)
	   if ( NULL == (slstr[i] = SLang_create_slstring (arr[i]))) {
		SLang_free_array(sarr);
		sarr = NULL;
		break;
	   }

   }
   if (sarr == NULL) {
	if (funcname == NULL) funcname = "unknown function";
	SLang_verror(SL_INTRINSIC_ERROR, "creating string array in %s", funcname);
   }
   else
	SLang_push_array(sarr, 1);

   if (free_arr) {
	i = copy - arr;
	while (i--)
	   SLfree(arr[i]);
	SLfree((char*)arr);
   }
}
