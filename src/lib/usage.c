static void Slirp_usage(int i, int last, int flags) /* {{{ */
{
   char *indent;
   int npop = SLstack_depth();
   if (npop > SLang_Num_Function_Args) npop = SLang_Num_Function_Args;
   SLdo_pop_n(npop);
   if (last == i)
	indent = (char*)"Usage:  ";
   else {
	indent = (char*)"\t";
	SLang_verror(SL_USAGE_ERROR, (char*)"Usage: one of");
   }
   do
	SLang_verror(SL_USAGE_ERROR, (char*)"%s%s", indent, usage_strings[i++]);
   while (i < last);
   if (flags & 0x2)
	SLang_verror(SL_USAGE_ERROR,
		(char*)"\tThis function has been vectorized and parallelized.");
   else if (flags & 0x1)
	SLang_verror(SL_USAGE_ERROR, (char*)"\tThis function has been vectorized.");
} /* }}} */
