static float score_func(Overloaded_Func *o, char *have, int vectorized) /*{{{*/
{
   char h, w, *want = o->signature;
   float score = 0.0;

   while ((w = *want++)) {

	if (!(h = *have++)) break;
	if (w == h) { score += 2;  continue;}
	if (h == 'N' && (w == 'o' || w == 's')) { score += 0.5; continue; }
	if (w < '[') w += 32;			/* lowercase */

	switch (h) {

	   case 'f' : case 'd' :

		if (w == 'f' || w == 'd') { score++; continue; }
		return 0;

	   case 'm': case 'n': case 'p': case 'q':
	   case 'r': case 't': case 'u':

		/* Let S-Lang decide how non-matching array */
		/* types may be cast to and from each other */
		if (w == 'm' || w == 'n' || w == 'p' || w == 'q'
		      			 || w == 'r' || w == 't' || w == 'u')
		   score++;
		else if (vectorized)
		   score += 0.5;
		else
		   return 0;

		continue;

	   case 'b' : case 'B' : case 'h' : case 'H' :
	   case 'i' : case 'I' : case 'l' : case 'L' :
	      
		if (w == 'b' || w == 'h' || w == 'i' || w == 'l'
		      			 || w == 'f' || w == 'd') {
		   score++;
		   continue;
		}				/* fallthrough */

	   default:  return 0;
	}
   }
   return score;
} /* }}} */

#define MAX_OVERLOAD_ARGS  20

static char* arg_signature_of_currently_invoked_slang_func() /* {{{ */
{
   static char sig[MAX_OVERLOAD_ARGS+1];		/* Not thread safe */
   int arg = SLang_Num_Function_Args;

   if (SLang_Num_Function_Args > MAX_OVERLOAD_ARGS) {
   	SLang_verror(SL_INTRINSIC_ERROR, (char*)"Too many args for overloaded func");
	return (char*)"";
   }

   while (arg--) {
	switch(SLang_peek_at_stack_n(arg)) {
	case SLANG_CHAR_TYPE   : sig[arg] = 'b'; break;
	case SLANG_UCHAR_TYPE  : sig[arg] = 'B'; break;
	case SLANG_SHORT_TYPE  : sig[arg] = 'h'; break;
	case SLANG_USHORT_TYPE : sig[arg] = 'H'; break;
	case SLANG_INT_TYPE    : sig[arg] = 'i'; break;
	case SLANG_UINT_TYPE   : sig[arg] = 'I'; break;
	case SLANG_LONG_TYPE   : sig[arg] = 'l'; break;
	case SLANG_ULONG_TYPE  : sig[arg] = 'L'; break;
	case SLANG_FLOAT_TYPE  : sig[arg] = 'f'; break;
	case SLANG_DOUBLE_TYPE : sig[arg] = 'd'; break;
	case SLANG_STRING_TYPE : sig[arg] = 's'; break;
	case SLANG_NULL_TYPE   : sig[arg] = 'N'; break;  // for NULLable args
	case SLANG_ARRAY_TYPE  :
		switch ( SLang_peek_at_stack1_n(arg) ) {
			case SLANG_CHAR_TYPE   :
			case SLANG_UCHAR_TYPE  : sig[arg] = 'm'; break;
			case SLANG_DOUBLE_TYPE : sig[arg] = 'n'; break;
			case SLANG_FLOAT_TYPE  : sig[arg] = 'p'; break;
			case SLANG_SHORT_TYPE  : 
			case SLANG_USHORT_TYPE : sig[arg] = 'q'; break;
			case SLANG_INT_TYPE    :
			case SLANG_UINT_TYPE   : sig[arg] = 'r'; break;
			case SLANG_LONG_TYPE   :
			case SLANG_ULONG_TYPE  : sig[arg] = 't'; break;
			case SLANG_STRING_TYPE : sig[arg] = 'u'; break;
			default:		 sig[arg] = 'o'; break;
		}
		break;

	/* Prohibit passing refs to C++, by assigning unused/ref mnemonic */
	case SLANG_REF_TYPE    : sig[arg] = 'R'; break;
	default: sig[arg] = 'o';
	}
   }

   sig[SLang_Num_Function_Args] = 0;

  {	char c; int i, j;	/* reverse signature to match stack */
	for (i=0, j = SLang_Num_Function_Args - 1; i < j; i++, j--)
	   { c = sig[i]; sig[i] = sig[j]; sig[j] = c; }
  }

  return sig;
} /* }}} */

static void dispatch(int first, int stop, int usage_idx, int vectorized) /*{{{*/
{
   int nargs = SLang_Num_Function_Args;
   Overloaded_Func *curr = &Dispatch_Table[first];
   Overloaded_Func *last = &Dispatch_Table[stop];
   Overloaded_Func *best = NULL, *alternate = NULL;
   char *sig = arg_signature_of_currently_invoked_slang_func();
   float curr_score, best_score = 0, best_possible_score = 2 * nargs;

   while (curr < last) {
	if (nargs == 0 && curr->nargs == 0) { best  = curr; break; }
	if (alternate == NULL && !nargs && !curr->min_nargs) alternate = curr;
	if (nargs >= curr->min_nargs && nargs <= curr->nargs) {
	   if ((curr_score = score_func(curr, sig, vectorized)) > best_score) {
		best = curr;
		if (curr_score == best_possible_score) break;
		best_score = curr_score;
	   }
	}
	curr++;
   }

   if (best == NULL)		/* use alternate, if necessary/possible */
	best = alternate;

   if (best) { best->func(); return; }

   SLdo_pop_n(SLang_Num_Function_Args);
   Slirp_usage(usage_idx, usage_idx + stop - first, vectorized);
} /* }}} */
