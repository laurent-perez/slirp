

/* C++ helper macros {{{ */
#define pop_class_instance(argno, objtype, sltype, obj, opaque)\
   (SLang_pop_opaque(sltype, NULL, &opaque) ==0 && opaque != NULL && (&(obj = *((objtype*)opaque->instance)) > 0))

#define pop_class_instanced(argno, objtype, sltype, obj, opaque, defval)\
   (SLang_Num_Function_Args >= argno ? (SLang_pop_opaque(sltype, NULL, &opaque) ==0 && opaque != NULL && (&(obj = *((objtype*)opaque->instance)) > 0)) : (&(obj = (objtype)defval) > (void*)0))

#define FIELD_SET(klass, field, ctype, sltype)\
static void sl_##klass##_##field##_set (void) \
{\
   klass* arg1;\
   Slirp_Opaque* arg1_o = NULL;\
   ctype arg2;\
\
   if (SLang_Num_Function_Args != 2 ||\
      SLang_pop_##sltype((sltype*)&arg2) == -1 ||\
      SLang_pop_opaque(klass##_Type, (void**)&arg1, &arg1_o) == -1 )\
   {SLang_verror(SL_USAGE_ERROR,(char*)"Usage: "#klass"_"#field"_set("#klass","#ctype")");     return;}\
\
    arg1->field = (ctype) arg2;\
    SLang_free_opaque(arg1_o);\
}

#define FIELD_GET(klass, field, ctype, sltype)\
static void sl_##klass##_##field##_get (void) \
{\
   klass* arg1;\
   Slirp_Opaque* arg1_o = NULL;\
\
   if (SLang_Num_Function_Args != 1 ||\
      SLang_pop_opaque(klass##_Type, (void**)&arg1, &arg1_o) == -1 )\
   {SLang_verror(SL_USAGE_ERROR,(char*)"Usage: "#klass"_"#field"_get("#klass")");return;}\
\
   SLang_push_##sltype(arg1->field);\
   SLang_free_opaque(arg1_o);\
}

#define FIELD_GET_OR_SET(klass,field) \
static void sl_##klass##_##field##_set_or_get(void) \
{ SLang_Num_Function_Args <= 1 ? sl_##klass##_##field##_get () : sl_##klass##_##field##_set (); }
/* }}} */

static char* make_method_signature (SLtype type, char *member, char *ns) /*{{{*/
{
   char *sig;
   size_t ns_len = strlen(ns);
   char *clname = SLclass_get_datatype_name (type);

   if ((sig = SLmalloc(strlen(clname) + ns_len + strlen(member) + 5)) != NULL) {

	strcpy(sig, ns);
	if (ns_len)
	   strcat(sig, "->");
	strcat(sig, clname);
	char *p = sig + strlen(sig) - 5;	/* ignore _Type suffix */
	*p++ = '_';
	*p = 0;
	strcat(sig, member);
   }

   return sig;
} /*}}}*/

static int function_cache (char *name, SLang_Name_Type **func) /*{{{*/
{
   static char *ns = NULL;
   int retval;

   if (! ns) {
	ns = (char*) "__SLIRP_FUNC_CACHE_NS__";
	SLns_load_string((char*)"variable cache=Assoc_Type[Any_Type,NULL]",ns);
   }

   char *statement;
   if ( (statement = SLmalloc(strlen(name) + 20)) == NULL)
	return 0;

   if (*func != NULL) {		/* Store a function in the cache ... */

   	if ( SLang_push_function(*func) == -1 && !(retval=0))
	   goto done;

	sprintf(statement, "cache[\"%s\"] = ()", name);
   }
   else
	sprintf(statement, "cache[\"%s\"]", name);   /* ... or look one up */

   if ( SLns_load_string(statement, ns) == -1 && !(retval=0))
	goto done;

   if (*func == NULL)
	switch(SLang_peek_at_stack())
	{
	   case SLANG_REF_TYPE:
	   	retval = (*func = SLang_pop_function()) != NULL;
		break;
	   default: SLdo_pop(); retval = 0;
	}

   done:
   SLfree(statement);
   return retval;
} /*}}}*/

static SLang_Name_Type* resolve_inheritance (SLtype obj_type, char *member,						SLang_MMT_Type **obj_mmt_ptr) /*{{{*/
{
   Slirp_Opaque *ot;
   SLang_Name_Type *func = NULL;
   char *funcname_to_attempt = NULL;

   if (	-1 == SLang_pop_opaque(obj_type, NULL, &ot))
	return NULL;

   *obj_mmt_ptr = ot->mmt;
   Slirp_Type *st = ot->type;
   SLtype classtype = SLclass_get_class_id(st->slclass);
   char *signature = make_method_signature(classtype, member, (char*)"");

   if (function_cache(signature, &func) == 1)
	goto done;

   while (1) {

	funcname_to_attempt = make_method_signature(classtype, member, slns);

	if (funcname_to_attempt != NULL) {
	   func = SLang_get_function(funcname_to_attempt);
	   SLfree(funcname_to_attempt);
	}

	if (func != NULL || st->parent == NULL)
	   break;

	st = st->parent;
	classtype = SLclass_get_class_id(st->slclass);
   }

   if (func != NULL)
	(void) function_cache(signature, &func);
   else
	SLang_verror(SL_UNDEFINED_NAME, (char*)
		"Could not determine function to invoke for %s", signature);

   done:

   SLang_free_opaque(ot);
   SLfree(signature);

   return func;
} /*}}}*/

static int cpp_obj_sget (SLtype obj_type, char *member_name) /*{{{*/
{ 
   SLang_Name_Type *func;
   SLang_MMT_Type *obj_mmt = NULL;

   if ((func = resolve_inheritance(obj_type, member_name, &obj_mmt)) == NULL)
	return -1;

   /* 2 class instance objs on stack implies field ref is a method invocation */
   if (SLstack_depth() > 0 && SLang_peek_at_stack() == (int)obj_type)
	return SLang_push_function(func);

   if ( 0 == SLang_start_arg_list() && 0 == SLang_push_mmt(obj_mmt)
				    && 0 == SLang_end_arg_list())
	return SLexecute_function(func);

  return -1;
} /*}}}*/

static int cpp_obj_sput (SLtype obj_type, char *member_name) /*{{{*/
{ 
   SLang_Name_Type *func;
   SLang_Any_Type *value;
   SLang_MMT_Type *obj_mmt;
   int retval = -1;

   if ((func = resolve_inheritance(obj_type, member_name, &obj_mmt)) == NULL
					|| SLang_pop_anytype(&value) == -1)
	return -1;

   if ( SLang_start_arg_list() == 0	&& SLang_push_mmt(obj_mmt) == 0
					&& SLang_push_anytype(value) == 0
					&& SLang_end_arg_list() == 0)
	retval = SLexecute_function(func);

   SLang_free_anytype(value);

   return retval;
} /*}}}*/
