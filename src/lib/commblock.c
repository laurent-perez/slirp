
static SLtype CommBlock_Type;

static void array_destroy_no_op(SLang_Array_Type *at)	/* {{{ */
{
  (void) at;
} /*}}}*/

static void block_destroy_no_op(SLtype t, void* p)	/* {{{ */
{
  (void) t;  (void) p;
} /*}}}*/

static void* findcomm(char* name, void *list, size_t next) /*{{{*/
{
   CommBlock *item = (CommBlock*) list;

   while (item->name != NULL && strcmp(item->name, name))
	 item = (CommBlock*) (((char*)item) + next);

   return ( item->name == NULL ? NULL : item);
} /*}}}*/

static int pop_common_block(CommBlock **block) /*{{{*/
{
    if ( SLang_peek_at_stack() == SLANG_STRING_TYPE) {

	char *blname = NULL;

	if (SLang_pop_slstring(&blname) == 0) {

	   *block = (CommBlock *)findcomm(blname, CommBlocks, sizeof(CommBlock));
	   SLang_free_slstring(blname);

	   if (*block == NULL)
		SLang_verror(SL_USAGE_ERROR,
			"No common blocks found with name: %s", blname);
	}
    }
    else  {

	SLang_MMT_Type *mmt;

	if (NULL != (mmt = SLang_pop_mmt(CommBlock_Type))) {
	    *block = (CommBlock *)SLang_object_from_mmt(mmt);
	    /* Since block destroy is a no-op, we can free the mmt here */
	    SLang_free_mmt(mmt);
	}
    }

    return SLang_get_error();
} /*}}}*/

static int push_fstring(CommBlockVar *v)  /* {{{ */
{  
   SLang_Array_Type *a;

   if (v->rank == 1)
	return SLang_push_malloced_string( SLmake_nstring((char *)v->data, v->dims[0]));

   /* Read-only b/c mapping back to CHARACTER doesn't happen on elem assign */
   if (NULL == (a = SLang_create_array(v->sltype,1,NULL, v->dims+1, v->rank-1)))
	return -1;

   a->client_data = v->data;
   unpack2c((char*)v->data, a, v->dims[0]);
   return SLang_push_array(a, 1);
}  /* }}} */

static int pop_fstring(CommBlockVar *v)  /* {{{ */
{
   char *s;

   if (SLang_pop_slstring(&s) != 0)
	return -1;

   strncpy((char *)v->data, s, v->dims[0]);
   SLang_free_slstring(s);

   return 0;
}  /* }}} */

static int push_scomplex(CommBlockVar *v) /*{{{*/
{
   /* Map Fortran single-prec complex to S-Lang's double-prec (only) complex */
   float *fc = (float *)v->data;

   if (v->rank == 0) {			/* Single value */
	double dc[2];
	dc[0] = fc[0];
	dc[1] = fc[1]; 
	return SLang_push_value (v->sltype, dc);
   }
   else {
	SLang_Array_Type *a;		/* mark array as read-only b/c  */
	unsigned int n;			/* mapping back to single prec  */
	double *dc;			/* isn't done on element assign */

	a = SLang_create_array(v->sltype, 1, NULL, v->dims, v->rank);
	if (a == NULL) return -1;

	dc = (double *)a->data;
	n = a->num_elements;
	while(n--) { *dc++ = *fc++; *dc++ = *fc++; }

	return SLang_push_array(a, 1);
   }
}  /* }}} */

static int pop_scomplex(CommBlockVar *v)  /* {{{ */
{
   double dc[2];
   float *fc = (float *)v->data;

   if (SLang_pop_value (v->sltype, dc) != 0)
	return -1;

   fc[0] = dc[0];
   fc[1] = dc[1];

   return 0;
}  /* }}} */

static int push_generic(CommBlockVar *v) /*{{{*/
{
   SLang_Array_Type *a;

   if (v->rank == 0)
	return SLang_push_value (v->sltype, v->data);

   if (NULL == (a = SLang_create_array(v->sltype,0,v->data,v->dims,v->rank)))
	return -1;

   a->free_fun = array_destroy_no_op;	/* prevent freeing of data */
   return SLang_push_array(a, 1);
} /*}}}*/

static int pop_generic(CommBlockVar *v)  /* {{{ */
{
    return SLang_pop_value (v->sltype, v->data);
}  /* }}} */

static int get_var(CommBlock *block, char *name, CommBlockVar **var) /*{{{*/
{
   if ( NULL == (*var = (CommBlockVar *)findcomm(name, block->vars, sizeof(CommBlockVar)))) {
	SLang_verror(SL_USAGE_ERROR,
		"Common block %s does not contain variable named: %s", 
							block->name, name);
	return -1;
   }

   return 0;
} /*}}}*/

static int block_sget(SLtype type, char* varname)  /* {{{ */
{
   CommBlock *block;
   CommBlockVar *var;

   (void) type;

   if (pop_common_block(&block) != 0 || get_var(block, varname, &var) != 0)
	return -1;

   return var->push(var);
}  /* }}} */

static int block_sput(SLtype type, char* varname)  /* {{{ */
{
   CommBlock *block;
   CommBlockVar *var;

   (void) type; 

   if (pop_common_block(&block) != 0 || get_var(block, varname, &var) != 0)
	return -1;

   if (var->rank == 0 || (var->sltype == SLANG_STRING_TYPE && var->rank == 1))
	return var->pop(var);

   (void) SLdo_pop();
   SLang_verror(SL_NOT_IMPLEMENTED,"CommBlock array assign not implemented.");
   return -1;

}  /* }}} */

static char* block_string(SLtype type, void* block_mmt_ptr)  /* {{{ */
{
   char buf[256];
   char *str;
   CommBlock *b;
   CommBlockVar *v;
   unsigned int strsize = 4 * sizeof(buf);

   b = (CommBlock *)SLang_object_from_mmt( *(SLang_MMT_Type**)block_mmt_ptr );

   if (NULL == (str = SLmalloc(strsize)))
	return SLmake_string("block_string: malloc failure");

   sprintf(str, "Fortran common block with %d variable", b->num_vars);
   if (b->num_vars > 1)
	strcat(str, "s:");
   else
	strcat(str, ":");

   v = b->vars;

   while (v->name != NULL) {

	int rank = v->rank;
	SLindex_Type *dims = v->dims;

	type = v->sltype;
	if (type == SLANG_STRING_TYPE) {
	   rank--;
	   dims++;
	}

	sprintf(buf, "\n\t%s %s", v->name, SLclass_get_datatype_name(type));

	if (rank) {

	   int i = 0;
	   char dimbuf[64];

	   strcat(buf, "[");

	   while (1) {
		sprintf(dimbuf, "%d", dims[i++] );
		strcat(buf, dimbuf);
		if (i < rank)
		   strcat(buf, ",");
	        else
		   break;
	   }

	   strcat(buf, "]");
	}

	strcat(str, buf);

	if (type == SLANG_STRING_TYPE) {
	   sprintf(buf, " (length %d)", v->dims[0]);
	   strcat(str, buf);
	}

	if (strsize - strlen(str) < sizeof(buf)) {
	   strsize *= 2;
	   if (NULL == (str = SLrealloc(str, strsize)))
	      return SLmake_string("block_string: realloc failure");
	}

	v++;
    }
    return str;
}  /* }}} */

static void get_commblock(void) /*{{{*/
{
   CommBlock *block;
   SLang_MMT_Type *mmt;

   if (SLang_Num_Function_Args != 1 || pop_common_block(&block) != 0) 
	USAGE("CommBlock = get_commblock(common_block_name)");

   if ( NULL == (mmt = SLang_create_mmt(CommBlock_Type, block)))
	return;
	         
  SLang_push_mmt(mmt);
} /*}}}*/

static void get_commblock_value(void) /*{{{*/
{
   char *varname;

   if (SLang_Num_Function_Args != 2 || SLang_pop_slstring(&varname) != 0)
	USAGE("value = get_commblock_value(CommBlock_or_name, var_name)");

   (void) block_sget(0, varname);

   SLang_free_slstring(varname);
} /*}}}*/

static void get_commblock_list(void)  /* {{{ */
{
   char **data;
   SLang_Array_Type *a;
   SLindex_Type i, n;
   
   if (SLang_Num_Function_Args != 0)
	USAGE("String_Type[] = get_commblock_list()");

   n = Num_CommBlocks;

   if (NULL == (a = SLang_create_array(SLANG_STRING_TYPE, 0, NULL, &n, 1)))
	return;

   data = (char **)a->data;

   for (i=0; i<n; i++)
	if (NULL == (data[i] = SLang_create_slstring(CommBlocks[i].name))) {
	   while(i--)
		SLang_free_slstring(data[i]);
	   SLang_free_array(a);
	   return;
	}

   SLang_push_array(a, 1);
}  /* }}} */

static void set_commblock_value(void) /*{{{*/
{
   char *varname;
   SLang_Any_Type *value;

   if (SLang_Num_Function_Args != 3
   			|| SLang_pop_anytype(&value) != 0
			|| SLang_pop_slstring(&varname) != 0
			|| SLang_push_anytype(value) != 0
			|| SLreverse_stack(2) != 0)
	USAGE("set_commblock_value(CommBlock_or_name, var_name, value)");

   (void) block_sput(0, varname);

   SLang_free_slstring(varname);
} /*}}}*/

static SLang_Intrin_Fun_Type CommBlock_Funcs[] = /*{{{*/
{
   MAKE_INTRINSIC_0("get_commblock_list", get_commblock_list, SLANG_VOID_TYPE),
   MAKE_INTRINSIC_0("get_commblock", get_commblock, SLANG_VOID_TYPE),
   MAKE_INTRINSIC_0("get_commblock_value", get_commblock_value, SLANG_VOID_TYPE),
   MAKE_INTRINSIC_0("set_commblock_value", set_commblock_value, SLANG_VOID_TYPE),
   SLANG_END_INTRIN_FUN_TABLE
}; /*}}}*/

static void initblock(int *block_index, int *count, ...) /*{{{*/
{
   va_list ap;
   CommBlock *block = CommBlocks + *block_index;
   CommBlockVar *vars = block->vars;
   int i = *count;

   va_start(ap, *count);

   while(i--) {
	vars->data = va_arg(ap, void*);
	vars++;
   }
   va_end(ap);
} /*}}}*/

static int init_common_blocks(SLang_NameSpace_Type *ns) /*{{{*/
{
   CommBlock *block = CommBlocks;
   SLang_Class_Type *cl;

   /* FIXME: to use >1 common-block module each needs its own namespace */
   if (SLang_is_defined((char*)"_CommBlock_initialized")) {

	if (SLang_load_string("CommBlock") != 0 ||
				SLang_pop_datatype(&CommBlock_Type) != 0)
	   return -1;
   }
   else {

   	if (0 != SLang_load_string((char*)
			"public variable _CommBlock_initialized=1")
	    || NULL == (cl = SLclass_allocate_class ("CommBlock"))
	    || 0 != SLclass_set_destroy_function (cl, block_destroy_no_op)
	    || 0 != SLclass_set_sget_function (cl, block_sget)
	    || 0 != SLclass_set_sput_function (cl, block_sput)
	    || 0 != SLclass_set_string_function (cl, block_string)
	    || 0 != SLclass_register_class (cl, SLANG_VOID_TYPE,
				sizeof(CommBlock), SLANG_CLASS_TYPE_MMT))
	   return -1;

	CommBlock_Type = SLclass_get_class_id(cl);
    }

   if (0 != SLns_add_intrin_fun_table (ns, CommBlock_Funcs, "__CommBlocks__"))
	return -1;

   while(block->name != NULL) {
	block->init_func (initblock);
	block++;
   }

   if (isatty(fileno(stdout)))
	SLang_vmessage(
   	"This module interfaces to %d Fortran common blocks.\n"
   	"You may need to invoke the Fortran routine(s) which\n"
   	"define(s) them prior to using their values in S-Lang.\n"
	"You may use struct.field notation to access or modify\n"
	"the values of individual common block variables.\n"
	"Type \"help commblock\" for more information.\n", Num_CommBlocks);

   return 0;
} /*}}}*/
