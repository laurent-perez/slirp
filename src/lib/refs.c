
typedef struct _Slirp_Ref {			/* Ref handling code {{{ */
#define	REF_FLAG_FREE_DATA			0x01
#define	REF_FLAG_IS_OPAQUE			0x02
#define	REF_FLAG_COLUMN_MAJOR			0x04
#define	REF_FLAG_ARRAY_EXPECTED			0x08
   unsigned int		flags;		/* Slirp_Ref encapsulates S-Lang    */
   SLtype		sltype;		/* array, ref, and MMT types, the   */
   size_t		sizeof_type;	/* latter two of which are seen as  */
   void			**data;		/* pointing to a single object      */
   unsigned int		vstride;	/* how to find "next" vectored elem */
   SLang_Array_Type	*array;
   SLang_Ref_Type	*ref;
   SLang_MMT_Type	*mmt;		/* mmt supports passing around C    */
} Slirp_Ref;				/* ptr arrays of indeterminate size */

static Slirp_Ref* ref_new(SLtype t,size_t typesize, void *d,unsigned int flags)
{
   Slirp_Ref *ref;
   if ((ref = (Slirp_Ref*)SLcalloc( sizeof(Slirp_Ref), 1)) != NULL) {
	ref->sltype = t;
	ref->sizeof_type = typesize;
	ref->flags = flags;
	ref->data = (void**)d;
	*ref->data = NULL;
   }
   return ref;
}

static int ref_finalize(Slirp_Ref *r) /* {{{ */
{
   int status = 0;
   if (r == NULL) return 0;

   if (r->ref) {

        void *ref_value = NULL; SLtype ref_type = 0; double dc[2];

	if (r->flags & REF_FLAG_IS_OPAQUE) { 	/* wrap aggregates/opaques  */
#ifdef NUM_RESERVED_OPAQUES			/* in mmt before ref assign */
	   void *opaqval = *r->data;		/* the mmt w/be freed when  */
	   if (opaqval == NULL) {		/* the S-Lang object goes   */
		ref_value = NULL;		/* out of scope		    */
		ref_type = SLANG_NULL_TYPE;
	   }
	   else {
		SLang_MMT_Type *mmt = create_opaque_mmt(r->sltype, opaqval, 0);
		ref_value = &mmt;
		ref_type = r->sltype;
	   }
#endif
	}
	else {
	   ref_type = r->sltype;
	   if (ref_type == SLANG_COMPLEX_TYPE && r->sizeof_type < sizeof(dc)) {
		float *fc = (float*) r->data;
		dc[0] = fc[0];
		dc[1] = fc[1];
		ref_value = dc;
	   }
	   else
		ref_value = r->data;
	}

	status = SLang_assign_to_ref (r->ref, ref_type, ref_value);
	SLang_free_ref(r->ref);
   }
   else if (r->array) {
#ifdef HAVE_FORTRAN_CODE
	if ((r->flags & REF_FLAG_COLUMN_MAJOR) && TRANSPOSE(1,r->array) == -1)
		return -1;
#endif
	SLang_free_array(r->array);
   }
   else if (r->mmt)
	SLang_free_mmt(r->mmt);

   if (r->flags & REF_FLAG_FREE_DATA)
	SLfree( (char*) r->data );

   SLfree((char*)r);
   return status;
} /* }}} */

static void finalize_refs(unsigned int nargs, ...) /* {{{ */
{
   va_list ap;
   va_start(ap, nargs);
   while (nargs--) ref_finalize(va_arg(ap, Slirp_Ref *));
   va_end(ap);
} /* }}} */

static unsigned int ref_get_size(Slirp_Ref *r, int which_dimension) /* {{{ */
{
   if (r->array) {
	if (which_dimension == 0)
	   return (unsigned int)r->array->num_elements;
	else if (which_dimension < 0)
	   return r->array->num_dims;
	else {
	   which_dimension--;
	   if ((unsigned int)which_dimension < r->array->num_dims)
		return (unsigned int)r->array->dims[which_dimension];
	   else
		return 0;
	}
   }

   return 1;
} /* }}} */

extern LINKAGE int _SLang_get_class_type (SLtype t);	/* quasi-public  */

#ifdef NUM_RESERVED_OPAQUES
static SLtype sltype_to_opaque_ptr_type(SLtype sltype) /*{{{*/
{
   Reserved_Opaque_Type *pt;

   if (sltype > Last_Reserved_Opaque_Type) return opaque_ptr_Type;
   if (sltype == void_ptr_Type) return void_ptr_Type;

   pt = Reserved_Opaque_Types;		/* sequential search, but s/b < O(n) */
   while (pt->name) {			/* since list is ordered by expected */
	if (pt->masked_type == sltype)	/* frequency of use for each SLtype  */
	   return *pt->type;		
	pt++;
   }
   return 0;
} /*}}}*/
#endif

static int try_pop_mmt(SLtype type, SLang_MMT_Type **mmt) /*{{{*/
{
   static SLang_Name_Type *cl_type_func;	/* SLang_pop_mmt doesn't */
   int classtype;				/* validate that type is */
						/* an MMT, so we do here */
   *mmt = NULL;					/* FIXME: remove v2.0.7  */

   if (cl_type_func == NULL)
	cl_type_func = SLang_get_function( (char*) "__class_type");

   if (cl_type_func == NULL)
      return 0;

   if (-1 == SLang_push_datatype(type) ||		/* do the hard way, */
	-1 == SLexecute_function(cl_type_func) ||	/* as C api lacks   */
	-1 == SLang_pop_int(&classtype))		/* get_class_type() */
	return -1;

   if (classtype == SLANG_CLASS_TYPE_MMT) {
	*mmt = SLang_pop_mmt(type);
	return 1;
   }
   return 0;
} /*}}}*/

#define POP_FLAG_NULLABLE	0x1
#define POP_FLAG_VECTORIZE	0x2
static int pop_array_or_ref(Slirp_Ref *r, int flags, int defaultable) /*{{{*/
{
   SLtype type;
   unsigned int i, objtype;
#ifdef NUM_RESERVED_OPAQUES
   unsigned int is_opaque;
#endif

   if (r == NULL) {
	SLang_verror(SL_INTRINSIC_ERROR, (char*)"Attempted NULL reference (out of memory?)");
	return -1;
   }

   if (defaultable && SLang_Num_Function_Args < defaultable) {
	r->ref   = NULL;	/* observe that only NULL can be */
	*r->data = NULL;	/* assigned as the default value */
	return 0;
   }
   
   objtype = SLang_peek_at_stack();

   if ((flags & POP_FLAG_NULLABLE) && objtype == SLANG_NULL_TYPE) {
	r->ref   = NULL;		/* nullable flag: a pointer arg for */
	*r->data = NULL;		/* which NULL is a legitimate value */
	return SLang_pop_null ();
   }

   type = r->sltype;

#ifdef NUM_RESERVED_OPAQUES
   is_opaque =(type >= First_Opaque_Type && sltype_to_slirp_type(type) != NULL);
   if (is_opaque) r->flags |= REF_FLAG_IS_OPAQUE;
#endif

   switch(objtype) {

	case SLANG_ARRAY_TYPE:

	   if (SLang_pop_array_of_type(&r->array, type) == -1)
		return -1;

#ifdef HAVE_FORTRAN_CODE
	   if (r->flags & REF_FLAG_COLUMN_MAJOR) {
		if (flags & POP_FLAG_VECTORIZE)		/* vectorizable arrs*/
		   r->flags ^= REF_FLAG_COLUMN_MAJOR;   /* r not transposed */
		else if (TRANSPOSE(0,r->array) == -1)
		   return -1;
	   }
#endif

	   i = r->array->num_elements;
#ifdef NUM_RESERVED_OPAQUES
	   if (is_opaque) {
	      	Slirp_Opaque *ot;
		SLang_MMT_Type** mmts = (SLang_MMT_Type**)r->array->data;
		void **arr = (void**)SLmalloc(i * sizeof(void*) );
		if (arr == NULL) return -1;

		while (i--) {
		   ot = (Slirp_Opaque*) SLang_object_from_mmt (mmts[i]);
		   if (ot == NULL) {
			SLfree((char*)arr);
			return -1;
		   }
		   arr[i] = ot->instance;
		}

		*r->data = (void*)arr; r->data = (void**)arr;
		r->flags |= REF_FLAG_FREE_DATA;
	   }
	   else
#endif
	   if (type == SLANG_COMPLEX_TYPE &&
		 		r->sizeof_type < r->array->sizeof_type) {
		double *dc = (double*) r->array->data;
		float  *fc = (float *) SLmalloc(i * r->sizeof_type);
		if (fc == NULL) return -1;
		*r->data = fc; r->data = (void**)fc;
		while (i--) { *fc++ = (float) *dc++; *fc++ = (float) *dc++; }
		r->flags |= REF_FLAG_FREE_DATA;
	   }
	   else {
		*r->data = r->array->data;
		r->data = (void**)*r->data;
	   }

	   break;

	case SLANG_REF_TYPE:

	   /* Refs can only send values one-way (C to S-Lang, not reverse) */
	   if (SLang_pop_ref(&r->ref) == -1)
		return -1;

	   /* Ref is assumed to point to a scalar instance of the  */
	   /* refd type, so declare enough space to hold one such. */
	   *r->data = (void*)SLmalloc(r->sizeof_type);
	   if (*r->data == NULL) return -1;
	   memset(*r->data, 0, r->sizeof_type);
	   r->flags |= REF_FLAG_FREE_DATA;
	   r->data = (void**)*r->data;
	   break;

	/* Allow scalars to used as if they were 1-element arrays */
	case SLANG_CHAR_TYPE: case SLANG_UCHAR_TYPE:
	case SLANG_SHORT_TYPE: case SLANG_USHORT_TYPE:
	case SLANG_INT_TYPE: case SLANG_UINT_TYPE:
	case SLANG_LONG_TYPE: case SLANG_ULONG_TYPE:
	case SLANG_FLOAT_TYPE: case SLANG_DOUBLE_TYPE:
	case SLANG_COMPLEX_TYPE: case SLANG_STRING_TYPE:

	   /* Accomodate FORTRAN-style pass by reference semantics */
	   if (map_scalars_to_refs &&
		 	SLang_pop_array_of_type(&r->array,type) == 0) {

		*r->data = (void*)SLmalloc(r->sizeof_type);
		if (*r->data == NULL) return -1;

		if (r->sizeof_type == r->array->sizeof_type)
		   memcpy(*r->data, r->array->data, r->sizeof_type);
		else if (type == SLANG_COMPLEX_TYPE) {
		   double *dc = (double*) r->array->data;
		   float  *fc = (float*) *r->data;
		   fc[0] = (float)dc[0];
		   fc[1] = (float)dc[1];
		}
		else  {
		   SLang_verror(SL_TYPE_MISMATCH, (char*)
			"mismatched type sizes, when popping scalar as ref");
		   SLang_free_array(r->array);
		   return -1;
		}

		r->data = (void**)*r->data;
		r->flags |= REF_FLAG_FREE_DATA;
		/* Nullify to distinguish between vectored/non-vectored args */
		SLang_free_array(r->array); r->array = NULL;
		break;
	   }				/* intentional fallthrough */

	default:

#ifdef NUM_RESERVED_OPAQUES
	   if (objtype >= First_Opaque_Type && 
		 		sltype_to_slirp_type(objtype) != NULL) {

		if (!(flags & POP_FLAG_VECTORIZE))
		   type = sltype_to_opaque_ptr_type(type);

		if (type) {

		   Slirp_Opaque *otp;
		   if (SLang_pop_opaque(type, NULL, &otp) == -1)
			return -1;
		
		   if (flags & POP_FLAG_VECTORIZE) {
			void **arr = (void**) SLmalloc(sizeof(void*));
			if (arr == NULL) return -1;
			arr[0] = otp->instance;
			*r->data = arr;
			r->flags |= REF_FLAG_FREE_DATA;
		   }
		   else
			*r->data = otp->instance;

		   r->data = (void**)*r->data;
		   r->mmt = otp->mmt;
		   return 0;
		}
	   }
	   else
#endif
	   if ( try_pop_mmt(objtype, &r->mmt) == 1 &&
		(*r->data = SLang_object_from_mmt (r->mmt)) != NULL) {
		   r->data = (void**)*r->data;	 /* not flagged for freeing */
		   return 0;
	   }

	   SLang_verror(SL_TYPE_MISMATCH, (char*)
			"context requires array, ref, or opaque pointer");
	   return -1;
   }
   return 0;
} /*}}}*/
/* }}} */
