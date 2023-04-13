
static int vec_pop(Slirp_Ref *r, int ndims, int nelems, VecSpec *vs)  /* {{{ */
{
   int ndims_got, vectored, biggest;
   SLang_Array_Type *arr;

   if (pop_array_or_ref(r, 0x2, 0) == -1) return -1;
   if (ndims) r->flags |= REF_FLAG_ARRAY_EXPECTED;
   if (r->array == NULL) return 0;

   arr = r->array;
   ndims_got = arr->num_dims;
   if (ndims_got < ndims && arr->num_elements < (SLuindex_Type)nelems) {
	SLang_verror(SL_INTRINSIC_ERROR, (char*)"array with %d elements required", nelems);
	return -1;
   }

   vectored = ndims_got > ndims;	/* is actual rank > expected rank? */
   biggest = !vs->master || (ndims_got > (int)vs->master->array->num_dims);
   if (biggest) vs->master = r;

   if (vectored) {
	if (biggest) {
	   int i = arr->num_dims - ndims;
	   vs->num_iters = 1;				/* see docs for   */
	   while (i)				  	/* theory behind  */
		vs->num_iters *= arr->dims[--i];	/* calculation of */
							/* vectorization  */
	   if (vs->num_iters > 0)			/* parameters     */
		r->vstride = arr->num_elements / vs->num_iters;
	}
	else if (arr->num_elements == vs->master->array->num_elements)
	   r->vstride = vs->master->vstride;
	else
	   r->vstride = 1;

	vs->flags |= VSFLAG_VECTORED;
   }
   return 0;
} /* }}} */

static int vec_validate(VecSpec *vs, unsigned int nargs, ...) /*{{{*/
{
   va_list ap;
   SLuindex_Type num_elems, stride;

   if (vs->master) {
	num_elems = vs->master->array->num_elements;
	stride = VECTORED(*vs) ? vs->master->vstride : num_elems;
   }
   else
	num_elems = stride = 1;

   va_start(ap, nargs);
   while (nargs--) {

	Slirp_Ref *r = va_arg(ap, Slirp_Ref *);
	SLang_Array_Type *arr = r->array;

	if (arr == NULL) {
	   if (!(r->flags & REF_FLAG_FREE_DATA || r->mmt)) {
		/* If data was not allocated AND this is not an MMT type, */
		/* then arg was not marshaled, so allocate space for it   */
		void *a = SLmalloc(num_elems * r->sizeof_type);
		if (a == NULL) {
		   SLang_verror(SL_INTRINSIC_ERROR, (char*)"No memory for output array");
		   return -1;
		}
		*r->data = a;		/* not to be freed during finalize */
		r->vstride = vs->master ? vs->master->vstride : 0;
	   }
	   else if (r->flags & REF_FLAG_ARRAY_EXPECTED && stride > 1) {
		SLang_verror(SL_INTRINSIC_ERROR, (char*)"Scalar cannot be used here");
		return -1;
	   }
	   continue;
	}

	if (arr->num_elements != num_elems) {
	   if (stride > 1 && arr->num_elements != stride) {
		SLang_verror(SL_INTRINSIC_ERROR, (char*)"Array shape or length mismatch");
		return -1;
	   }
	   r->vstride = 0;
	}
   }
   va_end(ap);
   return 0;
} /* }}} */

static int vec_push(void *v, VecSpec *vs, SLtype type, int free_strings) /*{{{*/
{
   SLang_Array_Type *arr, *master = vs->master->array;
   SLindex_Type num_elems = master->num_elements;

   /* String and opaque arrays are populated as if 1D, then reshaped to ND */
   if (type == SLANG_STRING_TYPE) {

	char **strs = (char**)v;
	SLindex_Type i = num_elems;

	if ((arr = SLang_create_array(type, 0, NULL, &num_elems, 1))) {
	   while (i--)
		if (SLang_set_array_element (arr, &i, &strs[i]) == -1) {
		   SLang_free_array(arr);
		   arr = NULL;
		   break;
		}
	}

	if (free_strings) { i = num_elems; while (i--) SLfree(strs[i]); }
	SLfree( (char*) strs);
	arr->num_dims = master->num_dims;
	memcpy(arr->dims, master->dims, arr->num_dims * sizeof(SLindex_Type));
   }
#ifdef NUM_RESERVED_OPAQUES
   else if (type >= First_Opaque_Type && sltype_to_slirp_type(type) != NULL) {

	void **objects = (void**)v;
	SLindex_Type i = num_elems;
	SLang_MMT_Type *mmt;

	if ((arr = SLang_create_array(type, 0, NULL, &num_elems, 1))) {
	   while (i--)  {
		mmt = create_opaque_mmt(type, objects[i],  0);
		if (mmt == NULL ||
			SLang_set_array_element (arr, &i, &mmt) == -1) {
		   SLang_free_array(arr);
		   arr = NULL;
		   break;
		}
	   }
	}
	SLfree( (char*) objects);
	arr->num_dims = master->num_dims;
	memcpy(arr->dims, master->dims, arr->num_dims * sizeof(SLindex_Type));
   }
#endif
   else {
	if (type == SLANG_COMPLEX_TYPE && vs->master->sizeof_type <
	      						2*sizeof(double)) {
	   float  *fc = (float *) v;
	   double *dc = (double*) SLmalloc(num_elems * master->sizeof_type);
	   if (dc == NULL) return -1;
	   while (num_elems--) { *dc++ = *fc++; *dc++ = *fc++; }
	   dc -= master->num_elements * 2;  /* rewind */
	   SLfree((char*)v);
	   v = dc;
	}
	arr = SLang_create_array(type, 0, v, master->dims, master->num_dims);
   }

   if (arr == NULL) return -1;
   return SLang_push_array(arr, 1);
} /* }}} */

static void establish_dims(VecSpec *vs, char *types, ...) /*{{{*/
{
   va_list ap;
   SLang_Array_Type *array = vs->master->array;
   int ndims = (int) strlen(types);
   int which = array->num_dims - ndims;

#  define typeassign(type) *(va_arg(ap, type*)) = (type) array->dims[which++]
   va_start(ap, types);
   while (ndims--) {
	switch (types[ndims])
	{
	case 'b' : typeassign(signed char); break;
	case 'B' : typeassign(unsigned char); break;
	case 'h' : typeassign(short); break;
	case 'H' : typeassign(unsigned short); break;
	case 'i' : typeassign(int); break;
	case 'I' : typeassign(unsigned int); break;
	case 'l' : typeassign(long); break;
	case 'L' : typeassign(unsigned long); break;
	default:   SLang_verror(SL_TYPE_MISMATCH,
		   (char*)"vector dimension %d not an integral type", which);
		   ndims = 0;
	}
   }
   va_end(ap);
} /* }}} */
