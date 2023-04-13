typedef struct {
   unsigned int num_iters;	/* number of wrapped function call iterations */
   unsigned int flags;		/* see VSFLAG_ macros below */
   Slirp_Ref *master;		/* describes controlling array dimensionality */
} VecSpec;

#define push_one_element_0(func, item, type)  func(item)
#define push_one_element_1(func, item, type)  func(type, item, 0)

#define VSFLAG_VECTORED		0x1	/* Do not change these without also */
#define VSFLAG_OPENMP		0x2	/* changing VecSpec initialization  */
					/* within emit_declaration_block    */

#define VECTORED(vs)	((vs).flags & VSFLAG_VECTORED)
#define OPENMP(vs)	((vs).flags & VSFLAG_OPENMP)

#define VEC_RETURN(v, rewind, sltype, pusher, typed, free_elements) \
   if (rewind && VECTORED(vs)) v -= vs.master->array->num_elements; \
   if (vs.master) { vec_push((void*)v, &vs, sltype, free_elements); } \
   else { push_one_element_##typed(pusher, v[0], sltype); SLfree( (char*) v); }

#define VEC_ALLOC_RETVAL(type, args) \
   if ((retval = (type*)SLmalloc(vs.num_iters * sizeof(type))) == NULL) \
	{finalize_refs(args); SLang_verror(SL_INTRINSIC_ERROR,(char*)"out of memory");return;}

#define VR(n) arg##n##_r
#define VI(n) arg##n += arg##n##_r->vstride
