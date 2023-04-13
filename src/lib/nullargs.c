static int pop_nullable(SLtype type, void **obj, void **meta)
{
   if (SLANG_NULL_TYPE == SLang_peek_at_stack ()) { 
	*obj = NULL;
	if (type && meta != NULL)
	   *meta = NULL;
	return SLang_pop_null ();
   }

   switch(type) {

	case 0:
	   return SLang_pop_cstruct(*obj, (SLang_CStruct_Field_Type *)meta);

	case SLANG_ANY_TYPE:
	   return SLang_pop_anytype((SLang_Any_Type**)obj);

	case SLANG_REF_TYPE:
	   return SLang_pop_ref((SLang_Ref_Type**)obj);

	case SLANG_STRING_TYPE:
	   return SLang_pop_slstring ((char**)obj);

	default:
	   /* This will fail, as intended, for non-opaque types */
	   return SLang_pop_opaque(type, meta, (Slirp_Opaque**)obj);
   }
}
#define pop_string_or_null(s)  pop_nullable(SLANG_STRING_TYPE,(void**)s,NULL)
#define pop_ref_or_null(r)  pop_nullable(SLANG_REF_TYPE,(void**)&r->ref,r->data)
#define pop_anytype_or_null(a) pop_nullable(SLANG_ANY_TYPE,(void**)a,NULL)
