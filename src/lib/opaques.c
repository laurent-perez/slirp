BEGIN_DECLS				/* Opaque handling code {{{ */

#define SLIRP_ABI_VERSION			10500

typedef void            (*FINALIZER)            (void*);
typedef void            (*INITIALIZER)          (void*);

typedef struct _Slirp_Type {
   SLang_Class_Type	*slclass;	/* SLang type class for this type */
   struct _Slirp_Type	*parent;	/* NULL if type has no ancestors  */
   FINALIZER		finalizer;
   INITIALIZER		initializer;
} Slirp_Type;

typedef struct _Slirp_Opaque {
   Slirp_Type *type;
   SLang_MMT_Type *mmt;
   void* instance;
   int deletion_in_progress;
   int free_me_too;		       /* if non-zero, free MMT, then struct */
   int owns_ref;		       /* if non-zero, do not free instance */
} Slirp_Opaque;

static void SLang_free_opaque (Slirp_Opaque *o)
{
   SLang_MMT_Type *mmt;

   if ((o == NULL)
       || (o->deletion_in_progress))
     return;

   mmt = o->mmt;
   if (mmt == NULL)
     return;

   o->mmt = NULL;
   o->deletion_in_progress = 1;
   SLang_free_mmt (mmt);
   o->deletion_in_progress = 0;
   if (o->free_me_too)
     SLfree ((char *) o);
}


static int opaque_typecast (SLtype from_type, VOID_STAR from_p,
			unsigned int num, SLtype to_type, VOID_STAR to_p)
{
   unsigned int i;
   SLang_MMT_Type **from = (SLang_MMT_Type**)from_p;
   SLang_MMT_Type **to   = (SLang_MMT_Type**)to_p;

   (void) from_type; (void) to_type; 

   for (i=0; i < num; i++) {
	to[i] = from[i];	/* FIXME: typeof() still reports prev type */
	if (from[i] != NULL)
	   SLang_inc_mmt(from[i]);
   }

   return 1;
}

static int opaque_search(const void* key, const void *elem)
{
   return *(SLtype*)key - SLclass_get_class_id((*(Slirp_Type**)elem)->slclass);
}

static void destroy_opaque(SLtype type, VOID_STAR w)
{
   Slirp_Opaque *ot = (Slirp_Opaque*)w;

   (void) type;

   if ((ot->type != NULL)
       && (ot->type->finalizer != NULL)
       && (ot->instance != NULL))
     {
	if (ot->owns_ref == 0)
	  (*ot->type->finalizer) (ot->instance);
	ot->instance = NULL;
     }

   if (ot->deletion_in_progress)
     {
	/* Evidently, the MMT container is being destroyed from SLang_free_mmt.
	 * Let it perform the final free.
	 */
	ot->free_me_too = 1;
	return;
     }
   SLfree( (char*)ot );
}
END_DECLS

SLIRP_EXTERN SLtype void_ptr_Type;	/* These opaque pointer types must */
SLIRP_EXTERN SLtype string_ptr_Type;	/* be kept in sync with opaques.c  */
SLIRP_EXTERN SLtype uchar_ptr_Type;
SLIRP_EXTERN SLtype short_ptr_Type;
SLIRP_EXTERN SLtype ushort_ptr_Type;
SLIRP_EXTERN SLtype int_ptr_Type;
SLIRP_EXTERN SLtype uint_ptr_Type;
SLIRP_EXTERN SLtype long_ptr_Type;
SLIRP_EXTERN SLtype ulong_ptr_Type;
SLIRP_EXTERN SLtype float_ptr_Type;
SLIRP_EXTERN SLtype double_ptr_Type;
SLIRP_EXTERN SLtype opaque_ptr_Type;
SLIRP_EXTERN SLtype file_ptr_Type;

typedef struct _Reserved_Opaque_Type {
   char	  *name;
   SLtype *type;		/* SLtype of opaque pointer defined by this */
   SLtype masked_type;		/* SLtype of datum to which this points */
} Reserved_Opaque_Type;

SLIRP_EXTERN Slirp_Type	**Slirp_Opaque_Types;
SLIRP_EXTERN SLtype 	Slirp_Num_Opaque_Types;
SLIRP_EXTERN SLtype	First_Opaque_Type;
SLIRP_EXTERN SLtype	Last_Reserved_Opaque_Type;

static Reserved_Opaque_Type Reserved_Opaque_Types[] = {

   { (char*)"void_ptr",		&void_ptr_Type,		SLANG_ANY_TYPE,    },
   { (char*)"int_ptr",		&int_ptr_Type,		SLANG_INT_TYPE,    },
   { (char*)"double_ptr",	&double_ptr_Type,	SLANG_DOUBLE_TYPE, },
   { (char*)"opaque_ptr",	&opaque_ptr_Type,	SLANG_ANY_TYPE,	   },
   { (char*)"file_ptr",		&file_ptr_Type,		SLANG_FILE_PTR_TYPE,},
   { (char*)"float_ptr",	&float_ptr_Type,	SLANG_FLOAT_TYPE,  },
   { (char*)"long_ptr",		&long_ptr_Type,		SLANG_LONG_TYPE,   },
   { (char*)"string_ptr",	&string_ptr_Type,	SLANG_STRING_TYPE, },
   { (char*)"uint_ptr",		&uint_ptr_Type,		SLANG_UINT_TYPE,   },
   { (char*)"short_ptr",	&short_ptr_Type,	SLANG_SHORT_TYPE,  },
   { (char*)"ulong_ptr",	&ulong_ptr_Type,	SLANG_ULONG_TYPE,  },
   { (char*)"ushort_ptr",	&ushort_ptr_Type,	SLANG_USHORT_TYPE, },
   { (char*)"uchar_ptr",	&uchar_ptr_Type,	SLANG_UCHAR_TYPE,  },
   { NULL, NULL, 0, }
};

#define NUM_RESERVED_OPAQUES \
	sizeof(Reserved_Opaque_Types) / sizeof(Reserved_Opaque_Type) - 1

static Slirp_Type* sltype_to_slirp_type(SLtype sltype)
{
   Slirp_Type **ot = (Slirp_Type**) bsearch( (const void*) &sltype,
	 		(const void*) Slirp_Opaque_Types,
			(size_t) Slirp_Num_Opaque_Types,
			sizeof(Slirp_Type*), opaque_search);

   if (ot != NULL) return *ot;
   return NULL;
}

static SLang_MMT_Type*
create_opaque_mmt(SLtype type, void *instance, unsigned int owns_ref)
{
   SLang_MMT_Type *mmt = NULL;
   Slirp_Opaque *ot = (Slirp_Opaque*) SLcalloc(1, sizeof(Slirp_Opaque));

   if (ot != NULL) {

	ot->instance = instance;
	ot->owns_ref = owns_ref;
	ot->mmt      = NULL;

	if ( (ot->type = sltype_to_slirp_type(type)) != NULL) {

	   if (ot->type->initializer != NULL)
		(*ot->type->initializer) (ot->instance);

	   mmt = SLang_create_mmt (type, (VOID_STAR) ot);
	}

	if (mmt == NULL)
	   SLfree((char*)ot);
   }

   return mmt;
}

static int SLang_push_opaque(SLtype type, void *instance, unsigned int owns_ref)
{
   SLang_MMT_Type *mmt;

   if (instance == NULL)
      return SLang_push_null();

   mmt = create_opaque_mmt(type, instance, owns_ref);
   if (NULL == mmt)
      return -1;

   if (-1 == SLang_push_mmt (mmt))
     {
	SLang_free_mmt (mmt);
	return -1;
     }
   return 0;
}

static int SLang_pop_opaque (SLtype type, void **instance, Slirp_Opaque **o)
{
   Slirp_Opaque *ot;
   SLang_MMT_Type *mmt = NULL;

   if (instance != NULL) *instance = NULL;
   *o = NULL;

   if ((type == file_ptr_Type)
       && (SLang_peek_at_stack() == SLANG_FILE_PTR_TYPE))
     {
	FILE *fp;
	if (-1 == SLang_pop_fileptr(&mmt, &fp))
	  return -1;

	ot = (Slirp_Opaque*) SLcalloc(1, sizeof(Slirp_Opaque));
	if (ot == NULL)
	  {
	     SLang_free_mmt(mmt);
	     return -1;
	  }
	ot->mmt = mmt;
	ot->free_me_too = 1;
	*instance = (void*)fp;
	*o = ot;
	return 0;
   }

   if (NULL == (mmt = SLang_pop_mmt (type)))
     return -1;

   if (NULL == (ot = (Slirp_Opaque*) SLang_object_from_mmt (mmt))
       || (ot->instance == NULL))
     {
	SLang_verror (SL_INVALID_PARM, "%s", "slirp: pop_opaque: instance=NULL");
	SLang_free_mmt (mmt);
	return -1;
     }

   /* This creates a circular reference:
    *    mmt->ot->mmt
    */
   ot->mmt = mmt;
   if (instance != NULL)
     *instance = ot->instance;
   *o = ot;
   return 0;
}

static unsigned int
allocate_opaque(char *name, FINALIZER fzer, INITIALIZER izer, SLtype parent_id,
		int (*sget)(SLtype, char *), int (*sput)(SLtype, char *))
{
   /* Returning from here with anything but a positive id is fatal */
   SLtype new_type, ancestor_type;
   Slirp_Type *type , *parent = NULL;
   SLang_Class_Type *new_class = SLclass_allocate_class (name);

   if (new_class == NULL) return SLANG_UNDEFINED_TYPE;

   (void) SLclass_set_destroy_function (new_class, destroy_opaque);
   if (-1 == SLclass_register_class (new_class, SLANG_VOID_TYPE,
				     sizeof(Slirp_Opaque),
				     SLANG_CLASS_TYPE_MMT))
	return SLANG_UNDEFINED_TYPE;

   new_type = SLclass_get_class_id(new_class);
   if (parent_id) {

	type = parent = sltype_to_slirp_type(parent_id);

	while (type) {

	   ancestor_type = SLclass_get_class_id(type->slclass);

	   /* Support downcasting ancestor types to this type */
	   if (-1 == SLclass_add_typecast (ancestor_type, new_type,
		    					opaque_typecast, 1))
		return SLANG_UNDEFINED_TYPE;

	   /* Support upcasting this type to each ancestor type */
	   if (-1 == SLclass_add_typecast (new_type, ancestor_type,
							opaque_typecast, 1))
		return SLANG_UNDEFINED_TYPE;

	   type = type->parent;
	}
   }

   if ( (sget != NULL && SLclass_set_sget_function(new_class, sget) == -1) ||
	(sput != NULL && SLclass_set_sput_function(new_class, sput) == -1))
	return SLANG_UNDEFINED_TYPE;
	
   if ( (type = (Slirp_Type*) SLmalloc(sizeof(Slirp_Type))) == NULL)
	return SLANG_UNDEFINED_TYPE;

   type->slclass = new_class;
   type->parent = parent;
   type->finalizer = fzer;
   type->initializer = izer;
   Slirp_Opaque_Types[ Slirp_Num_Opaque_Types++ ] = type;

   return new_type;
}

static int allocate_reserved_opaque_types(void)
{
   Reserved_Opaque_Type *pt;
   long abi_version = -1;

   if ( SLang_is_defined((char*)"_slirp_initialized")) {

	if ( SLang_is_defined((char*)"_slirp_abi_version") == 0  ||
		SLang_load_string((char*)"_slirp_abi_version;") == -1 ||
		SLang_pop_long(&abi_version) == -1 ||
		abi_version != SLIRP_ABI_VERSION)
	{
	   SLang_verror(SL_APPLICATION_ERROR,
		(char*) "SLIRP abi mismatch: want version %ld, have %ld\n",
		(long)SLIRP_ABI_VERSION, abi_version);
	   return -1;
	}

	return 0;
   }

   (void)SLang_load_string((char*)"public variable _slirp_initialized=1;");
   (void)SLang_push_int(SLIRP_ABI_VERSION);
   (void)SLang_load_string((char*)"public variable _slirp_abi_version=();");

   Slirp_Num_Opaque_Types = 0;

   if ( NULL == (Slirp_Opaque_Types = (Slirp_Type**) SLmalloc
			( sizeof(Slirp_Type*) * (NUM_RESERVED_OPAQUES + 1))))
      return -1;

   Slirp_Opaque_Types[0] = NULL;

   pt = Reserved_Opaque_Types;
   while (pt->name != NULL) {
	if ((*pt->type = allocate_opaque(pt->name, NULL,NULL,0,NULL,NULL)) ==
	      						SLANG_UNDEFINED_TYPE)
		return -1;
	   pt++;
	}

   /* Loop again, to enable casts to/from generic pointer type */
   pt = Reserved_Opaque_Types + 1;
   while (pt->name != NULL) {

	if (-1 == SLclass_add_typecast (*pt->type, void_ptr_Type,
							opaque_typecast, 1))
	   return SLANG_UNDEFINED_TYPE;

	if (-1 == SLclass_add_typecast (void_ptr_Type, *pt->type,
							opaque_typecast, 1))
	   return SLANG_UNDEFINED_TYPE;

	pt++;
   }

   First_Opaque_Type = *Reserved_Opaque_Types[0].type;
   Last_Reserved_Opaque_Type = First_Opaque_Type + NUM_RESERVED_OPAQUES - 1;

   return 0;
} /* }}} */
