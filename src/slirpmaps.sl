%  slirpmaps.sl:  Map types/functions from C/FORTRAN to S-Lang {{{
%
%  This file is part of SLIRP, the (Sl)ang (I)nte(r)face (P)ackage.
%
%  Copyright (c) 2003-2009 Massachusetts Institute of Technology
%  Copyright (C) 2002 Michael S. Noble <mnoble@space.mit.edu>
%
%  This software was partially developed by the MIT Center for Space
%  Research under contract SV1-61010 from the Smithsonian Institution.
%  
%  Permission to use, copy, modify, distribute, and sell this software
%  and its documentation for any purpose is hereby granted without fee,
%  provided that the above copyright notice appear in all copies and
%  that both that copyright notice and this permission notice appear in
%  the supporting documentation, and that the name of the Massachusetts
%  Institute of Technology not be used in advertising or publicity
%  pertaining to distribution of the software without specific, written
%  prior permission.  The Massachusetts Institute of Technology makes
%  no representations about the suitability of this software for any
%  purpose.  It is provided "as is" without express or implied warranty.
%  
%  THE MASSACHUSETTS INSTITUTE OF TECHNOLOGY DISCLAIMS ALL WARRANTIES
%  WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
%  MERCHANTABILITY AND FITNESS.  IN NO EVENT SHALL THE MASSACHUSETTS
%  INSTITUTE OF TECHNOLOGY BE LIABLE FOR ANY SPECIAL, INDIRECT OR
%  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
%  OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
%  WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. }}}

require("slirputils");

%  Front matter: core data structures, forward declarations, etc {{{

variable TYPECL_OPAQUE  = 0;
variable TYPECL_SCALAR  = 1;
variable TYPECL_VECTOR  = 2;
variable TYPECL_POINTER = 3;
variable INLINED = '-' ;

typedef struct {
   type,		% type name in underlying C/FORTRAN layer
   name,		% placeholder argname used in prototype (may be empty)
   dim,			% dimensionality: scalar=0, double*=1, double**=2, ...
   arrayspec,		% for multi-dimensional array types: <type> name[size]
   nelems,		% how many elements in multi-dimensional array?
   cast,
   layout,		% array layout in mem: C or FORTRAN (row- or /col-major)

   clone,
   ltype,		% type name of helpers in local/wrapper scope
   lname,		% variable name of instantiation of type in wrapper
   deref_type,		% for pointer types: ltype with one "*" removed

   marshal,		% indicates whether wrapper will expect an instantiated
			% arg of this type to be passed in from S-Lang scope
   proxy,		% indicates marshaling will occur through a proxy
   vectorized,

   defval,		% default value which an arg of this type may take
   nullable,		% may instance take on S-Lang NULL value?
   cpp_ref,		% will instance have C++ reference semantics?

   sltype,		% type name in S-Lang scope
   typeclass,		% S-Lang class type (scalar, vector, ptr, opaque/mmt)
   typeid,		% S-Lang SLtype identifier (usually a #define) for type
   const,
   parent,		% parent type, if any (e.g. for opaque hierarchies)
   subtypes,		% array of additional SLIRP subtypes
   mnemonic,		% to unify push/pop methods (also the default usage)
   usage,
   finalizer,		% finalizes wrapped type (e.g. free mem) @destroy time
   initializer,		% initializes wrapped type @create time

   sput,		% field get/set mechanisms (for aggregate types)
   sget,

   external,		% indicates whether type def'd w/in an included rc file

   aux,			% arbitrary information associated with type

   declarer,		% generator 4 declaring instances w/in wrappers
   marshaler,		% generator 4 marshaling instances from slang to wrapper
   marshaler_args,      % Additional info needed by the marshaler, if any
   referer,		% generator to insert args within either the wrapper
			% prototype or call to underlying wrapped function
   returner,		% generator 4 returning instances back to S-Lang scope
   cleaner,		% generator to free/deallocate helpers w/in wrapper
   cleaner_expr,	% expression to use to free/deallocate the arg
   retstruct            % function can return a structure
} TypeMapping;

typedef struct {
   name,		% name of function as prototyped in C/C++/FORTRAN
   slname,		% name of function as it will be called from S-Lang
   gname,		% name of generated func (wrapper or stub)
   args,		% typemaps for args given w/in prototyped function
   nargs,		% how many of them
   retval,		% typemap for the return value of prototyped function
   language,
   local_vars,		% additional variables to declare w/in emitted wrapper
   pop_args,		% will wrapper explicitly pop its args?
   retmap,		% annotation applied to return value (if any)
   argmaps,		% annotation(s) applied to function args (if any)
   arglist_patterns,	% longest-->shortest permutations of arguments
   inlined,		% was function/method declared inline?
   overloads,		% funcs with same name, but another entry point &| args
   class,		% parent class of this function/method (C++ only)

   sizer,		% returns dimensionality info about this funcs args
   referer,		% governs how func will be referred to at call time
   referer_hook,	% way of overriding referer with custom handling
   prototype_hook,	% allows additional content to be generated just prior
			% to prototype/definition of wrapper for this func
   data,		% stores arbitrary info / data
   vectorized
} FuncMapping;

typedef struct {
   method,
   pattern,		% the argument list

   code,		% C code fragment to insert w/in generated wrapper
   args,
   nargs,		% how many prototype arguments map to this argmap
   argnum,		% identifies ordinality of argument w/in wrapped func
   which,		% identifies the primary of a multi-argument map
   local_vars,		% extra local variables declared within argmap block
   usage,		% optional doc string indicating how arg is used
   proxy,
   funcmap		% descriptor for parent function of argmap
} ArgMapping;

variable AM_Ignore			= "ignore";
variable AM_Setup			= "setup";
variable AM_In				= "in";
variable AM_Init			= "init";
variable AM_Out				= "out";
variable AM_Final			= "final";
variable AM_Ret				= "return";
variable _retmaps			= Empty_Assoc_Array;
variable _argmaps			= Empty_Assoc_Array;
_argmaps[AM_Ignore]			= Empty_Assoc_Array;
_argmaps[AM_In]				= Empty_Assoc_Array;
_argmaps[AM_Final]			= Empty_Assoc_Array;
_argmaps[AM_Init]			= Empty_Assoc_Array;
_argmaps[AM_Out]			= Empty_Assoc_Array;
_argmaps[AM_Setup]			= Empty_Assoc_Array;

variable SLIRP_OPAQUE_TYPE_MNEMONIC      = "opaque";
variable SLIRP_OPAQUE_TYPE_NAME		 = "Slirp_Opaque*";
variable SLIRP_OPAQUE_DEFAULT_SLANG_NAME = "void_ptr";
variable SLIRP_OPAQUE_DEFAULT_SLANG_ID   = "void_ptr_Type";
variable SLirp_Opaque_Type               = SLIRP_OPAQUE_TYPE_NAME;
variable SLang_Struct_Type               = "SLang_Struct_Type*";
variable SLFile_FD_Type			 = "SLFile_FD_Type*";
variable SLang_String_Type               = "char*";
variable SLang_Ref_Type                  = "SLang_Ref_Type*";
variable SLang_Array_Type                = "SLang_Array_Type*";
variable SLang_Any_Type			 = "SLang_Any_Type*";
variable Posix_File_Ptr_Type		 = "FILE*";

variable DeReferer			= [ EMPTY, "*" ];
variable Referer			= [ EMPTY, "&" ];

variable ignored			= Assoc_Type[UChar_Type, 0];
variable try_vectorize			= Assoc_Type[UChar_Type, 0];
variable dont_vectorize			= Assoc_Type[UChar_Type, 0];
variable parse_file			= NULL;

private variable Include_Nesting = 0;
private variable Include_Files = String_Type[0];

define parse_func_decl();		% forward declarations
private define funcmap_process_args();
private define default_type_clone();
% }}}

%  Type handling (type registration & util funcs) {{{

variable ignored_variables = String_Type[0];

% Type-specific declare/marshal/etc callback functions {{{

private define default_declarer(arg) % Defaults {{{ 
{
   return sprintf ("   %s %s;\n",arg.type, arg.lname);
}

private define default_cleaner(arg, argno) {return NULL;}

private define default_marshaler(arg, argno)
{
  if (arg.nullable)
	sprintf("pop_nullable(%s, (void**)&%s, NULL)",arg.typeid,arg.lname);
  else if (arg.defval != EMPTY) {
	sprintf("pop_defaultable(%d, %s, %s, %s, %s%S)", argno, arg.mnemonic,
			arg.ltype, arg.lname, arg.cast, arg.defval);
  }
  else
	sprintf("SLang_pop_%s((%s*)&%s)", arg.mnemonic, arg.ltype,arg.lname);
}

private define default_c_referer(arg, typed, ignored)
{
   if (typed) 
	strcat(arg.const, arg.type, " ", arg.lname);
   else
	arg.lname;
}

private define default_cfront_referer(arg, typed, ignored)
{
   if (typed)
	sprintf("%s%s %s", arg.ltype, DeReferer[arg.cpp_ref], arg.lname);
   else if (arg.cpp_ref)
	sprintf("(%s&) * %s", arg.type, arg.lname);
   else	
	sprintf("(%s) %s", arg.type, arg.lname);
}

private define default_returner(arg)
{   
   sprintf("(void)SLang_push_%s(%s)", arg.mnemonic, arg.lname);
}

private define c_arg_sizer(arg, what)
{
   if (arg.typeclass == TYPECL_POINTER) {
	SC.have_refs |= 0x4;
	sprintf("ref_get_size(%s_r, %S)", arg.lname, what);
   }
   else
	return "1";
}
% }}}

private define vectorized_declarer(arg) % Vectorized arguments {{{
{
   variable tmap = arg;
   while (tmap.dim) tmap = tmap.aux;	% find base type, if necessary

   sprintf ("   %s%s* %s;\n", arg.const, tmap.type, arg.lname)
     + sprintf ("   Slirp_Ref *%s_r = ref_new(%s,sizeof(%s),&%s,0x%d);\n",
		arg.lname, tmap.typeid, tmap.type, arg.lname, arg.layout);
}

private define vectorized_marshaler(arg, ignored)
{
  variable dim = arg.dim;
  if (dim && arg.aux.typeclass == TYPECL_VECTOR) dim--;
  sprintf("vec_pop( %s_r, %d, %d, &vs)", arg.lname, dim, arg.nelems);
}

private define vec_return(thing, rewind)
{
   variable tmap = thing;
   while (tmap.dim) tmap = tmap.aux;	% find base type, if necessary

   tmap = @tmap;
   tmap.lname = thing.lname;

   variable pusher = tmap.returner();
   variable matches = string_matches (pusher, "^([^)]+) *\([^ (]+\)"R);
   if (matches == NULL)
     abort ("Unable to extract the function name from %s", pusher);
   pusher = matches[1];
   return sprintf("VEC_RETURN(%s, %d, %s, %s, %d, %d)", 
		thing.lname, rewind, tmap.typeid, pusher,
		thing.typeclass == TYPECL_OPAQUE,
		thing.const == EMPTY);
}

private define vectorized_returner(arg) { vec_return(arg, 1); }
% }}}

private define opaque_declarer(arg) % {{{ 
{
   variable s = sprintf("   %s%s%s %s;\n", arg.const, arg.type, DeReferer[arg.cpp_ref],
			arg.lname);

   if (arg.marshal && arg.proxy == NULL)
     s += sprintf ("   %s %s_o = NULL;\n", SLIRP_OPAQUE_TYPE_NAME, arg.lname);
   return s;
}

private define opaque_marshaler(arg, argno)
{
   if (arg.nullable)
	sprintf("pop_nullable(%s, (void**)&%s_o, (void**)&%s)",
	      				arg.typeid, arg.lname, arg.lname);
   else
	sprintf("SLang_pop_opaque(%s, (void**)&%s, &%s_o)", arg.typeid,
					arg.lname, arg.lname);
}

define struct_returner (arg)
{   
   sprintf ("(void) SLang_push_cstruct ((VOID_STAR) %s, %s_Layout);\n",
	    arg.lname, arg.deref_type);
}

define opaque_referer(arg, typed, ignore)
{
   if (typed) 
	strcat(arg.const,arg.type, Referer[arg.cpp_ref]," ",arg.lname);
   else 
	strcat(DeReferer[arg.cpp_ref], arg.lname);
}

private define opaque_cleaner(arg, argno)
{
   return sprintf ("SLang_free_opaque(%s_o);\n", arg.lname);
}

private define opaque_returner(arg)
{
   sprintf("(void)SLang_push_opaque(%s, (void*)%s, 0)", arg.typeid, arg.lname);
}
% }}}

private define struct_declarer(arg) % {{{
{
   sprintf ("   %s %s = (%s) alloca(sizeof(%s));\n",
	    arg.type, arg.lname, arg.type, arg.deref_type);
}

private define struct_cleaner(arg, argno)
{
   return sprintf ("SLang_free_cstruct((VOID_STAR)%s, %s_Layout);\n",
		   arg.lname, arg.deref_type);
}

private define struct_marshaler(arg, argno)
{
   if (arg.nullable)
	sprintf("pop_nullable(0, (void**)&%s, (void**)%s_Layout)",
						arg.lname, arg.deref_type);
   else
       sprintf("SLang_pop_cstruct ( (VOID_STAR)%s, %s_Layout)",
						arg.lname, arg.deref_type);
}
% }}}

private define ref_declarer(arg) % Refs {{{
{
   variable s;
   if (arg.marshal)
     {
	s = sprintf ("   %s%s %s;\n", arg.const, arg.type, arg.lname);
	if (arg.proxy == NULL)
	  s += sprintf ("   Slirp_Ref *%s_r = ref_new(%s,sizeof(%s),&%s,0x%d);\n",
			arg.lname, arg.aux.typeid, arg.aux.type, arg.lname, arg.layout);
   }
   else
     s = sprintf ("   %s %s%s;\n", arg.deref_type, arg.lname, arg.arrayspec);
   return s;
}

private define ref_returner(arg)
{
   return sprintf("(void)SLang_push_%s(%s)", arg.aux.mnemonic, arg.lname);
}

private define string_cleaner(arg, argno)
{
   return sprintf("%sSLang_free_slstring(%s);\n",
		  (arg.defval == EMPTY) ? "" : sprintf ("if (SLang_Num_Function_Args > %d) ", argno),
		  arg.lname);
}

private define string_returner(arg)
{
   variable const = SC.assume_const_strings || arg.const != EMPTY;
   if (const) {
	variable pusher = "SLang_push_string";
	variable cast = arg.cast;
   }
   else {
	pusher = "SLang_push_malloced_string";	% This frees string after
	cast = EMPTY;				% it's pushed onto stack
   }

   sprintf("(void)%s(%s%s)", pusher, cast, arg.lname);
}

private define array_cleaner(arg, argno)
{
   return sprintf("SLang_free_array(%s);\n", arg.lname);
}

private define anytype_cleaner(arg, argno)
{
   return sprintf("SLang_free_anytype(%s);\n", arg.lname);
}

private define ref_marshaler(arg, argno)
{
   if (arg.defval == EMPTY)	% Note that only NULL may be used as a default,
	argno = 0;		% and that pop_defaultable is not employed
   sprintf("pop_array_or_ref( %s_r, 0x%d, %d)", arg.lname, arg.nullable, argno);
}

private define c_ref_referer(arg, typed, dummy)
{
   if (typed)
      default_c_referer (arg, typed, dummy);
   else if (arg.marshal || arg.arrayspec != EMPTY)
	arg.cast + arg.lname;
   else
	strcat("&",arg.lname);
}

private define cfront_ref_referer(arg, typed, dummy)
{
   if (typed)
	sprintf("%s%s %s", arg.type, DeReferer[arg.cpp_ref], arg.lname);
   else if (arg.marshal || arg.arrayspec != EMPTY)
	arg.cast + arg.lname;
   else
	strcat("&",arg.lname);
}
% }}}

private define fd_marshaler(arg, argno) % File descriptors and pointers {{{
{
   sprintf("SLfile_pop_fd(&%s)", arg.lname);
}

private define fd_cleaner(arg, argno)
{
   return sprintf ("SLfile_free_fd(%s);\n", arg.lname);
}

private define fp_cleaner(arg, argno)
{
   %emit("   SLang_free_opaque(%s_o); SLfree((char*) %s_o);\n", arg.lname, arg.lname);
   return sprintf ("SLang_free_opaque(%s_o);\n", arg.lname);
}
% }}}

private define c_scalar_referer(arg, typed, arg_was_explicitly_popped)
{
   variable type;
   if (typed) type = arg.type; else type = EMPTY;
   if (arg_was_explicitly_popped)
      strcat(type," ", arg.lname);
   else
      strcat(type,"* ", arg.lname);
}

private variable default_referer = &default_c_referer;
private variable scalar_referer  = &c_scalar_referer;
private variable ref_referer_func     = &c_ref_referer;
define init_cfront()
{
   default_referer = &default_cfront_referer;
   scalar_referer  = &default_cfront_referer;
   ref_referer_func     = &cfront_ref_referer;
}

private define genstubs_referer(arg, typed, ignored)
{
   strcat(arg.const, arg.type, Referer[arg.cpp_ref]," ", arg.lname);
}

define init_genstubs()
{
   default_referer = &genstubs_referer;
   scalar_referer  = &genstubs_referer;
}

private define assign_typed_callbacks(tmap)  % {{{
{
   switch (tmap.typeclass)
   { case TYPECL_SCALAR: tmap.referer = scalar_referer; }
   { case TYPECL_POINTER:

	(tmap.deref_type, ) = strreplace(tmap.type, "*", EMPTY, -1);

	switch(tmap.ltype)
	{ case SLang_String_Type:
		if (tmap.type != "string")
		   tmap.cleaner  = &string_cleaner;
	   	tmap.returner = &string_returner;
	}
	{ case SLang_Ref_Type: 
	   tmap.declarer = &ref_declarer;
	   tmap.marshaler = &ref_marshaler;
	   tmap.referer = ref_referer_func;
	   tmap.returner = &ref_returner;
	}
	{ case SLang_Array_Type: tmap.cleaner = &array_cleaner; }
	{ case SLang_Any_Type: tmap.cleaner = &anytype_cleaner; }
	{ case SLang_Struct_Type:
	   if (tmap.deref_type == tmap.type)
	      abort("Struct mapping requires pointer type, not: %s", tmap.type);
	   tmap.declarer = &struct_declarer;
	   tmap.marshaler = &struct_marshaler;
	   tmap.cleaner = &struct_cleaner;
	}
        { case SLFile_FD_Type: 
		tmap.marshaler = &fd_marshaler;
		tmap.cleaner = &fd_cleaner;
	}
   }
   { case TYPECL_OPAQUE:

	if (tmap.type != NULL)
	   (tmap.deref_type, ) = strreplace(tmap.type, "*", EMPTY, -1);

      	tmap.declarer = &opaque_declarer;
      	tmap.marshaler = &opaque_marshaler;
      	tmap.referer = &opaque_referer;
      	tmap.returner = &opaque_returner;
	if (tmap.type == Posix_File_Ptr_Type)
	   tmap.cleaner = &fp_cleaner;
	else
	   tmap.cleaner = &opaque_cleaner;
   }

   if (tmap.declarer  == NULL) tmap.declarer =  &default_declarer;
   if (tmap.marshaler == NULL) tmap.marshaler = &default_marshaler;
   if (tmap.referer   == NULL) tmap.referer = default_referer;
   if (tmap.returner  == NULL) tmap.returner =  &default_returner;
   if (tmap.cleaner   == NULL) tmap.cleaner = &default_cleaner;
} % }}}
% }}}

% Meta functions {{{

private define typemap_new(type, ltype, mnemonic, typeid,
					typeclass, finalizer, initializer)
{
   % Note that several fields are intentionally left NULL
   variable tmap    = @TypeMapping;
   tmap.type        = type;		% sltype is left null b/c only opaque
   tmap.ltype       = ltype;		% mappings define new slang types
   tmap.layout	    = ROW_MAJOR;	% use C array ordering, by default
   tmap.mnemonic    = mnemonic;
   tmap.typeid      = typeid;		% aux may be any type-specific info
   tmap.typeclass   = typeclass;
   tmap.finalizer   = finalizer;
   tmap.sput	    = "NULL";
   tmap.sget	    = "NULL";
   tmap.initializer = initializer;
   tmap.const	    = EMPTY;
   tmap.cast	    = EMPTY;
   tmap.external    = Include_Nesting;
   tmap.marshal	    = 1;		% default: marshal all S-Lang args to C
   tmap.cpp_ref     = 0;
   tmap.dim 	    = 0;
   tmap.nelems	    = 0;
   tmap.vectorized  = 0;
   tmap.arrayspec   = EMPTY;
   tmap.defval	    = EMPTY;
   tmap.clone	    = &default_type_clone;

   assign_typed_callbacks(tmap);

   return tmap;
}

define slirp_map_unsupported(type) { error("unmapped type: " + type); }

define slirp_map()
{
   variable type, ltype, mnemonic, id, class, final = NULL, init = NULL;

   switch(_NARGS)
   { case 5: (type,ltype,mnemonic,id,class) = (); }
   { case 6: (type,ltype,mnemonic,id,class,final) = (); }
   { case 7: (type,ltype,mnemonic,id,class,final, init) = (); }
   { usage("slirp_map (type, ltype, mnemonic, typeid, typeclass " +
					"[, finalizer [, initializer] ])"); }

   SC.types[type] = typemap_new(type,ltype,mnemonic,id,class,final,init);
}

define slirp_map_ref(type)
{
   variable tmap = SC.types[type[ [:-2]]];	% ref types must have at least
   if (tmap == NULL)				% 1 level of ptr indirection
      error("Attempting to define ref type <" + type +
					"> for an undefined base type!");

   variable refmap = typemap_new(type, SLang_Ref_Type, tmap.mnemonic,
				"SLANG_REF_TYPE", TYPECL_POINTER, NULL, NULL);

   refmap.sltype = tmap.sltype;		% NULL, except for refs to opaque
   refmap.aux = tmap;			% remember typemap of ref'd type
   refmap.dim = tmap.dim + 1;
   if (refmap.dim > 1)
      refmap.mnemonic += "_ptr";
   SC.types[type] = refmap;
}

define slirp_map_cpp_ref(type)
{
   % C++ references: scalar syntax with pointer semantics
   variable tmap = SC.types[type];
   if (tmap == NULL)
      error("Attempting to define C++ ref type <" + type +
					"& > for an undefined base type!");
   tmap = @tmap;
   tmap.type = type;
   tmap.cpp_ref = 1;
   SC.types[ type +"&" ] = tmap;
}
% }}}

define slirp_map_void(type) % {{{
{
   slirp_map(type,VOID,VOID,VOID,TYPECL_POINTER);
}; % }}}

#if (SLIRP_HAVE_FORTRAN) % {{{
define slirp_map_scomplex(t)
{
   slirp_map(t,"spcomplex","scomplex","SLANG_COMPLEX_TYPE",TYPECL_VECTOR);
   slirp_map_ref(t+"*");
}  

define slirp_map_dcomplex(t)
{
   slirp_map(t,"dpcomplex","dcomplex","SLANG_COMPLEX_TYPE",TYPECL_VECTOR);
   slirp_map_ref(t+"*");
}  
#endif % }}}

% Opaque type mapping interface  {{{

private define strip_unwanted_type_info(type) % {{{
{
   !if (strncmp(type, "struct ", 7))
	type = type[[7:]];
   else
	if (not strncmp(type, "unsigned ", 9))
	type = "u"+ type[[9:]];

   return type;
} % }}}

define slirp_map_opaque()
{
   variable type, sltype, parent = NULL;

   switch(_NARGS)
      { case 1 : type = (); sltype = SLIRP_OPAQUE_DEFAULT_SLANG_NAME; }
      { case 2 : (type, sltype) = (); }
      { usage("slirp_map_opaque(c_type_name [,slang_type_name]");}

   if (sltype != NULL)
	sltype = strip_unwanted_type_info(sltype);

   parent = SC.types[sltype];

   if (parent == NULL || parent.typeclass != TYPECL_OPAQUE)
      error("Attempting to map to non-existent or non-opaque type!");

   parent.subtypes = [ parent.subtypes, type ];
   variable tmap = typemap_new(type, parent.ltype, parent.mnemonic,
			parent.typeid, parent.typeclass, parent.finalizer,
			parent.initializer);

   tmap.sltype = parent.sltype;
   tmap.aux    = parent.aux;
   tmap.referer = parent.referer;
   SC.types[type] = tmap;

   slirp_map_ref(type+"*");	% Create ref mapping for arrays of this type
}

define slirp_set_opaque_default(sltype)
{
   SLIRP_OPAQUE_DEFAULT_SLANG_NAME = sltype;
   SLIRP_OPAQUE_DEFAULT_SLANG_ID   = sltype + "_Type";
}

define _define_opaque()
{
   variable sltype, finalizer, initializer, parent;
   switch(_NARGS)
	{ case 1 : (sltype) = (); parent = SLIRP_OPAQUE_DEFAULT_SLANG_NAME; }
	{ case 2 : (sltype,parent) = (); }
	{ case 3 : (sltype,parent,finalizer) = (); }
	{ case 4 : (sltype,parent,finalizer,initializer) = (); }
	{ usage("slirp_define_opaque(slang_name [, parent [, finalizer " +
						"[, initializer ] ] ])"); }

   sltype = strip_unwanted_type_info(sltype);

   if (parent != NULL) {
	variable p = parent;
	parent = SC.types[parent];
	if (parent == NULL || parent.typeclass != TYPECL_OPAQUE)
	   verror("Cannot use non-existing opaque type as parent: %S", p);
	parent.subtypes = [ parent.subtypes, sltype ];
   }

   % The finalizer field refers to the function that will be called at destroy
   % time (when the S-Lang object goes out of scope) to free the memory 
   % allocated to the C instance wrapped by the S-Lang opaque instance.
   % (note that many opaque type wrappers will not need explicit freeing).
   !if (__is_initialized(&finalizer)) {
	if (parent != NULL)
	   finalizer = parent.finalizer;
	else
	   finalizer = NULL;
   }

   % The initializer field refers to a function that will be called at
   % creation time (create_opaque_mmt) to perform custom initialization
   % of the C instance wrapped by the S-Lang opaque instance.
   % For example, the C instance may include a count of how many S-Lang
   % objects point to it, and the initializer could increment this count.
   % As is the case with finalizers, an initializer is not required.
   !if (__is_initialized(&initializer)) {
	if (parent != NULL)
	   initializer = parent.initializer;
	else
	   initializer = NULL;
   }

   % type == NULL means a "pure virtual" base type, not instantiable in S-Lang
   variable ot = typemap_new(NULL, SLirp_Opaque_Type, sltype, sltype + "_Type",
					TYPECL_OPAQUE, finalizer, initializer);
   ot.sltype   = sltype;
   ot.aux      = ot.typeid;
   ot.subtypes = String_Type[0];
   ot.parent   = parent;
   SC.types[sltype] = ot;
   SC.opaque_types = [ SC.opaque_types, ot ];

   return ot;
}

private define define_reserved_opaque(slang_type_name, parent)
{
   variable ot = _define_opaque(slang_type_name, parent);
   ot.external = 1;	% prevent allocation of type w/in generated C code
}

define define_reserved_opaques()
{
   % Reserved opaque types: pointers to various types (must be kept
   % in sync with opaques.c).  To keep generated code as short as
   % possible these will only be defined if needed within a module.
   define_reserved_opaque("void_ptr", NULL);
   define_reserved_opaque("int_ptr","void_ptr");
   define_reserved_opaque("double_ptr","void_ptr");
   define_reserved_opaque("opaque_ptr","void_ptr");
   define_reserved_opaque("file_ptr","void_ptr");
   define_reserved_opaque("float_ptr","void_ptr");
   define_reserved_opaque("long_ptr","void_ptr");
   define_reserved_opaque("string_ptr","void_ptr");
   define_reserved_opaque("uint_ptr","void_ptr");
   define_reserved_opaque("short_ptr","void_ptr");
   define_reserved_opaque("ulong_ptr","void_ptr");
   define_reserved_opaque("ushort_ptr","void_ptr");
   define_reserved_opaque("uchar_ptr","void_ptr");
   SC.num_reserved_types = length(SC.opaque_types);
}

define slirp_define_opaque()
{
   !if (SC.num_reserved_types)		% Define reserved opaque ptr types
	define_reserved_opaques();	% Doing it this way ensures that the
   variable args = __pop_args(_NARGS);	% absolute min # of LOC are generated
   () = _define_opaque(__push_args(args));
   variable sltype = args[0].value;

   % Conveniences:
   %	- each new opaque type becomes "default opaque type"
   %	- automatically create a mapping for pointers to each new opaque type
   slirp_set_opaque_default(sltype);
   slirp_map_opaque(sltype+"*",sltype);
   slirp_map_cpp_ref(sltype);
}
% }}}

define slirp_map_pointer() % {{{
{
   !if (SC.num_reserved_types)          % Ensure opaque pointer
	define_reserved_opaques();      % types are defined

   variable is_pointer, ctype, ptype;

   switch (_NARGS)
   { case 1:

      	ctype = ();
	(ptype, is_pointer) = strreplace(ctype, "*", "_ptr", 99);
	if (is_pointer && SC.types[ptype] == NULL)
	   () = _define_opaque(ptype, "void_ptr");
	else
	   ptype = "void_ptr";
	   
   }
   { case 2: (ctype, ptype) = (); }
   { usage("slirp_map_pointer(ctype, pointer_type)"); }

   slirp_map_opaque(ctype, ptype);
} % }}}

define slirp_map_string(type) % {{{
{
   variable tmap;
   tmap = typemap_new(type, SLang_String_Type, "string",
	 			"SLANG_STRING_TYPE", TYPECL_POINTER, , );
   SC.types[type] = tmap;
   tmap.cast = "(" + tmap.type + ")";
   tmap.aux = tmap;			% use string mnemonics, not byte/char
   slirp_map_ref(type+"*");
   slirp_map_cpp_ref(type);
} % }}}

% Primitive C type mapping interface {{{
define slirp_map_char(type)
{
   slirp_map(type, "char", "char", "SLANG_CHAR_TYPE", TYPECL_SCALAR);

   % Following C, <type>* is NOT treated as a reference to a single
   % char type, but rather as a string, so no ref map is made here

   slirp_map_cpp_ref(type);
}

private define map4(type,ctype,mnemonic,typeid,typecl)
{
   slirp_map(type, ctype, mnemonic, typeid, typecl);
   slirp_map_ref(type+"*");
   slirp_map_cpp_ref(type);
   slirp_map_cpp_ref(type+"*");
}

define slirp_map_uchar(type)
{
   map4(type, "unsigned char", "uchar","SLANG_UCHAR_TYPE", TYPECL_SCALAR);
}

define slirp_map_short(type)
{
   map4(type, "short", "short", "SLANG_SHORT_TYPE", TYPECL_SCALAR);
}

define slirp_map_ushort(type)
{
   map4(type,"unsigned short","ushort","SLANG_USHORT_TYPE",TYPECL_SCALAR);
}

define slirp_map_int(type)
{
   map4(type, "int", "int", "SLANG_INT_TYPE", TYPECL_SCALAR);
}

define slirp_map_uint(type)
{
   map4(type, "unsigned int", "uint","SLANG_UINT_TYPE", TYPECL_SCALAR);
}
define slirp_map_uinteger(type)	 { slirp_map_uint(type); }

define slirp_map_long(type)
{
   map4(type, "long", "long", "SLANG_LONG_TYPE", TYPECL_SCALAR);
}

define slirp_map_ulong(type)
{
   map4(type, "unsigned long", "ulong", "SLANG_ULONG_TYPE", TYPECL_SCALAR);
}

#if (SLIRP_HAVE_LONG_LONG)
define slirp_map_llong(type)
{
   map4(type, "long long", "long_long", "SLANG_LLONG_TYPE", TYPECL_SCALAR);
}

define slirp_map_ullong(type)
{
   map4(type, "unsigned long long", "ulong_long", "SLANG_ULLONG_TYPE",
	 							TYPECL_SCALAR);
}
#else
define slirp_map_llong(type) { slirp_map_unsupported(type); }
define slirp_map_ullong(type) { slirp_map_unsupported(type); }
#endif

define slirp_map_long_long(type)  { slirp_map_llong(type); }
define slirp_map_ulong_long(type) { slirp_map_ullong(type); }

define slirp_map_float(type)
{
   map4(type, "float", "float", "SLANG_FLOAT_TYPE", TYPECL_SCALAR);
}

define slirp_map_double(type)
{
   map4(type, "double", "double", "SLANG_DOUBLE_TYPE", TYPECL_SCALAR);
}

define slirp_map_ldouble(type)
{
   map4(type, "long double", "ldouble", "SLANG_LDOUBLE_TYPE", TYPECL_SCALAR);
}
% }}}

define slirp_map_struct(type) % {{{
{
   variable tmap = typemap_new(type, SLang_Struct_Type, "struct",
	 			"SLANG_STRUCT_TYPE", TYPECL_POINTER, , );
   SC.types[type] = tmap;
} % }}}

private define default_type_clone(typemap, clone) % {{{
{
   typemap = @typemap;
   SC.types[clone] = typemap;
   slirp_map_ref(clone + "*");
   return typemap;
} % }}}

define parse_typedef(t, assign_typedef_name_to_type) % {{{
{
   t = strtok( group_pointer_qualifiers_with_type( strtrim_end(t,";") ) );
   variable name = t[-1];

   if (ignored[name])			% avoid potentially redefining the
	return 0;			% type if it's already been mapped

   t = strjoin ( t[[:-2]], " ");
   t = SC.types[t];

   if (t == NULL || t.type == VOID)
	return 0;

   t = t.clone(name);

   if (assign_typedef_name_to_type)
	t.type = name;

   return 1;
} % }}}

define get_typemap(ctype, generate_new_typemap) % {{{
{
   variable const = EMPTY;

   ctype = strcompress(ctype," \t\r\n");
   % To save space vacuous "const" entries are not stored in typemap table,
   % so we peel off const qualifiers here and add them back @ declaration
   % Loop, so that all const qualifiers (at most 2) will be stripped
   forever {
   	if (string_match(ctype, "\\(.*\\)\\(\\<const\\>\\)\\(.+\\)", 1)) {
   	   ctype = strtrim(get_match(ctype, 1)) +  strtrim(get_match(ctype, 3));
   	   const = "const ";
   	}
   	else
   	   break;
   }
   variable tmap = SC.types[ctype];
   if (tmap == NULL) {

	if (ctype == Posix_File_Ptr_Type)
	   slirp_map_pointer(ctype, "file_ptr");
	else if (SC.types[ctype[[:-2]]] != NULL ||
				(generate_new_typemap && ctype != "..."))
	   slirp_map_pointer(ctype);

	tmap = SC.types[ctype];
   }

   if (tmap != NULL) {
	if (tmap.type == NULL)		% never match pure virtual base type
	   return NULL;

	tmap = @tmap;
	tmap.const = const;
   }

   return tmap;
} % }}}

define print_typemap() % {{{
{
   variable tmap = NULL, caller_info, name;
   switch(_NARGS)
   { case 1: tmap = (); caller_info = "unknown"; }
   { case 2: (tmap,caller_info) = (); }

   if (typeof(tmap) != TypeMapping) return;

   tprintf("");
   tprintf("TypeMap::%s", caller_info);

   foreach (get_struct_field_names (tmap)) {
	name = ();
	tprintf("%-12s =  %S", name, get_struct_field(tmap,name));
   }
   tprintf("");
} % }}}

private define parse_enum(fp, is_typedef) % {{{
{
   variable identifier = EMPTY;

   % some enum declarations may contain embeded comment, like this
   % example from gioenums.h
   % typedef enum /*< flags >*/ { 
   %   G_MOUNT_MOUNT_NONE = 0
   % } GMountMountFlags;
   % eat_comment (fp);

   !if (open_C_block(fp)) {
	identifier = get_token(fp);
   	!if (open_C_block(fp))
	   return;			% simple typedef of another enum
   }

   do {
	variable mnemonic = extract_enumerator(fp);

	if (mnemonic != EMPTY)
	{
	   if (ignored [mnemonic])
	     {
		warn_ignore (mnemonic, "in ignored list");
		continue;
	     }	   
	   add_const(SC.interface.int_consts, mnemonic, mnemonic);
	}
      
   } while ( more_enum_values(fp));

   close_C_block(fp);

   variable token = get_token(fp);
   if (token[-1] != ';')
      swallow_to(fp, ';');

   if (is_typedef) {
	token = strtrim_end(token,";");
	if (token == EMPTY)
	   token = identifier;
   }
   else if (identifier != EMPTY)
	token = strcat("enum ",identifier);
   else
	token = EMPTY;
   if (token != EMPTY) 
	slirp_map_int(token);
} % }}}

% Type abbbreviations (keep in sync with dispatch.c) % {{{
private variable TypeAbbrevs = Assoc_Type[String_Type, "o"];
TypeAbbrevs["SLANG_CHAR_TYPE"] = "b";
TypeAbbrevs["SLANG_UCHAR_TYPE"] = "B";
TypeAbbrevs["SLANG_SHORT_TYPE"] = "h";
TypeAbbrevs["SLANG_USHORT_TYPE"] = "H";
TypeAbbrevs["SLANG_INT_TYPE"] = "i";
TypeAbbrevs["SLANG_UINT_TYPE"] = "I";
TypeAbbrevs["SLANG_LONG_TYPE"] = "l";
TypeAbbrevs["SLANG_ULONG_TYPE"] = "L";
TypeAbbrevs["SLANG_FLOAT_TYPE"] = "f";
TypeAbbrevs["SLANG_DOUBLE_TYPE"] = "d";
TypeAbbrevs["SLANG_STRING_TYPE"] = "s";
TypeAbbrevs["SLANG_REF_TYPE"] = "R";
TypeAbbrevs["SLANG_ARRAY_TYPE"] = "R";

private variable PtrType_Abbrevs = Assoc_Type[String_Type, "o"];
PtrType_Abbrevs["b"] = "m";	% arrays of char-sized objects
PtrType_Abbrevs["d"] = "n";	% arrays of double-sized data
PtrType_Abbrevs["f"] = "p";	% arrays of float-sized data
PtrType_Abbrevs["h"] = "q";	% arrays of short-sized data
PtrType_Abbrevs["i"] = "r";	% arrays of int-sized data
PtrType_Abbrevs["l"] = "t";	% arrays of long-sized data
PtrType_Abbrevs["s"] = "u";	% arrays of char*-sized data

define sltype_abbrev(type) { return TypeAbbrevs[type]; }
define sltype_ptr_abbrev(type) { return PtrType_Abbrevs[strlow(type)]; }
% }}}
% }}}

%  Macro handling {{{
public define _slirp_macro_return_zero ()
{
   _pop_n (_NARGS);
   return 0;
}

variable _macros = Empty_Assoc_Array;
_macros ["extern"]   = EMPTY;
_macros ["volatile"] = EMPTY;

variable ignored_macros = String_Type[0];

private variable _macromaps = Empty_Assoc_Array; % {{{

% The _macromaps table can be used to transform #define macros
% into functions callable from S-Lang scope.  For example, a
%
%	_macromaps["GTK_WIDGET_STATE"] = "int (GtkWidget*)"
%
% mapping for the macro
%
%		#define GTK_WIDGET_STATE(w)
% 
% allows the latter to be called from S-Lang scripts as if it were
% a function prototyped as
%
%		int gtk_widget_state(GtkWidget *widget);
% }}}

private define do_macro_substitutions(input) % {{{
{
   variable out = String_Type[0], orig, new;

   if (typeof(input) == String_Type)
	input = strtok(input);

   foreach(input) {
	orig = ();
	forever {
	   new = _macros[ orig ];
	   switch (new)
	   { case NULL: new = orig; break; }
	   { case EMPTY: break; }
	   { orig = new; }
	}
	if (new != EMPTY)
	   out = [ out, new ];
   }

   return out;
} % }}}

define slirp_map_macro()	% {{{
{
   if (Include_Nesting) return;

   variable name, signature;
   switch(_NARGS)
   {  case 2: (name,signature) = (); }
   {  usage(_function_name + "(#define_macro_name,\"return_type (arglist)\"");}

   variable mmap = strtok(signature);		% split TYPE (ARGLIST)
   variable funcmap = parse_func_decl( sprintf("%s %s %s",
			mmap[0], name, strjoin(mmap[[1:]], " ")), 1, 1);

   if (funcmap == NULL)
      abort("Could not parse macro signature: %s(%s)", name, signature);

   funcmap.slname = strlow(funcmap.slname);
   _macromaps [ name ] = funcmap;
} % }}}

private define wrappable_macro(macro) % {{{
{
   return (_macromaps[macro] != NULL);
} % }}}
		   
private define constant_macro(name, value, attempt_to_resolve_later) % {{{
{
   % Determine if a given macro #defines a constant, of the general form
   %		#define <name> <value>

   if (is_substr(name, "("))		% parameter-ized --> not a constant
      return 0;

   % Strip balanced sets of enclosing parens
   while (string_match(value, "^(\\(.*\\))$",1))
	value = get_match(value, 1);

   value = strjoin (do_macro_substitutions(value), " ");
   variable consts = NULL, iface = SC.interface;

   switch( _slang_guess_type(value)) 

	{ case Integer_Type or case UInteger_Type: consts = iface.int_consts; }
	{ case Long_Type or case ULong_Type: consts = iface.long_consts; }
	{ case Double_Type or case Float_Type: consts = iface.double_consts; }
	{ case String_Type  :

		if (value[0] == '"' && value[-1] == '"')

		   consts = iface.string_consts;

		else if (string_match(value,"(\\d+[.]\\d*)",1) ||
			 string_match(value,"(\\d*[.]\\d+)",1) ||
			 % match those constants defined in pango-font.h
			 % #define PANGO_SCALE_X_LARGE  ((double)1.44)
			 string_match(value,"\(double\)[0-9.]+",1))

		   consts = iface.double_consts;

		else if (string_match(value,"([0-9]+)",1))

		   consts = iface.long_consts;

		else if (string_match(value, "^sizeof[ \t]*(", 1))

		   consts = iface.long_consts;
	}

   if (consts == NULL) {
	if (attempt_to_resolve_later)
	   iface.unresolved_macros[name] = value;
	return 0;
   }

   _macros[name] = value;		% ensure its detectable by #ifdef, etc
   add_const(consts, name, value);   
   return 1;
} % }}}

define determine_unresolved_macro_disposition(interface) % {{{
{
   variable iconsts = interface.int_consts; 
   variable lconsts = interface.long_consts; 
   variable dconsts = interface.double_consts;
   variable sconsts = interface.string_consts;
   variable ivars = interface.intrin_vars;
   variable pvars = interface.private_vars;
   variable table, name, value;

   foreach (interface.unresolved_macros) using ("keys","values")
     {
	(name, value) = ();

	if (value == "")
	  continue;

	table = NULL;

	% First, attempt to evaluate macro as a constant expression (SLang2 only)
	% Turn off the debugger for this try block.
	variable dbghook = _set_debug_hook (NULL);
	variable evalue;
	try
	  {
	     evalue = string (eval(value));
	  }
	catch AnyError: evalue = NULL;
	finally: () = _set_debug_hook (dbghook);

	if (evalue != NULL)
	  {
	     if (constant_macro (name, evalue, 0))
	       continue;
	  }

	% See if this #define is referencing other, known
	% macros, or a simple bitwise combination of them
	variable fields = strtok(value, " |&^~");
	variable n = length(fields) - 1;
	if (n < 0) continue;

	foreach ([iconsts, lconsts, dconsts, sconsts])
	  {
	     variable consts = ();
	     if (consts[fields[n]] != NULL)
	       {
		  % If this is a bitwise combo, all elements must be of same type
		  while (n > 0, n--)
		    {
		       if (consts[fields[n]] != NULL)
			 continue;

		       consts = NULL;
		       break;
		    }
		  table = consts;
		  break;
	       }
	  }

	% Finally: might macro reference an intrinsic or private variable?
	if (table == NULL)
	  {
	     variable varname = fields[-1];
	     variable is_pointer = value[0] == '&';

	     value = ivars[varname];
	     if (value == NULL)
	       {
		  value = pvars[varname];
		  if (value == NULL)
		    continue;
	       }
	     table = ivars;

	     if (is_pointer)
	       {
		  value = @SC.types[value.sltype + "*"];
		  value.name = varname;
	       }
	  }
	add_const(table, name, value);
     }
} % }}}

private define determine_slirp_macro_disposition() % {{{
{
   switch(_NARGS)
   {	case 2: variable value = (), name = ();
		value = strcompress(value," \t\r\n");
		name = strcompress(name," \t\r\n");
   }
   {	variable d = ();
	!if (string_match(d, "^[ \t]*\\([^ \t]+\\)[ \t]*\\([^ \t]*.*\\)", 1)) {
	   warn("ill-formed macro ignored: %S", strtrim(d));
	   return;
	}
	name = get_match(d, 1);
	value = get_match(d, 2);
   }

   if (is_substr(name, "(")) {
      warn("parameterized macro unsupported: %S",name);
      return;
   }

   !if (string_match(name,"\\C^\\([a-z_][a-z_0-9]*\\)",1)) {
      warn("illegal macro name %S", name);
      return;
   }

   name = get_match(name, 1);
   value = strtrim(extract_element(value, 0, '%'));

   if (_macros[name] != NULL)
	warn("macro redefined: %S", name);

   !if (constant_macro(name, value, 1))
	_macros[name] = value;
} % }}}

define slirp_define_macro() % {{{
{
   variable name, value;
   switch(_NARGS)
   {  case 1: name = (); value = EMPTY; }
   {  case 2: (name, value) = (); }
   {  usage(_function_name + ": (macro_name [,macro_value=\"\"])"); }

   determine_slirp_macro_disposition(name, value);
} % }}}

define slirp_substitute_macro()	% {{{ deprecated
{
   variable args = __pop_args(_NARGS);
   slirp_define_macro(__push_args(args));
} % }}}

private define macro_undefiner(name) % {{{
{
   !if (string_match(name, "\\C^[ \t]*\\([a-z_][a-z_0-9]*\\).*",1))
	abort("Invalid #undef directive: <%S>", strtrim(name));

   name = get_match(name, 1);
   assoc_delete_key(_macros, name);
   assoc_delete_key(SC.interface.int_consts, name);
   assoc_delete_key(SC.interface.long_consts, name);
   assoc_delete_key(SC.interface.double_consts, name);
   assoc_delete_key(SC.interface.string_consts, name);
}

() = preproc_handler_add("undef", EMPTY, &macro_undefiner);
() = preproc_handler_add("define", EMPTY, &determine_slirp_macro_disposition);
% }}}

private define prep_eval_conditional(fp) % {{{
{
   variable expr = {};
   variable last_token = NULL;

   forever {

	variable token = get_const_expr_token(fp);
	if (token == NULL) break;

	if (_slang_guess_type(token) == String_Type) {

	   switch (token)
		{ case "defined" :
			token = get_const_expr_token(fp);
			if (token == "(") {
			   token = get_const_expr_token(fp);
			   () = get_const_expr_token(fp);
			}
			token = string((_macros[token] != NULL));
		}
		{ case "!"  : token = "not"; }
		{ case "||" : token = "or";  }
		{ case "&&" : token = "and"; }
		{ case ">>" : token = "shr"; }
		{ case "<<" : token = "shl"; }
		{ case "^"  : token = "xor"; }
	        { case "%": token = "mod"; }
		{ case "?" or case ":" :

			% Using these in a #[el]if is highly questionable
			lprintf("Warning: operator " + token +
						" unsupported in this context");
			() = get_rest_of_line(fp);
			return 0;
		}
	     {
	      case "," :
		% valid construct in ex. glib-2.0/gobject/gtype.h
		% #if !defined (__cplusplus) && (G_GNUC_CHECK_VERSION(2, 7)) && !(defined (__APPLE__) && defined (__ppc64__))
		() = get_rest_of_line(fp);
		return 0;
	     }	   
		{
		   if ((token == "(") && (last_token == "0"))
		     expr[-1] = "_slirp_macro_return_zero";

		   ifnot (Legal_Const_Expr_Operator[token])
		     {
			token = _macros[token];
			if (token == NULL) token = "0";
			if ((token == "0") && (length(expr) == 0))
			  token = "_slirp_macro_return_zero";
		   }
		}
	}
      list_append (expr, token);
      last_token = token;
   }

   expr = strjoin(list_to_array (expr, String_Type ), " ");
   if (SC.debug)
     {
	dprintf ("  Calling eval with %s", expr);
     }

   return eval(expr);
} % }}}

define prep_get_token(fp) % {{{
{
   forever {

	variable token = get_token(fp), accept = SC.prep_accept[0];
     
      if (SC.eof)
	return EMPTY;
      variable str;
      foreach str (discard_line_strings)
	{
	   if (token == str)
	     {
		() = get_rest_of_line (fp);
		return EMPTY;
	     }	   
	}	 
      variable nesting = length(SC.prep_accept) - 1;

	switch(token)
	{  case "#ifdef" :
		token = get_token(fp);
		accept = accept and (_macros[token] != NULL);
	   	SC.prep_accept = [ accept, SC.prep_accept ];
	}
	{  case "#ifndef" :
		token = get_token(fp);	   
		accept = accept and (_macros[token] == NULL);
	   	SC.prep_accept = [ accept, SC.prep_accept ];
	}
	{  case "#if" :
	
		accept = accept and prep_eval_conditional(fp);
	   	SC.prep_accept = [ accept, SC.prep_accept ];
	}
	{  case "#elif" :

		!if (nesting) abort("Misplaced #elif");

		if (accept) {
		   accept = 0;
		   () = get_rest_of_line(fp);	% no need to evaluate
		}
		else {
		   accept = SC.prep_accept[1] and prep_eval_conditional(fp);
		   SC.prep_accept[0] = accept;
		}
	}
	{  case "#else" :

		!if (nesting) abort("Misplaced #else");

		if (accept) {
		   accept = 0;
		   SC.prep_accept[0] = 0;
		}
		else {
		   SC.prep_accept[0] = SC.prep_accept[1];
		   accept = SC.prep_accept[0];
		}
	}
	{  case "#endif" :

		!if (nesting) abort("Misplaced #endif");
	   
		SC.prep_accept =  SC.prep_accept[[1:]];
		accept = SC.prep_accept[0];
	}
	{ if (accept) return token; }
   }
} % }}}

private define simple_typedef(token, fp) % {{{
{
   % simple typedef: typedef <existing_type> <new_type_name>;
   variable rest_of_decl = EMPTY;
   while(rest_of_decl[-1] != ';')
	rest_of_decl = strcat(rest_of_decl, get_line(fp,0));

   if ( parse_typedef(strcat(token, " ", rest_of_decl), 1) )
	return 1;

   return 0;
} % }}}

define slirp_discard_line (str)
{
   list_append (discard_line_strings, str);
}

define discard_line (fp)
{
   variable line, str, n;

   % Read a line.
   n = fgets (&line, fp);
   if (n == -1)
     {
	SC.eof = 1;
   	return 0;
     }
   % Some gtk header files end with 1 (or 2) line(s) without a ";"
   % ex. : gtkaboutdialog.h
   % G_DEFINE_AUTOPTR_CLEANUP_FUNC(GtkAboutDialog, g_object_unref)
   % which will confuse the parser.
   % In case of an empty line preceding such a line, we need to
   % discard it and return 1 so that the parser will call this
   % function again.
   if (strtrim (line) == EMPTY)
   % if (strcompress (line, " \t\r\n\f") == EMPTY)
     {
   	SC.line ++;
   	return 1;
     }
   foreach str (discard_line_strings)
     {
	if (is_substr (line, str) > 0)
	  {	       
	     % () = printf ("discarding line #%d : %s (%s)\n", SC.line, strtrim (line), SC.infname);
	     SC.line ++;
	     return 1;
	  }
     }
   % In case of no pattern matching, rewind fp to the begining of the
   % line and let the parser try something else.   
   % backup (fp, strlen (line));
   backup (fp, n);
   return 0;
}

define macro_or_type_definition(fp, token) % {{{
{
   if (token == NULL)
     token = prep_get_token(fp);
   
   forever {

      switch (token)

	{  case "#define" :

	   variable def = get_rest_of_line(fp);
	   () = string_match(def, "^[ ]*\\([^ ]+\\)[ ]*\\([^ ]*.*\\)", 1);
	   variable name = get_match(def, 1), value = get_match(def, 2);

	   variable parameterized = string_match(name, "(", 1);
	   if (parameterized)
		name = name[[0:parameterized - 2]];

	   if (ignored[name]) {
		_macros[name] = EMPTY;
		break;
	   }

	   % Macro definitions within interface file supersede those
	   % from a header file; #undef may be used to circumvent
	   if (_macros[name] != NULL) break;

	   % Avoid improper substitutions of parameterized macros, by
	   % grouping RHS parentheses with the RHS macro name (if any)
	   if (value != EMPTY)
		(value, ) = strreplace(value, " (", "(", 99);

	   if (parameterized || not constant_macro(name, value, 1)) {

		!if (wrappable_macro(name)) {

		   variable subst = SC.types[name];
		   if (subst != NULL)
			_macros[name] = subst.type;
		   else
			_macros[name] = value;
		}
		else
		   _macros[name] = value;
	   }
	   break;
	}

	{  case "#undef" :  macro_undefiner( get_token(fp) ); break; }

	{  case "struct" or case "union": () = (@SC.parse_struct)(fp); break; }

	{  case "enum" : parse_enum(fp, 0); break; }

	{  case "typedef" :

		token = get_token(fp);

		if (token == "struct") {

		   variable decl = (@SC.parse_struct)(fp);

		   % If no new type has been defined, or is wanted, skip
		   if (decl == EMPTY or SC.autotype == 0)
			break;

		   decl = group_pointer_qualifiers_with_type(decl);
		   variable is_ptr, type = strtok(decl);

		   % FIXME: do more with the pointerness here
		   (type[0], is_ptr) = strreplace(type[0], ASTERISK, EMPTY, 99);

		   % Wrap new type, if not already defined
		   if (SC.types[type[0]] == NULL)
			slirp_define_opaque(type[0], NULL, "free");

		   % Ditto, if an alias is being defined for it too
		   if (length(type) == 2 && type[1] != type[0] &&
						SC.types[type[1]] == NULL) {
			variable ot = _define_opaque(type[1], type[0]);
			ot.type = type[1];
		   }
		}
		else {
		   switch(token)
		   { case "enum": parse_enum(fp, 1); }
		   { () = simple_typedef(token,fp); }
		}

		break;
	}

	{  case "extern":

		if ("\"C\"" == next_token(fp)) {
		   swallow_to(fp, '{');
		   break;
		}
		else 
		   return 0;
	}

	{
	   if (token == EMPTY)			% eof condition
		break;
	   
	   if (token == ";" || _macros[token] == EMPTY) {
		token = prep_get_token(fp);
		continue;
	   }

	   if (string_match(token,"[#{}]",1)) {	      
		() = get_rest_of_line(fp);
		break;
	   }
	   else {
	      unget_token(fp, token);
	      return 0;	      
	   }
	}
   }

   return 1;
} % }}}

% }}}

%  Argument handling % {{{
define arg_new(type, name, defval, arrayspec, autotype) % {{{
{
   variable arg = get_typemap(type, autotype);
   if (arg == NULL) return NULL;
   arg = @arg;
   arg.name = name;
   arg.defval = defval;
   arg.nullable = 0;
   arg.arrayspec = arrayspec;
   return arg;
} % }}}

private define standard_arg(type, name, num, defval, arrspec, autotype, f) % {{{
{
   variable arg = arg_new(type, name, defval, arrspec, autotype);
   if (arg == NULL)
	return NULL,warn_ignore(f + ":" + name, "type <" + type + 
		"> unmapped or not instantiable in S-Lang");

   @num = @num + 1;			% SLang-1 doesn't like @num += 1;
   arg.lname = sprintf("arg%d", @num);	% Local name (within wrapper func)
   return arg;
} % }}}

private variable arg_init = &standard_arg;

private define vectorized_arg(type, name, num, defval, arrspec, autotype, f)%{{{
{
   variable argmaps = _argmaps[AM_In];
   if (argmaps[type] != NULL || argmaps[type + " " + name] != NULL)
	return NULL, warn_novec(f, "arg type <%s> is annotated", type);

   variable arg = arg_new(type, name, defval, arrspec, autotype);
   if (arg == NULL) return NULL;

   % A DIM annotation applied to a vectorized func arg means the arg:
   %   - defines the length of (other) arrayed args, under normal C usage,
   %     and that arrayed args must have exactly this number of elements
   %   - is not to be passed in from S-Lang scope

   if (string_match(arg.name,"^DIM[0-9]+$",1)) {
	arg.marshal = 0;
	arg.lname = strlow(arg.name);
	arg.dim = -1;		% prevent it from being treated as vectorized
   }
   else {
	@num = @num + 1;
	arg.lname = sprintf("arg%d", @num);
	arg.declarer = &vectorized_declarer;
	arg.marshaler = &vectorized_marshaler;
	arg.cleaner = &default_cleaner;
	arg.returner = &vectorized_returner;
	arg.vectorized = 1; 
   }
   return arg;
} % }}}

define parse_args(func, argstr, autotype) % {{{
{
   % Routine assumes enclosing parens have been removed from argstr,
   % and that no whitespace remains around any commas it may contain
   argstr    = trim_around(argstr, "*");
   argstr    = trim_around(argstr, "[");
   variable toks = strchop(argstr, ',', 0);
   variable ntoks = length(toks), ntoks_minus_one = ntoks - 1;
   variable arg, args = TypeMapping[0], nargs = 0;
   variable type, name, arrspec, defval = EMPTY;

   if (argstr == EMPTY || toks[0] == VOID)
	return args;					% zero-arg function

   variable toknum = 0;
   while (toknum < ntoks) {

	type = EMPTY;
	arg = toks[toknum];
	arrspec = EMPTY;
	variable _autotype = autotype;

	if (is_substr(arg, "(")) {		% function pointer

	   _autotype = 0;			% never autowrap func ptrs

	   while(toknum < ntoks_minus_one && arg[-1] != ')') {
		toknum++;
		arg = strcat(arg, ",", toks[toknum]);
	   }

	   if (string_match(arg,
			"\\([^( ]+\\)[ ]*\\((.**.*)\\)[ \t]*\\((.*)\\)",1)) {

		type = strcat(get_match(arg, 1), "(*)", get_match(arg, 3));
		name = get_match(arg, 2);
	   }
	}

	if (type == EMPTY) {

	   % Grab default value, if present
	   if (string_match(arg, "^\\([^=]+\\)=\\(.+\\)$", 1)) {
		defval = strtrim_beg(get_match(arg, 2));
		arg = strtrim_end(get_match(arg, 1));
	   }
	   else if (defval != EMPTY)
		abort("Illegal default arg value for function: %S", func);

	   variable pos = string_match(arg, "[^ *]*$", 1);
	   type = strtrim_end(arg[[:pos-2]]);
	   if (pos > 1) {
		name = arg[[pos-1:]];
		pos = string_match(name, "\\[[^]]*\\]", 1);
		if (pos) {
		   % Keep the array spec around for argmap matching, etc ...
		   arrspec = name[[pos-1:]];
		   % ... but ensure that the type is treated as a pointer/ref
		   type += "*";
		   name = name[[:pos-2]];
		}
	   }
	   else	
		name = EMPTY;
	}

	arg = (@arg_init) (type, name, &nargs, defval, arrspec, _autotype,func);
	if (arg == NULL) return NULL;

	if (SC.debug)
	   print_typemap(arg, sprintf("%s: %s(%s)", _function_name, func,
		    						argstr));

	if (arg.arrayspec != EMPTY) {

	   variable dims = strchop( strcompress(arg.arrayspec," []"), SPACE, 0);
	   arg.dim = length(dims);
	   arg.nelems = 1;
	   foreach(dims) {

		variable dim = ();

		if (_slang_guess_type(dim) == String_Type) {

		   variable tmp = dim;

		   dim = _macros[dim];

		   % If not a constant macro, check integral constants
		   if (dim == NULL)
		     {
			dim = SC.interface.int_consts[tmp];
			if (dim == NULL)
			  dim = SC.interface.long_consts[tmp];
			if (dim == NULL)
			  dim = SC.global_interface.int_consts[tmp];
			if (dim == NULL)
			  dim = SC.global_interface.long_consts[tmp];

			if (dim != NULL)
			   dim = dim.name;
		   }

		   if (dim == NULL) {
			warn("unknown array dimension for %S in %S: %s; " +
			     "setting num elements to 0",
			     arg.name, func, tmp);
			arg.nelems = 0;
			break;
		   }
		}
		arg.nelems *= int(atof(dim));		% atoi not in SLang1
	   }

	   if (arg.dim > 1) {
		() = string_match(arg.arrayspec,
				  	"\\(\\[[0-9]+\\]\\)\\(.*\\)", 1);
		arg.cast = sprintf("(%s (*)%s)", arg.deref_type,
						get_match(arg.arrayspec, 2));
	   }
	}

	SC.have_refs |= (arg.ltype == SLang_Ref_Type);

	args = [args, arg];
	toknum++;
   }

   return args;
} % }}}

private define make_arg_patterns(arg, nameless) % {{{
{
   % An argmap parameter will match a prototype parameter if they agree in
   % type AND either a) both are unnamed, or b) the latter is named and the
   % former is not, or c) both have the same names.  Multi-argument argmaps
   % will only match if each parameter within its argument sequence matches.
   %
   % Matching assumes argmap and prototype param lists are in normal form:
   %
   %		<type1>[<space><name1>[,<type2>[<space><name2>] ...]
   %
   %  - any outer/enclosing parentheses have been removed
   %  - comments are stripped
   %  - pointer qualifiers are grouped with the type
   %  - optional names are separated from type by exactly 1 space
   %  - commas delimiting multiple parameters have no surrounding whitespace
   %
   %  Example: double* d1,int,char* s

   variable patterns, type = arg.const;
   if (arg.arrayspec == EMPTY)
	 type += arg.type;		% if not arrayed, match on full type
   else
	 type += arg.deref_type;	% else match on scalar type + arrayspec

   if (arg.cpp_ref) type += "&";

   if(nameless || arg.name == EMPTY)
	patterns = [ strcat(type, arg.arrayspec) ];
   else {
	if (arg.arrayspec == EMPTY)
	   patterns = [type, strcat(type," ",arg.name)];
	else
	   patterns = [strcat(type," ",arg.name, arg.arrayspec)];
   }
   return patterns;
} % }}}

private define permute_args(funcmap, nameless) % {{{
{
   % Precondition argmap matching by generating an ordered list
   % of longest-match-first patterns for the given function

   !if (funcmap.nargs) return NULL;

   variable argno = funcmap.nargs;
   variable patterns = Array_Type[argno], prev_arg_patterns = String_Type[0],;

   while (argno, argno--) {

	variable arg = funcmap.args[argno], last_pattern;
	variable this_arg_patterns = make_arg_patterns(arg, nameless);

	last_pattern = this_arg_patterns[-1];

	foreach(prev_arg_patterns) {
	   variable prev_pattern = ();
	   this_arg_patterns = [ this_arg_patterns,
				strcat(last_pattern, ",", prev_pattern) ];
	}

	prev_arg_patterns = this_arg_patterns;
	patterns[argno] = this_arg_patterns;	% reverse these, downstream
   }

   return patterns;
} % }}}

private variable unquoted = "[^\"]+";
private variable quoted   = "^\".+\"";
private variable line_terminator = [ EMPTY, "\n" ];

private define do_variable_substitutions(fmap, amap) % {{{
{
   % Note that proxy variables defined by #argmap(in) annotations can be
   % safely referenced within subsequent annotations, e.g. #argmap(final)
   variable vars = Struct_Type[0], var = fmap.args[amap.argnum-1].proxy;
   if (var != NULL) vars = [ vars, var];
   vars = [ vars, amap.local_vars, fmap.local_vars];
   !if (length(vars)) return;

   % Ensure each variable is declared later during wrapper emission ...
   variable argnum = string(amap.argnum);
   foreach (amap.local_vars) {
	var = ();
	var.lname = var.name + argnum;
	fmap.local_vars = [ fmap.local_vars, @var];
   }

   % ... now perform the substitutions within the argmap code fragment
   variable pos, len, orig = amap.code, new = EMPTY, loc = 1, chunk;
   forever {

	variable substitute = not(string_match(orig, quoted, loc));
	if (substitute && not string_match(orig, unquoted, loc))
	      break;

	(pos,len) = string_match_nth(0);
	chunk = orig[[pos:pos+len-1]];
	loc = pos + len;

	if (substitute)
	   foreach (vars) {

		var = ();
		variable pat = sprintf("\\<%s\\>",var.name);

		while (string_match(chunk, pat, 1)) {
		   (pos,len) = string_match_nth(0);
		   chunk = strcat(chunk[[:pos-1]], var.name, argnum,
			 				chunk[[pos+len:]]);
		}
	   }

	% Handle newlines, since regexp matching will not
	new = strcat(new, chunk, line_terminator[ orig[loc] == '\n' ] );
	loc++;
   }
   amap.code = new;
} % }}}

define apply_multi_arg_maps(funcmap, kind, unmarshal, conflicts) % {{{
{
   variable amaps_of_this_kind = _argmaps[kind];
   !if (length(amaps_of_this_kind)) return;

   variable argno = 0, amaps_for_this_func = funcmap.argmaps[kind];
   if (amaps_for_this_func == NULL)
	amaps_for_this_func = ArgMapping[0];

   if (funcmap.arglist_patterns == NULL) {
	variable named = permute_args(funcmap, 0);
	variable nameless = permute_args(funcmap, 1);

	funcmap.arglist_patterns = Array_Type[funcmap.nargs];
	while (argno < funcmap.nargs) {

	   % Cull potential duplicates
	   variable first = 0;
	   if (named[argno][0] == nameless[argno][0])
		first++;

	   funcmap.arglist_patterns[argno] =
		[ reverse(named [argno] [[first:]]), reverse(nameless [argno])];
	   argno++;
	}
	named = NULL;
	nameless = NULL;
   }

   argno = 0;
   while (argno < funcmap.nargs) {

	foreach ( funcmap.arglist_patterns[argno] ) {

	   variable pattern = ();

	   if (SC.debug > 1)
		dprintf("\tapply_multi_argmap (%S): trying to match <%S>",
								kind, pattern);

	   if (conflicts != NULL && conflicts[pattern] != NULL)
		break;

	   variable am = amaps_of_this_kind[pattern];
	   if (am != NULL) {

		if (SC.debug > 1)
		   dprintf("\tapply_multi_argmap (%S): MATCHED", kind, pattern);

		% For annotation codefrags to be included args must be popped
		funcmap.pop_args = 1;

		am = @am;			% Ensure uniqueness by deep
		am.args = @am.args;		% copying structs/arrays
		am.funcmap = funcmap;
		am.argnum = argno + 1;

		do_variable_substitutions(funcmap, am);

		% Assign a local variable name to each arg w/in the argmap
		variable which = 0;
		foreach(am.args) {

		   variable arg = @(), farg = funcmap.args[argno];

		   % Scalars passed via the default S-Lang arg transfer
		   % mechanism need to be dereferenced once before use
		   if (farg.typeclass == TYPECL_SCALAR && not funcmap.pop_args)
			arg.lname = "*" + farg.lname;
		   else
			arg.lname = farg.lname;

		   variable match = length( where(am.which == which));
		   if (match) {
			if (am.proxy != NULL) {
			   farg.proxy = @am.proxy;
			   farg.proxy.defval = farg.defval;
			   farg.mnemonic = am.proxy.mnemonic;
			}
			else if (am.usage != NULL)
			   farg.usage = am.usage;
		   }
		   else if (unmarshal)
			farg.marshal = 0;

		   am.args[which] = arg;

		   argno++;
		   which++;
		}

		argno--;
		amaps_for_this_func = [ amaps_for_this_func, am ];
		break;
	   }
	}

	argno++;
   }

   funcmap.argmaps[kind] = amaps_for_this_func;
} % }}}

private define apply_single_arg_maps(funcmap, kind, unmarshal) % {{{
{
   variable amaps_of_this_kind = _argmaps[kind];
   !if (length(amaps_of_this_kind)) return;

   variable amaps_for_this_func = funcmap.argmaps[kind];
   if (amaps_for_this_func == NULL)
	amaps_for_this_func = ArgMapping[0];

   variable argno;
   for (argno=0; argno<funcmap.nargs; argno++) {

	variable arg = funcmap.args[argno];
	variable patterns = make_arg_patterns(arg, 0);

	foreach (patterns) {
	   variable pattern = ();
	   variable am = amaps_of_this_kind[pattern];
	   if (am != NULL) {

		% For annotation codefrags to be included args must be popped
		funcmap.pop_args = 1;

		am = @am;			% Ensure uniqueness by deep
		am.args = @am.args;		% copying structs/arrays
		if (unmarshal)
		   arg.marshal = 0;
		am.args[0] = @arg;
		am.argnum = argno + 1;
		am.funcmap = funcmap;

		amaps_for_this_func = [ am, amaps_for_this_func];
		do_variable_substitutions(funcmap, am);
	   }
	}
   }

   funcmap.argmaps[kind] = amaps_for_this_func;
} % }}}

private define ignored_by_argmap(fmap)
{
   apply_multi_arg_maps(fmap, AM_Ignore, 0, NULL);
   variable ig = fmap.argmaps[AM_Ignore];
   return (ig != NULL && length(ig));
}

private define is_function_typedef(retval, fp)
{
#iffalse
   token = get_token(fp);
   if (token[0] == '(')
	return 1;
   backup(fp, strbytelen(token)+1);
#endif
   return 0;
}
% }}}

%  Miscellaneous helper functions {{{

define slirp_include_rc(rcfile) %  Resource file includer {{{
{
   Include_Files = [ Include_Files, rcfile ]; 
   Include_Nesting++;
   () = evalfile(rcfile);
   Include_Nesting--;
   Include_Files = Include_Files[ [1:Include_Nesting] ];
} % }}}

private variable tlevel = 0;
private define list_subtypes();

private define list_subtypes(typemap) % {{{
{
   variable indent = sprintf("%%%Ss",tlevel*3+1);
   () = printf(indent," ");

   if (typemap.subtypes != NULL) {
	() = printf("+");
	() = printf("  %S\n",typemap.sltype);
   }
   else {
	() = printf("|");
	() = printf("  %S\n",typemap.type);
   }

   tlevel++; 
   foreach(typemap.subtypes) {
	variable subtype = ();
	list_subtypes(SC.types[subtype]);
   }
   tlevel--; 
} % }}}

define list_opaque_types() % {{{
{
    foreach (SC.opaque_types) {
	variable ot = ();
	if (ot.parent == NULL)
	   list_subtypes(ot);
    }
} % }}}

define emit_typemaps(file) % {{{
{
   variable refs = Struct_Type[0], virtuals = SC.opaque_types, types = @refs;
   variable tmap, parent, mapping, fp = fopen(file,"w+");

   foreach(SC.types) using ("values") {

	tmap = ();

	if (tmap.type == NULL)				% virtual base type
	   continue;
	else switch(tmap.ltype)
	   { case SLang_Ref_Type: refs = [refs, tmap]; }
	   { case SLFile_FD_Type: continue; }		% phony type
	   { types = [types, tmap]; }
   }

   foreach (virtuals) {

	tmap = ();

	% Not necessary to specify finalizer, since emitted typemap
	% amounts to a series of forward declarations, not defintions
	% (ie, SLIRP will not generate new C types for them)
	mapping = sprintf("\"%s\"",tmap.sltype);
	if (tmap.parent != NULL)
	   mapping = sprintf("%s, \"%s\"",mapping, tmap.parent.sltype);

	if (fprintf(fp,"slirp_define_opaque(%s);\n",mapping) == -1)
		error("Saving typemaps to file: %s",file);
   }

   foreach (types) {

	tmap = ();
	if (tmap.typeclass == TYPECL_OPAQUE)
		mapping = sprintf("slirp_map_opaque(\"%s\",\"%s\");\n",
						tmap.type, tmap.sltype);
	else
		mapping = sprintf("slirp_map_%s(\"%s\");\n",
					strlow(tmap.mnemonic),tmap.type);

	if (fprintf(fp, mapping) == -1)
	   error("Saving typemaps to file: %s",file);
   }

   foreach (refs) {
	tmap = ();
	if (fprintf(fp,"slirp_map_ref(\"%s\");\n",tmap.type) == -1)
	      error("Saving typemaps to file: %s",file);
   }

   () = fclose(fp);
} % }}}

define generate_prototype(fmap) % {{{
{
   variable args, proto, regexp, rename, retval = fmap.retval;

   % See if compiled function should be mapped to different S-Lang name
   % FIXME: sort this list and use binary search for speed
   foreach (SC.renames) using ("keys", "values") {
	(regexp, rename) = ();

	% Prohibit complete elision of class name from C++ constructors,
	% since it would result in multiple S-Lang funcs w/ same name
	if (rename == EMPTY && fmap.class != NULL
			    && fmap.class.name == fmap.name)
	   continue;

	if (string_match(fmap.slname, regexp, 1))
	   fmap.slname = strcat(rename, get_match(fmap.slname,1));
	if (fmap.slname == EMPTY)
	   abort("Function <%S> -rename-d to empty string!",fmap.name);
   }
 
   if (fmap.gname == NULL)
	fmap.gname = SC.wrapper_prefix + fmap.slname;
   else
	fmap.gname = SC.wrapper_prefix + fmap.gname;

   if (SC.cfront)
	proto = sprintf("%s %s (", (@retval.referer)(retval, 1, 1), fmap.gname);
   else if (SC.genstubs)
	proto = sprintf("%s (", fmap.gname);
   else
	proto = sprintf("static void %s (", fmap.gname);

   if (fmap.pop_args || fmap.nargs == 0)
	args = VOID;
   else {

	args = String_Type[0];
	foreach (fmap.args) {
	   variable arg = ();
	   arg = (@arg.referer)(arg, 1, SC.genstubs);
	   args = [args, arg ];
	}

	args = strjoin(args, ARG_SEPARATOR);
   } 

   return sprintf("%s%s)", proto, args);
} % }}}

define get_var_decl(fp) % {{{
{
   variable decl = get_token(fp);
   if (decl == EMPTY)
	return 0;

   if (_macros[decl] != NULL) {
	variable next = get_token(fp);
	if (next == "(") {			% discard parameterized macros
	   decl = EMPTY;
	   forever {
		variable tok = get_token(fp);
		if (tok == ")")
		   break;
		decl += " " + tok;
	   }
	}
	else
	   decl += " " + next;
   }

   SC.decl = decl;				% accumulate current declaration
   decl = get_rest_of_line(fp);
   if (decl == NULL) return 0;
   SC.decl += " " + decl;

   if (string_match(SC.decl,"(",1))		% heuristic: variable decls
	return 0;				% do not look like func decls

   if (SC.decl[-1] != ';')
	return 0;

   if (string_match(SC.decl, "\\(.+\\)=.*", 1)) % Strip value assignment,
	SC.decl = get_match(SC.decl, 1);	% if present

   SC.decl = trim_around(SC.decl, ",");
   SC.decl = group_pointer_qualifiers_with_type(SC.decl);

   if (SC.debug > 1)
	dprintf("variable declaration: %S", SC.decl);

   return 1;
} % }}}

private define wrap_variable(var_typemap) % {{{
{
   variable hash = SC.interface.intrin_vars;
   
   if (ignored[var_typemap.name])
	return;

   if (var_typemap.const != EMPTY)
     {
	switch(var_typemap.type)
	  {
	     case "char" or case "unsigned char" or
	     case "short" or case "unsigned short" or
	     case "int" or case "unsigned int"
	       :
	       hash = SC.interface.int_consts;
	  }
	  {
	     case "long" or case "unsigned long"
	       :
	       hash = SC.interface.long_consts;
	  }
	  {
	     case "long long" or case "unsigned long long"
	       :
	       hash = SC.interface.long_consts;
	     () = fprintf (stderr, "Warning long long variable seen.  Mapping to long\n");
	  }
	  { case "float" or case "double": hash = SC.interface.double_consts; }
     }   
   add_const(hash, var_typemap.name, var_typemap);
} % }}}

private define try_to_remember_private_variable(type, name)
{
   % The "private" here refers to variables instantiated in C scope but not
   % visible in S-Lang, such as a C struct instance whose definition is not
   % local to the header file being SLIRPed.  Pointers to private variables
   % need not be private, as they may be easily wrapped as opaques and passed
   % back & forth between S-Lang & C scope.  This is why they are remembered.

   type = SC.types[type];
   if (type == NULL)
	return;

   SC.interface.private_vars[ SC.interface.name + name ] = type;
}

define parse_var_decl(vars_ref) % {{{
{
   variable decl = do_macro_substitutions( strtrim_end(SC.decl,";") );
   % FIXME: is there a better way than this?
   variable is_struct, type = strjoin( decl[[:-2]], " " );
   (type, is_struct) = strreplace(type, "struct ", "", 1);
   variable var, name, vars = TypeMapping[0];

   foreach name (strtok(decl[-1], ",")) {

	if (ignored[name])
	   warn_ignore(name, "in ignored list");
	else {

	   var = parse_args(_function_name, type + " " + name, SC.autotype);

	   if (var == NULL) {
		try_to_remember_private_variable(type, name);
		continue;
	   }
	   
	   vars = [vars, var ];
	}
   }

   @vars_ref = vars;

   return 1;

} % }}}

define variable_declaration(fp) % {{{
{
   !if (get_var_decl(fp)) return 0;

   variable var, vars;

   !if (parse_var_decl(&vars))
	return 1;

   % Reflect scalar variables in the generated wrapper module
   foreach var (vars)
	if (var.dim <= 1)
	   wrap_variable(var);

   return 1;
} % }}}
% }}}

%  Function handling {{{

%  Ignoring {{{
variable ignored_functions = String_Type[0];

private define ignore(thing) { ignored[ strtrim(thing) ] = 1; }

private define ignore_array(array) 
{
   if (length(array)) {
	array = array_map(String_Type, &strtrim, array);
	array_map(Void_Type, &ignore, array);
   }
}

define finalize_ignore_list()
{
   ignore_array(ignored_functions);
   ignore_array(ignored_macros);
   ignore_array(ignored_variables);
} 
% }}}

define standard_return_mechanism(fmap) % {{{
{
   if (fmap.retval.ltype == VOID)
	return EMPTY;

   if (SC.cfront)
	if (fmap.retval.typeclass == TYPECL_POINTER) 	% de-constify,
	   sprintf("return (%s) ", fmap.retval.type);	% if necessary
	else
	   "return ";
   else
	"retval = ";
} % }}}

private define default_func_referer(fmap, ignore) { return fmap.name; }

private define vec_loop_begin() % {{{
{
   variable code = "", cast = "";
   if (SC.openmp)
     {
	code =  "   #pragma omp parallel for\n";
	cast = "(int)";		       %  see emit_declaration_block in slirp.sl
     }
   code += "   for (_viter=0; _viter < $cast vs.num_iters; _viter++) {\n"$;
   return code;
} % }}}

private define vec_loop_end() % {{{
{
   "   }\n";
} % }}}

private define vectorized_func_referer(fmap, vectorization) % {{{
{
   variable retval = fmap.retval, rtype = retval.type;

   if (vectorization.nargs > 1 || not fmap.args[0].marshal)
      slirp_emit("   if (vec_validate(&vs, %s) == -1) {finalize_refs(%s); return;}\n",
				vectorization.refs, vectorization.refs);

   % Don't alloc return value before input vectors are validated
   if (rtype != VOID) {
	variable ret_assignment = sprintf("retval[_viter] = ");
	slirp_emit("   VEC_ALLOC_RETVAL(%s%s, %s);\n", retval.const, retval.type,
							vectorization.refs);
   }
   else	
	ret_assignment = EMPTY;

   if (length(vectorization.dim_args)) {

	variable args = vectorization.dim_args ;
	variable names = "&" + struct_map(String_Type, args, "lname");
	names = strjoin(names, ",");

	% FORTRAN: ensure we use the base type, not the reference type
	if (fmap.language == FORTRAN)
	   args = struct_map(Struct_Type, args, "aux");

	variable types = struct_map(String_Type, args, "typeid");
	types = array_map(String_Type, &sltype_abbrev, types);
	types = strjoin(types, EMPTY);
	slirp_emit("   establish_dims(&vs,(char*)\"%s\",%s);\n", types, names);
   }

   slirp_emit( vec_loop_begin );
   slirp_emit("\t%s%s%s;\n", ret_assignment, (@fmap.referer)(fmap, ),
						vectorization.arglist);

   if (not SC.openmp)
	slirp_emit("\t%s;", vectorization.increments, vec_loop_end );

   slirp_emit("\n%s", vec_loop_end );
} % }}}

define annotate(fmap) % {{{
{
   apply_multi_arg_maps(fmap, AM_Setup, 0, NULL);
   apply_multi_arg_maps(fmap, AM_Init, 0, NULL);

   % In case of conflict, OUTPUT argmaps have precedence over INPUT
   apply_multi_arg_maps(fmap, AM_In, 1, _argmaps[AM_Out]);

   apply_single_arg_maps(fmap, AM_Out, 1);
   apply_multi_arg_maps(fmap, AM_Final, 0, NULL);
} % }}}

private define funcmap_process_retval(fmap) % {{{
{
   variable retval_str = fmap.retval;
   variable retval = get_typemap(retval_str, SC.autotype);
   if (retval == NULL)
	return 0, warn_ignore(fmap.name, sprintf( "unsupported return "+
						"type <%S>", retval_str));

   retval = @retval;		% prevent changes to original typemap
   if (SC.cfront)
	retval.lname = EMPTY;
   else
	retval.lname = "retval";

   if (retval.ltype == SLang_Ref_Type) {

	%  Arrays of indeterminate size can only be returned as opaque ptrs
	variable aux = retval.aux;
	if (aux.typeclass == TYPECL_SCALAR || aux.mnemonic == "string")
	    retval.typeid = retval.aux.mnemonic + "_ptr_Type";
	else
	    retval.typeid = "opaque_ptr_Type";

	retval.typeclass = TYPECL_OPAQUE;
	retval.returner = &opaque_returner;
	retval.dim = 0;

	!if (SC.num_reserved_types)
	   define_reserved_opaques();
   }

   % Functions with non-omitted, non-void return type have at least 1 out argmap
   if (retval.type != VOID) {

	variable rmap = _retmaps[retval_str];	% match #retmap on raw rtrn type

	if (rmap == NULL) {
	   rmap = @ArgMapping;
	   rmap.which = 0;
	   % rmap.method left NULL, to distinguish from user-specified #retmap
	   if (fmap.vectorized)
		rmap.code = sprintf("   %s;\n", vec_return(retval, 0));
	   else
		rmap.code = sprintf("   %s;\n", (@retval.returner) (retval));
	}
	else {
	   rmap = @rmap;
	   %  FIXME: this should be done with a callback attached
	   %  to the annotation, rather than the hack given here
	   SC.have_null_term_str_arrays |= 
	   			not(strncmp(retval_str,"NT_STR_ARRAY",12));
	}
	retval.usage = rmap.usage;
	rmap.args = [ @retval ];
	fmap.argmaps[AM_Out] = [rmap];
	rmap.funcmap = fmap;
   }

   fmap.retval = retval;
   return 1;
} % }}}

define finalize_vectorization(fmap) % {{{
{
   % This is called during code emission, rather than funcmap
   % creation, to be certain of the language being wrapped;
   % funcmaps can be created when the interface file is read
   % (#prototype), prior to any language-specific header/src

   % Do not vectorize C++ hidden "this" arg passed to normal class methods
   if (fmap.language == CPLUSPLUS && fmap.name != fmap.class.name)
	variable args = fmap.args[[1:]];
   else
	args = fmap.args;

   variable casts = struct_map(String_Type, args, "cast");
   variable dims = struct_map(Int_Type, args, "dim"), nargs = length(args);
   variable dim_args = where(dims < 0), min_dimensionality = length(dim_args);

   if (min_dimensionality)
	foreach(args [ where (dims >= 0) ]) {
	  variable arg = ();
	  arg.dim = min_dimensionality;
	}

   variable names = struct_map(String_Type, args, "lname");

   % Hidden array len is declared scalar, but FORTRAN calls needs address
   if (fmap.language == FORTRAN && min_dimensionality)
	names[dim_args] = "&" + names[dim_args];

   dims = dims == 0;		 % which arguments are scalars?

   if (SC.openmp)
	names = array_map(String_Type, &sprintf,"%s%s[_viter]", casts,names);
   else
	names = array_map(String_Type, &sprintf,"%s%s%s", casts,
						DeReferer[dims], names);

   names = "(" + strjoin(names, ",") + ")";
   dim_args = args[dim_args];

   fmap.referer_hook = &vectorized_func_referer;

   variable vectorization = struct{nargs, arglist, dim_args, refs, increments};
   vectorization.nargs = nargs;
   vectorization.arglist = names;
   vectorization.dim_args = dim_args;
   vectorization.refs = sprintf("VREF_%d", nargs - min_dimensionality);
   vectorization.increments = sprintf("VINCR_%d", nargs - min_dimensionality);

   return vectorization;
} % }}}

private define funcmap_process_args(fmap) % {{{
{
   if (fmap.vectorized) arg_init = &vectorized_arg;

   variable args = parse_args( fmap.name, fmap.args, SC.autotype);

   arg_init = &standard_arg;

   if (args == NULL) {
	if (fmap.vectorized) {
	   fmap.vectorized = 0;
	   return funcmap_process_args(fmap);
	}
      return 0;
   }
   fmap.nargs = length(args);

   if (fmap.nargs > SLIRP_MAX_VEC_ARGS && fmap.vectorized) {
	warn("%s not vectorized: too many arguments", fmap.name);
	fmap.vectorized = 0;
	return funcmap_process_args(fmap);
   }
   fmap.args = args;

   variable nullables = accepts_null_args[fmap.name];
   if (nullables != NULL) {
	foreach( fmap.args[nullables - 1] ) {
	   variable arg = ();
	   arg.nullable = 1;
	}
   }
   
   variable retstructs = returns_struct [fmap.name];
   if (retstructs != NULL)
     {	
	% start counting args from 0
	foreach (fmap.args [retstructs - 1] )
	  {	    	     
	     arg = ();
	     arg.retstruct = 1;
	     arg.marshal = 0;
	     arg.returner = &struct_returner;
	     fmap.retval.retstruct = 1;
	  }	
     }

   return 1;
} % }}}

private define funcmap_new(decl) % {{{
{
   decl = group_pointer_qualifiers_with_type(decl);

   !if (string_match(decl, "(.*)", 1))
	return NULL, warn_ignore(decl, "could not identify argument list");

   variable pos, len;
   (pos, len) = string_match_nth(0);

   variable typed_name = decl[ [:pos - 1] ];

   variable tokens = do_macro_substitutions( typed_name );
   if (length(tokens) < 2)
	return NULL, warn_ignore(decl, "function ptr or missing return type?");

   variable fmap = @FuncMapping;
   variable name = tokens[-1];

   !if (strncmp(name, "operator", 8))
	return NULL, warn_ignore(decl, "C++ operators not supported yet");

   % Remove scope resolution operator, if present
   if (SC.cplusplus && string_match(name, "\\([^:]+\\)$", 1))
	name = get_match(name, 1);

   !if (string_match(name,SC.funcprefix,1))
	return NULL, warn_ignore(name, "did not match function regexp");

   if (ignored[name])
	return NULL, warn_ignore(name,"in ignored list");

   fmap.name = name;
   fmap.retval = strjoin( tokens[[:-2]], " ");
   % Argument list: first strip balanced sets of enclosing parens
   variable args = decl[ [pos : pos+len-1] ];
   while (string_match(args, "^(\\(.*\\))$",1))
	args = get_match(args, 1);

   % If unbalanced closeparen remains then it means we have parenthetical
   % content following the arglist (e.g. preprocessor macros), so strip it
   if (string_match(args, "^\\([^(]+\\)).*$",1))
      args = get_match(args, 1);

   args = strtrim (args," \t");		% Add finishing touches, effectively
   args = trim_around(args, ",");	% putting arglist into "normal form"
   args = trim_around(args, ":");
   fmap.args = args;

   fmap.argmaps = Empty_Assoc_Array;
   fmap.overloads = Struct_Type[0];
   fmap.slname = fmap.name;		% by default S-Lang name matches C name
   fmap.local_vars = Struct_Type[0];
   fmap.referer = &default_func_referer;
   fmap.sizer = &c_arg_sizer;
   fmap.language = C;			% will be overridden for fortran/c++

   fmap.vectorized = SC.vectorize || try_vectorize[fmap.name];
   if (fmap.vectorized) {
	if (dont_vectorize[fmap.name]) {
	   fmap.vectorized = 0;
	   warn_novec(name, "in #novectorize list");
	}
	else if (args == EMPTY || args==VOID) {
	   fmap.vectorized = 0;
	   warn_novec(name, "empty arg list");
	}
   }

   % Stub signatures must exactly match func declarations (including Windows
   % tags such as __declspec), so set stub name to fully qualified decl name
   if (SC.genstubs)
	fmap.gname = typed_name;

   return fmap;
} % }}}

define parse_func_decl(decl, prohibit_overloads, apply_annotations) % {{{
{
   variable name, typed_name, args, inlined = (decl[0] == INLINED);
   if (inlined) decl = decl[[1:]];

   variable fmap = funcmap_new(decl);
   if (fmap == NULL) return NULL;

   variable existing_fmap = SC.interface.functions[fmap.name];
   if (existing_fmap != NULL)
	if (prohibit_overloads)
	   return existing_fmap;

   !if (funcmap_process_retval(fmap)) return NULL;
   !if (funcmap_process_args(fmap)) return NULL;

   if (fmap.nargs and apply_annotations) {
	if (ignored_by_argmap(fmap))
	   return NULL, warn_ignore(fmap.name, "ignored by argmap");
	annotate(fmap);
   }

   if (existing_fmap == NULL)
	SC.interface.functions[fmap.name] = fmap;
   else {

	!if (length(existing_fmap.overloads))
	   existing_fmap.overloads = [ @existing_fmap ];

	existing_fmap.overloads = [ existing_fmap.overloads, fmap ];
   }

   % Determine argument transfer mechanism for this function {{{
   %
   % A wrapper for a function with one or more arguments will populate
   % its arguments with values from S-Lang either implicitly or explicitly.
   %
   % By default SLIRP attempts to use the implicit form, since it yields
   % shorter code. With this method args are transferred directly from
   % the S-Lang stack through the C stack (via the arguments specified in
   % the wrapper prototype, whose signature is by definition non-void).
   % In this case, no "pop" calls are issued within the wrapper.
   %     
   % The explicit form, wherein the wrapper is prototyped with a void
   % signature and explicitly issues a series of "pop" calls to populate
   % its arguments with values from the S-Lang stack, is employed when
   % the wrapped function
   %
   %	1) legitimately accepts NULL as a value for one or more arguments
   %	2) has had an annotation applied
   %
   % OR the signature of the wrapped function contains
   %
   %	3) more args than SLang permits w/in an intrinsic func table entry (7)
   %    4) a pointer or array type (treated more or less equivalently)
   %	5) a SLang_Struct_Type arg
   %	6) an "opaquely typed" argument
   %	7) that the function/method is overloaded (in the C++ sense)
   %	8) a default value for one or more arguments

   if (fmap.pop_args == NULL) {

	if (SC.cfront)			% No S-Lang code, just pure C wrappers

	   fmap.pop_args = 0;

	else if (fmap.nargs) {
	   
	   if (SC.always_pop_args || accepts_null_args[fmap.name]
				  || length(fmap.overloads) || fmap.nargs > 7)

		fmap.pop_args = 1;

	   else {
		variable map = struct_map(String_Type, fmap.args, "ltype");
		fmap.pop_args = length(where(map == SLang_Ref_Type or
					map == SLirp_Opaque_Type or
		    			map == SLang_Struct_Type));

		!if (fmap.pop_args) {
		   map = struct_map(String_Type, fmap.args, "defval");
		   fmap.pop_args = length(where(map != EMPTY));
		}
	   }
	}
	else
	   fmap.pop_args = 0;
   } % }}}

   fmap.inlined = inlined;
   return fmap;
}  %}}}

define get_func_decl(fp, fragment) % {{{
{
   variable decl = fragment;
   if (decl == NULL) decl = get_line(fp, 1);
   if (decl == NULL) return NULL;

   variable open_paren = string_match(decl,"(",1);
   !if (open_paren) {

	if (macro_or_type_definition(fp, NULL))
	   return NULL;

	% Is return type declared on different line from rest of fxn signature?
	variable next_line = get_line(fp, 1);
	if (next_line != NULL) {
	   decl = sprintf("%s %s", decl, next_line);
	   open_paren = string_match(decl,"(",1);
	}
   }

   !if (open_paren) {
	if (SC.debug)
	   warn_ignore(decl,"not a function declaration");
	return NULL;
   }

   variable inlined = 0;
   forever {

	% Inlined funcs may be tagged with 'inline' keyword ...
	if (string_match(decl, "\\<inline\\>", 1))
	   (decl, inlined) = strreplace(decl, "inline ", "", 1);

	% ... OR simply defined in-place
	variable begin_body = is_substr(decl, "{");

	if (begin_body) {

		inlined = 1;

		if ( not(is_substr(decl, "}")) )
		   swallow_rest_of_C_block(fp);

		decl = strcat( strtrim(decl[[:begin_body-2]]), ";");
		break;
	}

	if ( decl[-1] == ';' ) break;
	decl += " " + prep_get_token(fp);
	decl = strcat(decl, " ", get_line(fp,0));
   }

   % A function declaration need not be parsed immediately, so
   % if its body is inlined then tag the declaration as such
   if (inlined) decl = sprintf("%c%s", INLINED, decl);

   return decl;
} % }}}

define function_declaration(fp, decl, prohibit_overloads) % {{{
{
   if (decl == NULL)
	decl = get_func_decl(fp, SC.decl);
   if (decl != NULL)
	()  = parse_func_decl(decl, prohibit_overloads, 1);

   SC.decl = EMPTY;
} % }}}

%  Function annotation interface {{{

private define check_empty_qualifier_list(qualifiers, directive)
{
   !if (length(qualifiers))
      abort("Empty qualifier list not permitted for %S directive",directive);
}

variable Symbol_Context = struct {buf, language, inline_type, openmp};

% #prototype directive {{{
private variable Proto_Fortran = 1;
private variable proto_qualifiers = "^[ \t]*(\\([a-z, \t]*\\))[ \t]*$";

private define parse_proto_qualifiers(ctx, tokenized_qualifiers)
{
   check_empty_qualifier_list(tokenized_qualifiers, "#prototype");
   foreach (tokenized_qualifiers) {
	variable qualifier = strtrim(());
	switch(qualifier)
	{case "fortran" : ctx.language = Proto_Fortran; }
	{ abort("Invalid #prototype qualifier: <%s>", qualifier); }
   }
}

private define func_prototyper(line, ctx)
{
   if (line == NULL) {					% done with block

	if (Include_Nesting) return;			% no-op for externs

	ctx.buf = strtrans(ctx.buf,"\t\n", " ");
  	variable funcs = strtok( ctx.buf, ";");

	foreach(funcs) {
	   line = ();
	   if (parse_func_decl(line, 1, 1) == NULL)
		abort("Invalid #prototype content: <%S>"+
			"\nCheck return and parameter types",line);
	}
	ctx.buf = NULL;
	return;
   }

   if (ctx.buf == NULL) {
	ctx.buf = EMPTY;
	if (string_match(line, proto_qualifiers, 1))
	   parse_proto_qualifiers(ctx, strtok(get_match(line, 1)));
	return;
   }

   line = strtrim(extract_element(line, 0, '%'));
   if (line == EMPTY) return;
   ctx.buf = strcat(ctx.buf, line);
} % }}}

private define func_vectorizer(func, ctx) % {{{
{
   % Vectorization is meaningless in the following contexts
   if (SC.cfront || SC.genstubs || Include_Nesting) return;

   if (func == NULL) {				% done with block

	foreach( strtok(ctx.buf, "|")) {

	   func = ();

	   % A vectorization spec can be either a simple function name
	   % or a complete (and probably annotated) function prototype
	   if (string_match(func, "\\([^(]+\\)(.*)", 1)) {
		variable name = strtok( get_match(func, 1) )[-1];
		try_vectorize[name] = 1;
		() = parse_func_decl(func, 0, 1);
	   }
	   else if (not ignored[func])
		try_vectorize[func] = 1;
	}

	ctx.buf = NULL;
   }
   else if (ctx.buf == NULL)
	ctx.buf = EMPTY;
   else {
	func = strtrim(extract_element(func, 0, '%'));
	if (func != EMPTY)
	   ctx.buf = strcat(ctx.buf, "|", func);
   }
} % }}}

private define func_unvectorizer(func, ctx) % {{{
{
   if (func == NULL) {				% done with block
	foreach (strtok(ctx.buf, ",")) {
	  func = ();
	  dont_vectorize[func] = 1;
	}
	ctx.buf = NULL;
   }
   else if (ctx.buf == NULL)
	ctx.buf = EMPTY;
   else {
	func = strtrim(extract_element(func, 0, '%'));
	if (func != EMPTY)
	   ctx.buf = strcat(ctx.buf, ",", func);
   }
} % }}}

private define func_renamer(line) % {{{
{
   !if (string_match(line, "^\\([^ %\t]+\\)[ \t]+\\([^% \t\n\r]+\\)", 1)) {
	warn("Invalid #rename directive: <%S>",strtrim(line));
	return;
   }
   SC.renames[ make_rename_regexp( get_match(line,1) ) ] = get_match(line, 2);
} % }}}

private define symbol_ignorer(symbol, ctx) % {{{
{
   if (symbol == NULL) {				% done with block
	array_map(Void_Type, &ignore,  strtok(ctx.buf, ","));
	ctx.buf = NULL;
   }
   else if (ctx.buf == NULL)
	ctx.buf = EMPTY;
   else {
	if (string_match(symbol, "^\\([^%]*\\).*",1))
	   symbol = get_match(symbol, 1);
	if (symbol!= EMPTY)
	   ctx.buf = strcat(ctx.buf, ",", symbol);
   }
} % }}}

% Inline directive {{{
private variable inline_qualifiers = "^[ \t]*(\\([a-z, \t]*\\))[ \t]*$";
private variable Inline_Init = 1;
private define parse_inline_qualifiers(ctx, tokenized_qualifiers)
{
   check_empty_qualifier_list(tokenized_qualifiers, "#inline");
   foreach (tokenized_qualifiers) {
	variable qualifier = strtrim(());
	switch(qualifier)
	{case "init" : ctx.inline_type = Inline_Init; }
	{ abort("Invalid #inline qualifier: <%s>", qualifier); }
   }
}
private define c_inliner(line, ctx)
{
   if (line == NULL) {
	if (ctx.inline_type == Inline_Init)
	   SC.interface.inlines_init = [ SC.interface.inlines_init, ctx.buf ];
	else
	   SC.interface.inlines = [ SC.interface.inlines, ctx.buf ];
	ctx.buf = NULL;
   }
   else if (ctx.buf == NULL) {
	ctx.buf = EMPTY;
	if (string_match(line, inline_qualifiers, 1))
	   parse_inline_qualifiers(ctx, strtok(get_match(line, 1)));
   }
   else 
	ctx.buf = strcat(ctx.buf, line);
} % }}}
%  }}}

%  Argument mapping interface {{{

private variable argmap_open ="^[ \t]*(\\([a-zA-Z0-9,:\*=_$ \t\"\\[\\]]+\\))\\(.*\\)$";

% These patterns strip enclosing parens when called via get_match(s, N > 0)
private variable single_param_no_vars = "^[^()]+$";
private variable single_param_with_vars = "^\\([^()]+\\)[ \t]*(\\(.+\\))[ \t]*$";
private variable multi_param_no_vars = "^(\\([^()]+\\))[ \t]*$";
private variable multi_param_with_vars = "^(\\(.+\\))[ \t]*(\\(.+\\))[ \t]*$";
private variable which_qualifier
  = "[ \t]*which[ \t]*=[ \t]*[\\[]?[ \t]*\\([0-9]+[,:0-9]*\\)[ \t]*[\\]]?";
private variable usage_qualifier = "[ \t]*usage[ \t]*=[ \t]*\\([^,)]+\\)";
private variable proxy_qualifier = "[ \t]*proxy[ \t]*=[ \t]*\\([^,)]+\\)";

private define string_literal(str) % {{{
{
   variable lit = strtrans(str, "\"","");
   if (lit == str) {
	!if (is_defined(lit))
	   abort("#argmap reference to undefined name: <%s>", lit);
	eval( sprintf("%s;",lit) );
	lit = string(());
   }
   strtrim(lit);
} % }}}

private define parse_amap_qualifiers(argmap, toks) % {{{
{
   if (length(toks) == 1) return;

   variable ntoks = length(toks), toknum = 1;

   while (toknum < ntoks)  {

	variable qualifier = toks[toknum];

	if (is_substr(qualifier, "which")) {
	   % Any legal S-Lang array syntax may be used to specify the 'which'
	   % qualifier, which means we may have to re-compact some tokens
	   if (is_substr(qualifier, "[")) {
		while(qualifier[-1] != ']') {
		   toknum++;
		   qualifier = strcat(qualifier, ",", strtrim(toks[toknum]));
		}
	   }
	   if (string_match(qualifier, which_qualifier, 1))
		argmap.which = eval( sprintf("[%s];",
			 			get_match(qualifier, 1))) - 1;
	}
	else if (string_match(qualifier, "omit", 1))
	   argmap.which = [-1];			% drop all arguments

	else if (string_match(qualifier, usage_qualifier, 1))
	   argmap.usage = string_literal(get_match(qualifier, 1));

	else if (string_match(qualifier, proxy_qualifier, 1)) {
	   if (argmap.method != AM_In)
	   	abort("only #argmap(in) may establish a proxy");
	   argmap.proxy = string_literal(get_match(qualifier, 1)) + " proxy";
	   argmap.proxy = parse_args("#argmap( ,proxy)", argmap.proxy, 0) [0];
	   argmap.local_vars = [ argmap.local_vars , argmap.proxy ];
	}

	else
	   abort("Invalid argmap method qualifier: <%s>", qualifier);

	toknum++;
   }
} % }}}

define get_amap_paramlist(line, amap, allow_vars) % {{{
{
   variable c = string_match(line, "%", 1);	% Allow comments, but strip
   if (c) line = line[ [0:c-2]];

   if (string_match(line, single_param_no_vars, 1)) {
	amap.pattern = get_matchc(line, 0);
	if (amap.which == NULL) amap.which = 0;
   }
   else if (string_match(line, multi_param_no_vars, 1))
	amap.pattern = get_matchc(line, 1);
   else if (string_match(line, single_param_with_vars, 1)) {
	amap.pattern = get_matchc(line, 1);
	if (allow_vars)
	   amap.local_vars = [ amap.local_vars, 
			parse_args(_function_name, get_matchc(line, 2), 0) ];
	if (amap.which == NULL) amap.which = 0;
   }
   else if (string_match(line, multi_param_with_vars,  1)) {
	amap.pattern = get_matchc(line, 1);
	if (allow_vars)
	   amap.local_vars = [ amap.local_vars,
			parse_args(_function_name, get_matchc(line, 2), 0) ];
   }
} % }}}

define argmap_new(method, funcmap, codefrag) % {{{
{
   % This function simplifies the creation of argmaps from w/in SLIRP internals
   variable amap = @ArgMapping;
   amap.method = method;
   amap.funcmap = funcmap;
   amap.code = codefrag;

   variable maps_of_this_kind = funcmap.argmaps[method];
   if (maps_of_this_kind == NULL)
	maps_of_this_kind = ArgMapping[0];

   funcmap.argmaps[method] = [ amap, maps_of_this_kind];

   return amap;
} % }}}

private define argmap_create(line, amap) % {{{
{
   if (line == NULL) {						% finalize
	if (SC.debug)
	   dprintf("\targmap_create: #argmap(%S) %S",amap.method,amap.pattern);
	_argmaps[amap.method][amap.pattern] = @amap;
	nullify_struct(amap);
	return;
   }

   if (amap.code == NULL) {					% initialize

	amap.code = EMPTY;
	amap.local_vars = TypeMapping[0];

	if (string_match(line, argmap_open, 1)) {

	   variable tokenized_method_spec = strtok( get_matchc(line, 1), ",");
	   check_empty_qualifier_list(tokenized_method_spec, "#argmap");
	   amap.method = strtrim(tokenized_method_spec[0]);
	   line = trim_around( strtrim(get_matchc(line, 2)), ",");

	   switch(amap.method)
	   {
		case AM_Ignore:
		   preproc_allow_unterminated();
		   parse_amap_qualifiers(amap, tokenized_method_spec);
	   }
	   {
		case AM_Setup or
		case AM_In or
		case AM_Out or
		case AM_Final or
		case AM_Init:

		   parse_amap_qualifiers(amap, tokenized_method_spec);
	   }
	   { abort("Invalid argmap method: <%S>", amap.method); }

	   get_amap_paramlist(line, amap, 1);
	   if (amap.local_vars == NULL)
		abort("Variable of unknown type declared in: %s",line);

	}

	if (amap.method == NULL || amap.pattern == NULL)
	   abort("Invalid #argmap directive: <%S>",line);

	amap.pattern = group_pointer_qualifiers_with_type(amap.pattern);
	amap.args = parse_args("argmap", amap.pattern, SC.autotype);
	if (amap.args == NULL)
	   abort("Invalid '%s' argmap pattern: %s",amap.method,amap.pattern);

	amap.nargs = length(amap.args);

	if (amap.nargs > 1) {
	   if (amap.method == AM_Out)
		abort("#argmap(out) may only map a single parameter");
	   else if (amap.proxy != NULL and length(amap.which) > 1)
		abort("#argmap(in,proxy) may only map a single parameter");
	}

	if (amap.which == NULL)
	   amap.which = [0 : amap.nargs - 1];

	return;
   }

   amap.code = strcat(amap.code, line);		% continue normal execution
} % }}}

private define validate_single_line_directive(kind,line) % {{{
{
   variable amap = @ArgMapping;
   get_amap_paramlist(line, amap, 0);
   if (amap.pattern == NULL)
	abort("Invalid #%s directive: <%S>",kind, strtrim(line));
   return amap;
} % }}}

private define annot_clear(line) % {{{
{
   variable amap = validate_single_line_directive("clear",line);
   amap.pattern = group_pointer_qualifiers_with_type(amap.pattern);
   amap.pattern = trim_around(amap.pattern, ",");
   foreach(_argmaps) using ("values") {
	variable kind_of_argmaps = ();
	assoc_delete_key( kind_of_argmaps, amap.pattern);
   }
   assoc_delete_key( _retmaps, amap.pattern);
} % }}}

private define argmap_copy(line) % {{{
{
   !if (string_match(line, "^\\(.+\\)[ \t]*{\\(.+\\)}[ \t]*$", 1))
	abort("Invalid #copy directive: <%S>",line);

   variable src_pattern = strtrim( get_matchc(line, 1), "()");
   variable copies, dest_patterns = get_matchc(line, 2);

   if (is_substr(line, "(")) {
	% Multi-arg source pattern
	dest_patterns = strtok( get_matchc(line, 2), ")");
	copies = array_map(String_Type, &strtrim_beg, dest_patterns, "(, ");
   }
   else 
	copies = strtok( dest_patterns, ",");

   src_pattern = group_pointer_qualifiers_with_type(src_pattern);
   src_pattern = trim_around( src_pattern, ",");

   variable orig_args = parse_args("copy", src_pattern, 0);
   if (orig_args == NULL)
	abort("Bad source param list in #copy directive: <%S>",line);
   variable orig_nargs = length(orig_args);

   foreach(copies) {

	variable copy = ();

	% Destination types not mapped yet will be autotyped here
	variable copy_args = parse_args("copy", copy, 1);
	if (copy_args == NULL)
	   abort("Bad destination param list in #copy directive: <%S>",line);

	variable copy_nargs = length(copy_args);
	if (copy_nargs != orig_nargs)
	   abort("Mismatched param list sizes in #copy directive: <%S>",line);

	if (copy_nargs == 1) {
	   copy = copy_args[0];
	   variable type = copy.type + Referer[copy.cpp_ref];
	   if (copy.name == EMPTY)
		copy = strcat(copy.const,type);
	   else
		copy = strcat(copy.const,type," ",copy.name);
	}
	else {
	   copy = group_pointer_qualifiers_with_type(copy);
	   copy = trim_around(copy, ",");
	}

	foreach(_argmaps) using ("values") {
	   variable kind_of_argmaps = ();
	   variable orig = kind_of_argmaps[src_pattern];
	   if (orig != NULL) {
		orig = @orig;			% Keep original intact
		orig.usage = NULL;		% Never copy usage statements
		orig.args = copy_args;
		kind_of_argmaps[copy] = orig;
	   }
	}
   }
} % }}}

private variable arg_pat = "\\($\\([0-9]+\\)\\)[^_]";
private variable arg_meta_pat = "\\($\\([0-9]+\\)\\)_\\([^; \t)]+\\)";
private variable argnum_pat = "$argnum";
private variable argname_pat = "$argname";
private variable fname_pat = "$funcname";
private variable fnargs_pat = "$funcnargs";
private variable return_pat = "$return";
private variable cleanup_pat = "$cleanup";

define do_parameter_substitutions(argmap, cleanup_code) % {{{
{
   variable fmap = argmap.funcmap;
   variable code = argmap.code, argnum, argstr, meta;
   variable vectorized = fmap.vectorized and argmap.method == AM_Ret;

   forever {

	variable subst = NULL;

	if (string_match(code, arg_meta_pat, 1))
	{
	   argstr = get_match(code, 0);

	   argnum = integer(get_match(code, 2));
	   if (argnum > argmap.nargs)
		abort("Invalid argmap parameter: <%s>",argstr);
	   argnum--;

	   meta = get_match(code, 3);

	   variable arg = fmap.args[argmap.argnum + argnum - 1];

	   !if (strncmp(meta, "dim", 3)) {
		if (strlen(meta) >= 4)
		   subst = (@fmap.sizer) (arg, integer(char(meta[3])));
		else
		   subst = "1";
	   }
	   else !if (strncmp(meta, "length", 6))
		subst = (@fmap.sizer) (arg, 0);
	   else !if (strncmp(meta, "ndims", 5))
		subst = (@fmap.sizer) (arg, -1);
	   else !if (strncmp(meta, "type", 4))
	      	subst = arg.type;
	   else !if (strncmp(meta, "nullify", 7)) {
		subst = arg.lname;
	      	if (arg.typeclass == TYPECL_OPAQUE)
		   subst = strcat(subst,"_o->instance = NULL");
		else
		   subst = strcat(subst,"= NULL");
	   }
	   else
		abort("Invalid parameter metadata substitution: <%s>",argstr);
	}
	else if (string_match(code, arg_pat, 1))
	{
	   % This handles the LHS of assignments like $1 = ...
	   argstr = get_match(code, 1);
	   argnum = integer(get_match(code, 2)) - 1;
	   subst = argmap.args[argnum].lname;
	   if (vectorized)
	      subst += "[_viter]";
	}
	else if (string_match(code, argnum_pat, 1)) {
		argstr = argnum_pat;
		subst = string(argmap.argnum);
	}
	else if (string_match(code, argname_pat, 1)) {
		% $argname might be empty (function signatures
		% need not name their args) and thus may result
		% in the generation of uncompilable code
		argstr = argname_pat;
		subst = fmap.args[argmap.argnum-1].name;
	}
	else if (string_match(code, return_pat, 1)) {
		if (argmap.method != AM_Out)
		   abort("%s can only be used within 'out' argmaps",argstr);

		argstr = return_pat;
		arg = argmap.args[0];

		if (arg.returner != NULL)
		   subst = (@arg.returner) (arg);
		else
		   abort("%s type not supported by %s", arg.type, argstr);
	}
	else if (string_match(code, cleanup_pat, 1)) {
		argstr = cleanup_pat;
		subst = cleanup_code;
	}
	else if (string_match(code, fname_pat, 1)) {
		argstr = fname_pat;
		subst = sprintf("\"%S\"",fmap.name);
	}
	else if (string_match(code, fnargs_pat, 1)) {
		argstr = fnargs_pat;
		subst = sprintf("%d",fmap.nargs);
	}
	else
	   break;

	if (subst != NULL)
	   (code, ) = strreplace(code, argstr, subst, 1);
   }

   if (vectorized) {
	code = vec_loop_begin + "   " + code + vec_loop_end;
	if (argmap.which == -1)	
	   % Free retval when it's not passed back as return value of wrapper
	   code += "   SLfree((char*)retval);\n";
   }

   return code;
} % }}}
% }}}

%  Return value mapping interface {{{

private variable retmap_plain ="^[ \t]*\\([^()]+\\)$";
private variable retmap_with_qualifiers = "^[ \t]*(\\([^)]*\\))\\(.*\\)$";

private define parse_rmap_qualifiers(retmap, tokenized_qualifiers)
{
   check_empty_qualifier_list(tokenized_qualifiers, "#retmap");

   foreach (tokenized_qualifiers) {
	variable qualifier = strtrim(());
	switch(qualifier)
	{case "omit" : retmap.which = -1; }
	{ if (string_match(qualifier, usage_qualifier, 1)) {
		retmap.usage = string_literal(get_match(qualifier, 1));
		retmap.which = 0;
	  }
	  else
		abort("Invalid retmap method qualifier: <%s>", qualifier);
	}
   }
}

private define retmap_create(line, rmap)
{
   if (line == NULL) {						% finalize
	rmap.pattern = group_pointer_qualifiers_with_type(rmap.pattern);
	_retmaps[rmap.pattern] = @rmap;
	nullify_struct(rmap);
	return;
   }

   if (rmap.code == NULL) {					% initialize

      	rmap.method = AM_Ret;
	rmap.code = EMPTY;
	rmap.local_vars = TypeMapping[0];
	if (string_match(line, retmap_plain, 1)) {
	    rmap.pattern = get_matchc(line, 1);
	    rmap.which = 0;
	}
	else if (string_match(line, retmap_with_qualifiers, 1)) {
	   rmap.pattern = get_matchc(line, 2);
	   parse_rmap_qualifiers(rmap, strtok( get_matchc(line, 1), ",") );
	}

	if (rmap.pattern != NULL && rmap.which != NULL) {

	   % Allow comments, but strip
	   variable c = string_match(rmap.pattern,"[ \t]*%.*", 1);
	   if (c) rmap.pattern = rmap.pattern[ [0:c-2]];

	   rmap.args = parse_args("retmap", rmap.pattern, SC.autotype);
	}

	if (rmap.args == NULL)
	   abort("Invalid #retmap directive: <%S>",line);

	return;
   }

   rmap.code = strcat(rmap.code, line);		% continue normal execution
}
% }}}

private define type_cloner(line) % {{{
{
   variable amap = validate_single_line_directive("typedef",line);
   !if (parse_typedef(amap.pattern, 0))
	abort("Invalid #typedef clone attempt: <%S>",strtrim(line));
}

() = preproc_handler_add("prototype", , &func_prototyper, @Symbol_Context);
() = preproc_handler_add("vectorize", , &func_vectorizer, @Symbol_Context);
() = preproc_handler_add("novectorize", , &func_unvectorizer, @Symbol_Context);
() = preproc_handler_add("rename", EMPTY, &func_renamer);
() = preproc_handler_add("ignore", , &symbol_ignorer, @Symbol_Context);
() = preproc_handler_add("argmap", , &argmap_create, @ArgMapping);
() = preproc_handler_add("retmap", , &retmap_create, @ArgMapping);
() = preproc_handler_add("clear", EMPTY, &annot_clear);
() = preproc_handler_add("copy", EMPTY, &argmap_copy);
() = preproc_handler_add("typedef", EMPTY, &type_cloner);
() = preproc_handler_add("inline_c", , &c_inliner, @Symbol_Context);
() = preproc_handler_passthru("inline_c", "define");
() = preproc_handler_passthru("inline_c", "undef");
() = preproc_handler_passthru("inline_c", "include");
%  }}}

provide("slirpmaps");
