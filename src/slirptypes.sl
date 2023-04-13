% This file gets loaded from slirp_initialize()

% Basic C & C++ mappings {{{
slirp_map_string("char*");
slirp_map_char("char");
slirp_map_char("signed char");
slirp_map_uchar("unsigned char");
slirp_map_short("short");
slirp_map_short("signed short");
slirp_map_ushort("unsigned short");
slirp_map_int("int");
slirp_map_int("signed int");
slirp_map_int("signed");
slirp_map_uint("unsigned");
slirp_map_uint("unsigned int");
slirp_map_long("long");
slirp_map_long("long int");
slirp_map_llong("long long");
slirp_map_ulong("unsigned long");
slirp_map_ulong("unsigned long int");
slirp_map_ullong("unsigned long long");
slirp_map_float("float");
slirp_map_double("double");
slirp_map_ldouble("long double");
slirp_map_void(VOID);		% Handle funcs with void return types

slirp_map_string("string");

#if (SLIRP_HAVE_STDINT_H)
slirp_map_char("int8_t");
slirp_map_uchar("uint8_t");
slirp_map_int16("int16_t");
slirp_map_uint16("uint16_t");
slirp_map_int32("int32_t");
slirp_map_uint32("uint32_t");
slirp_map_int64("int64_t");
slirp_map_uint64("uint64_t");
#endif

slirp_config_map_types ();	       %  in slripconf.sl

% Typemap stubs, which facilitate use of proxies in argmaps below % {{{
slirp_map(SLFile_FD_Type, SLFile_FD_Type, "FD_Type", "SLANG_FILE_FD_TYPE",
      							TYPECL_POINTER);
slirp_map(SLang_Array_Type, SLang_Array_Type, "array", "SLANG_ARRAY_TYPE",
      							TYPECL_POINTER);
#argmap(out) short *OUTPUT
   $return;
#end

#copy short *OUTPUT { unsigned short *OUTPUT, int *OUTPUT, unsigned int *OUTPUT, unsigned *OUTPUT, long *OUTPUT, unsigned long *OUTPUT, float *OUTPUT, double *OUTPUT }

#copy short *OUTPUT { short *OUT, unsigned short *OUT, int *OUT, unsigned int *OUT, unsigned *OUT, long *OUT, unsigned long *OUT, float *OUT, double *OUT}
% }}}
% }}}

% FORTRAN mappings {{{

#if (SLIRP_HAVE_FORTRAN)
slirp_map_char("integer*1");
slirp_map_char("logical*1");
slirp_map_int16("integer*2");
slirp_map_int16("logical*2");
slirp_map_int("integer");
slirp_map_int("logical");
slirp_map_int32("integer*4");
slirp_map_int32("logical*4");
slirp_map_int64("integer*8");
slirp_map_int64("logical*8");
slirp_map_float("real");
slirp_map_float32("real*4");
slirp_map_float64("real*8");
slirp_map_double("double precision");
slirp_map_scomplex("spcomplex");	% C-scoped single-prec (float) complex
slirp_map_scomplex("complex");		% FORTRAN-scoped single-prec complex
slirp_map_dcomplex("dpcomplex");	% C-scoped double-prec complex
slirp_map_dcomplex("double complex");	% FORTRAN-scoped double-prec complex
slirp_map_string("character");

#copy short *OUTPUT { spcomplex *OUTPUT, spcomplex *OUT }
#copy short *OUTPUT { dpcomplex *OUTPUT, dpcomplex *OUT }

% Fortran <--> C string and string array handling %% {{{

slirp_map_string("FTN_STR_ARR");

#argmap(in, proxy=SLang_Array_Type) FTN_STR_ARR
   proxy->client_data = NULL;
   if (pack2for(proxy, &$1, &arg$argnum_len) != 0)
	return;
#end

#argmap(out) FTN_STR_ARR OUTPUT
   SLang_push_array(proxy, 1);
#end

#argmap(init) FTN_STR_ARR OUTPUT (SLang_Array_Type *proxy)
   if ( NULL == (proxy = SLang_create_array(SLANG_STRING_TYPE, 0,
			NULL, arg$argnum_dims, NDIMS(arg$argnum_dims))))
	return;

   if ( ($1 = SLmalloc( proxy->num_elements * arg$argnum_len)) == NULL) {
	SLang_free_array(proxy);
	return;
   }
   memset($1, ' ', proxy->num_elements * arg$argnum_len);
   proxy->client_data = proxy->data;
#end

#argmap(final) FTN_STR_ARR
   unpack2c($1, proxy, arg$argnum_len);
   SLfree($1);
#end

#endif				       %  FORTRAN
%  }}}

% }}}

% Integer file descriptor proxy mapping % {{{
% This allows the S-Lang FD_Type to be passed to compiled routines
% expecting an integer file descriptor.

#argmap(in, proxy=SLFile_FD_Type) int FD_PROXY
   if (-1 == SLfile_get_fd (proxy, &$1)) {
	SLang_verror(SL_INTRINSIC_ERROR, "could not assign file descriptor proxy");
	return;
   }
#end
% }}}

% void* proxy mappings (Array_Type, Any_Type) % {{{

variable vt = @SC.types[VOID];	% Create temporary void* type mapping,
vt.type += "*";			% just to evaluate proxy annotations
vt.ltype = SLang_Array_Type;
SC.types[vt.type] = vt;

#argmap(in, proxy=SLang_Array_Type) void* ARRAY_2_VOIDP
   $1 = proxy->data;
#end

slirp_map(SLang_Any_Type, SLang_Any_Type, "anytype", "SLANG_ANY_TYPE",
      								TYPECL_POINTER);

#argmap(in, proxy=SLang_Any_Type) void* ANY_2_VOIDP
   $1 = proxy;
#end

% }}}

#argmap(in, proxy="char*") string % C++ string proxy {{{
   $1 = proxy;
#end
% }}}

#ifeval SC.cfront % C++ string array handling
#argmap(ignore) string* % not wrapped in cfront mode {{{
   % Routines with signatures containing an array of string objects
   % are currently ignored in cfront mode. A better approach would be
   % to enable annotations to be applied while in cfront mode (and
   % allow char** to be used as a proxy for string*, similar to the
   % S-Lang mode), but that is not feasible yet.
#end % }}}
#else
#argmap(in, proxy=SLang_Array_Type) string* % use proxy {{{
   {
   int i, l;
   char **arr;
   if (proxy) {
	if (proxy->data_type != SLANG_STRING_TYPE) {
	   SLang_verror(SL_USAGE_ERROR, (char*)"String array is required here");
	   return;
	}
	arr = (char**)proxy->data;
	l = proxy->num_elements;
	$1 = new string[l];
	for (i=0; i<l; i++)
	   $1[i] = string(arr[i]);
   }
   else
	$1 = NULL;
   }
#end
% }}}
#argmap(final) string* % C++ string array finalizer {{{
   delete [] $1;
#end
% }}}
#endif

% Mappings for NULL-terminated string arrays {{{
#typedef char** NT_STR_ARRAY
#typedef char** NT_STR_ARRAY_FREE

#retmap  NT_STR_ARRAY
   push_null_term_str_array($1, $funcname, 0);
#end

#retmap  NT_STR_ARRAY_FREE
   push_null_term_str_array($1, $funcname, 1);
#end
% }}}

assoc_delete_key(SC.types, VOID+"*");		% discard VOID stub
