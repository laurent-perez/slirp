% SLIRP: The (Sl)ang (I)nte(r)face (P)ackage (a code generator for S-Lang) {{{
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
%  WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% }}}

require ("slirpconf.sl");
require("slirpmaps.sl");
require("slirpc++.sl");

% Support functions, iterators, etc {{{

private define emit_constants(consts, kind)
{
   !if (length(consts)) return;

   variable name, value;
   slirp_emit("static SLang_%sConstant_Type %s_%sConsts [] =\t%s\n{\n", kind,
						SC.modname, kind, FOLD_OPEN);
   foreach(consts) using ("keys","values") {
	(name, value) = ();
	if (typeof(value) == TypeMapping)
	    value = name;
	slirp_emit("   MAKE_%sCONSTANT((char*)\"%s\",%s),\n", kind, name, value);
   }

   slirp_emit("   SLANG_END_%sCONST_TABLE\n};\t%s\n\n", kind, FOLD_CLOSE);
}

private define emit_stubbed_global_vars(interface)
{
   variable name, value;
   foreach(interface.intrin_vars) using ("keys", "values") {
	(name, value) = ();
	slirp_emit("%s %s;\n", value.type, name);
   }
}

private define emit_vec_macros(nargs, longname, shortname, prefix_with_nargs)
{
   if (prefix_with_nargs)
	slirp_emit("#define %s_%d  %d,", longname, nargs, nargs);
   else
	slirp_emit("#define %s_%d  ", longname, nargs);

   variable expansion = array_map(String_Type, &sprintf, "%s(%d)",
	 						shortname, [1:nargs]);

   slirp_emit("%s\n", strjoin(expansion, ","));
}

private define emit_vectorization_code()
{
   slirp_emit("\n/* Vectorization types and macros {{{ */\n");

   inject_file("vectorize.h");

   array_map(Void_Type, &emit_vec_macros, [1:SLIRP_MAX_VEC_ARGS], "VREF",  "VR", 1);
   array_map(Void_Type, &emit_vec_macros, [1:SLIRP_MAX_VEC_ARGS], "VINCR", "VI", 0);

   slirp_emit("/* }}} */\n");

   inject_file("vectorize.c");
}

private define emit_intrinsic_vars(interface)
{
   variable name, value, i = 0, sarray_name;
   if (length(interface.string_consts)) {

	sarray_name = SC.modname + "_strings";

	slirp_emit("static const char *%s[] = {\t%s\n", sarray_name, FOLD_OPEN);

	foreach(interface.string_consts) using ("values") {
	   value = ();
	   slirp_emit("   %s,\n", value);
	}

	slirp_emit("NULL\n};\t%s\n\n", FOLD_CLOSE);
   }
   else !if (length(interface.intrin_vars))
	return;

   slirp_emit("\nstatic SLang_Intrin_Var_Type %s_IVars [] = %s\n{\n",
	      SC.modname, FOLD_OPEN);
   foreach(interface.string_consts) using ("keys") {
	name = ();
	slirp_emit("   MAKE_VARIABLE((char*)\"%s\",&%s[%d], SLANG_STRING_TYPE,1),\n",
	      					name, sarray_name, i);
	i++;
   }

   variable arrays = Struct_Type[0];
   variable opaques = List_Type[0];

   foreach(interface.intrin_vars) using ("keys", "values") {
	(name, value) = ();

	% At present we wrap only 1D arrays & scalars (basic C & opaque types)
	if (value.dim == 1)
	   arrays = [ arrays, value ];
	else if (value.typeclass == TYPECL_OPAQUE)
	   opaques = [ opaques, {name, value} ];
	else
	   slirp_emit("   MAKE_VARIABLE((char*)\"%s\",&%s, %S, 1),\n", name,
						value.name, value.typeid);
   }

   slirp_emit("   SLANG_END_INTRIN_VAR_TABLE\n};\t%s\n\n", FOLD_CLOSE);

   if (length(opaques)) {

#iffalse
	inject_file("opivars.c");

	slirp_emit("\nstatic SLang_Intrin_Var_Type %s_Opaque_IVars [] = %s\n{\n",
							SC.modname, FOLD_OPEN);

	foreach value (opaques)
	   slirp_emit("   MAKE_OVAR(\"%s\",&%s, %S, 1),\n", value[0],
	   				value[1].name, value[1].typeid);

	slirp_emit("   SLANG_END_INTRIN_VAR_TABLE\n};\t%s\n\n", FOLD_CLOSE);
#else
      vmessage ("Not generating wrappers for:");
      	foreach value (opaques)
	{
	   vmessage (" %S (%S)", value[0], value[1].name);
	}
#endif
   }

   !if (length(arrays)) return;

   slirp_emit("static int make_intrinsic_arrays(SLang_NameSpace_Type *ns)\n{");

   foreach(arrays) {
	variable arr = ();
	slirp_emit("\n   if (-1 == SLang_add_intrinsic_array((char*)\"%s\", %s, 1,\n"+
	      "\t\t%s, 1, sizeof(%s) / sizeof(%s)))\n"+
		"\treturn -1;",
		arr.name, arr.aux.typeid, arr.name, arr.name, arr.aux.type);
   }
	    
   slirp_emit("\n   return 0;\n}\n");
   interface.intrin_arrays = arrays;		% hint to generate_module_init
}

private define emit_usage_strings()
{
   !if ( length(SC.usage_strings)) return;
   slirp_emit("static const char* usage_strings[] = { %s\n", FOLD_OPEN);
   array_map(Void_Type, &slirp_emit, "   \"%s\",\n", SC.usage_strings);
   slirp_emit("NULL\n}; %s\n\n", FOLD_CLOSE);
   inject_file("usage.c");
   slirp_emit("\n");
}

% }}}

% Usage statements {{{

private variable Commas = [ EMPTY, ","];

private define finalize_usage(args)
{
   variable usages = String_Type[0], usage;

   foreach(args) {

	variable arg = ();

	if (arg.usage == NULL) {

	   usage = arg.mnemonic;

	   if (arg.arrayspec != EMPTY)
		usage += arg.arrayspec;
	   else loop (arg.dim)
		usage += "[]";

	   if (arg.defval != EMPTY)
		usage += "=" + arg.defval;
	}
	else
	   usage = arg.usage;

	usages = [ usages, usage];
   }
   return strjoin(usages, ",");
}

private define generate_usage_statement(fmap)
{
   variable outputs = TypeMapping[0];

   debug_emit ("/* start generate_usage_statement */\n");

   foreach (fmap.argmaps[AM_Out]) {

	variable amap = ();
	if (amap.which < 0) continue;		% an omitted return value

	variable arg = @amap.args[amap.which];

	% C pointers changed from an argument list parameter to an output
	% value effectively have their indirection reduced by one (e.g. a
	% double* becomes a double scalar), which we reflect by setting
	% the usage to that of the type being pointed to.  This will not
	% be done for vectorized functions, though, under the assumption
	% that an arrays coming in must be reflected as arrays going out.
	if (arg.typeclass == TYPECL_POINTER && not arg.marshal &&
							not fmap.vectorized)
	   arg.usage = arg.aux.mnemonic;

	outputs = [arg, outputs ];
   }

   variable num = length(outputs);
   outputs = finalize_usage(outputs);
   if (outputs != EMPTY)
	if (num > 1)
	   outputs = sprintf("(%s) = ",outputs);
	else
	   outputs = strcat(outputs, " = ");

   variable args = fmap.args, narg = "SLang_Num_Function_Args";
   variable inputs = where( struct_map(Integer_Type, args, "marshal"));
   variable defaults = struct_map(String_Type, args[inputs], "defval");
   variable optional = where ( defaults != EMPTY);
   variable num_optional = length(optional);
   variable minimum_nargs = length(inputs) - num_optional; 

   if (num_optional) {
	num = minimum_nargs;
	narg = sprintf("%s < %d || %s > %d", narg, num, narg, num+num_optional);
	optional = sprintf(" [%s%s]", Commas[ (num > 0)],
				finalize_usage(args[inputs][optional]));
	optional = str_quote_string(optional, "\"",'\\');
   }
   else {
	num = length(inputs);
	narg = sprintf("%s != %d", narg, num);
	optional = EMPTY;
   }

   if (num > 0) {
	args = args[inputs];
	args = finalize_usage(args[[0:num-1]]);
   }
   else
	args = EMPTY;

   if (fmap.retval.retstruct != NULL)
     {	
	variable outs = {};
	foreach arg (fmap.args)
	  if (arg.retstruct)
	    list_append (outs, arg.deref_type);
	if (length (outs) < 2)
	  outputs = list_pop (outs) + " = ";
	else
	  outputs = "(" + strjoin (list_to_array (outs, String_Type), ", ") + ") = ";
     }
   
   variable usg = sprintf("%s%s(%s)", outputs, fmap.slname, args+optional);
   SC.usage_strings = [SC.usage_strings, usg ];

   debug_emit ("/* end generate_usage_statement */\n");
   return ( minimum_nargs, narg, usg);

} % }}}

private define emit_declaration_block(fmap)	% {{{
{
   debug_emit ("/* begin emit_declaration_block */\n");
   EXIT_BLOCK
     {
	slirp_emit ("   int issue_usage = 1;\n");
	debug_emit ("/* end emit_declaration_block */\n");
     }

   variable r = fmap.retval;

   if (r.ltype != VOID) {
	variable rtype = r.type;
	if (fmap.vectorized) rtype += "*";
	slirp_emit("   %s%s %s;\n", r.const, rtype, r.lname);
   }

   foreach(fmap.local_vars) {

	variable type, var = (), init = EMPTY, arrspec = var.arrayspec;

	if (arrspec == EMPTY) {
	   if (var.typeclass == TYPECL_POINTER)
		init = " = NULL";
	   type = var.type;
	}
	else   
	   type = var.deref_type;

	slirp_emit("   %s %s%s%s;\n", type, var.lname, arrspec, init);
   }

   !if (fmap.pop_args) return;

   foreach(fmap.args) {  variable arg = (); slirp_emit ("%s", (@arg.declarer)(arg)); }

   if (fmap.vectorized) {
	slirp_emit("   ");
	if (not SC.openmp)
	   slirp_emit("unsigned ");
	slirp_emit("int _viter;\n   VecSpec vs = {1, %d, 0};\n", SC.openmp);
   }
} % }}}

% Argmap emission {{{ 
private define emit_argmap_codefrag(argmap, cleanup)
{
   % Use %s to ensure formats w/in fragment are not interpreted by S-Lang
   variable s = do_parameter_substitutions(argmap, cleanup);
   slirp_emit("%s", s);
#iffalse
   if (s != "")
     {
	vmessage ("***CODEFRAG: %s --> %s", cleanup, s);
     }
#endif
}

private define emit_argmap_code_fragments(fmap, kind, cleanup)
{
   debug_emit ("/* begin emit_argmap_code_fragments */\n");
   foreach(fmap.argmaps[kind])
	emit_argmap_codefrag((), cleanup);
   debug_emit ("/* end emit_argmap_code_fragments */\n");
}
% }}}

private define generate_marshaling_code_ORIG(fmap)	% {{{
{
   variable mc = EMPTY, refs = Struct_Type[0];

   if (fmap.pop_args) {

	variable arg, argno = 0;

   	foreach(fmap.args) {

	   arg = ();

	   !if (arg.marshal) continue;
	   if (arg.proxy != NULL) arg = arg.proxy;

	   % Accumulate refs here: avoids having an args loop in cleanup()
	   if (arg.dim)
		refs = [ refs, arg ];

	   argno++; % arg #s begin with 1
	   mc = sprintf("%s == -1 ||\n\t%s", (@arg.marshaler)(arg, argno), mc);
	}

	if (mc != EMPTY) {
	   (mc, ) = strreplace(mc, "||\n\t","",-1);
	   mc = sprintf(" ||\n\t%s",mc);
	}
   }

   return refs, mc;

} % }}}

private define generate_marshaling_code (fmap)	% {{{
{
   variable marshal_list = {}, refs = Struct_Type[0], cleaner_list = {};

   if (fmap.pop_args) {

	variable arg, argno = 0;
   	foreach(fmap.args) {

	   arg = ();

	   !if (arg.marshal) continue;
	   if (arg.proxy != NULL) arg = arg.proxy;

	   % Accumulate refs here: avoids having an args loop in cleanup()
	   if (arg.dim)
		refs = [ refs, arg ];

	   argno++; % arg #s begin with 1
	   variable m = arg.marshaler(argno);
	   list_insert (marshal_list, sprintf ("-1 == %s", m));
	   list_insert (cleaner_list, arg.cleaner (argno));
	}
   }

   return refs, marshal_list, cleaner_list;

} % }}}

private define emit_call_block(fmap, vectorization)	% {{{
{
   debug_emit ("/* begin emit_call_block */\n");
   if (fmap.referer_hook == NULL) {

	slirp_emit("   ");

	% Fabricate call to underlying function/class method/subroutine/etc
	variable argno = 0, fcall = (@fmap.referer) (fmap, &argno) + "(";
	foreach(fmap.args[[argno:]]) {
	   variable arg = ();
	   arg = (@arg.referer)(arg, 0, fmap.pop_args);
	   fcall = strcat(fcall, arg, ARG_SEPARATOR);
	}
	fcall = strtrim_end(fcall, ARG_SEPARATOR) + ")";
	slirp_emit( sprintf("%s%s;\n", standard_return_mechanism(fmap), fcall) );
   }
   else
	(@fmap.referer_hook)(fmap, vectorization);

   debug_emit ("/* end emit_call_block */\n");
} % }}}

private define emit_return_block(fmap, cleanup, cleaner_list, usage_stmt)	% {{{
{
   debug_emit ("/* begin emit_return_block */\n");

   emit_argmap_code_fragments(fmap, AM_Final, cleanup);

   variable outmaps = fmap.argmaps[AM_Out];		% Length check needed
   if (outmaps != NULL && length(outmaps))	% for S-Lang1
	array_map(Void_Type, &emit_argmap_codefrag, reverse(outmaps), cleanup);

   if (fmap.retval.retstruct != NULL)
     {	
	variable arg;
	foreach arg (fmap.args)
	  {	     
	     if (arg.retstruct)
	       slirp_emit ("   %s", (@arg.returner)(arg));
	  }	
     }
   
   slirp_emit ("   goto free_and_return;\nfree_and_return:\n");

   if (fmap.pop_args) {

      variable emit_drop = 1;
      foreach (cleaner_list)
	{
	   variable statement = ();
	   if (statement == NULL)
	     continue;
	   if (emit_drop)
	     {
		slirp_emit ("   /* drop */\n");
		emit_drop = 0;
	     }
	   slirp_emit ("%s", statement);
	}

      if (emit_drop) slirp_emit ("   /* drop */\n");
      slirp_emit ("usage_label:\n");
      if (usage_stmt != NULL)
	slirp_emit ("%s", usage_stmt);

      if (cleanup != EMPTY)
	{
	% Refs are finalized en-masse: both here when the routine exits
	% cleanly and, to avoid leaks, when it signals an error (earlier)
	   debug_emit ("/* begin cleanup */\n");
	   slirp_emit("   %s\n", cleanup);
	   debug_emit ("/* end cleanup */\n");
	}
   }
   else if (usage_stmt != NULL)
     {
	slirp_emit ("   /* drop */\n");
	slirp_emit ("usage_label:\n");
	slirp_emit ("%s", usage_stmt);
     }

   debug_emit ("/* end emit_return_block */\n");
} % }}}

private define emit_function_table_entry(fmap) % {{{
{
   if (fmap.pop_args || fmap.nargs == 0) {
	slirp_emit("   MAKE_INTRINSIC_0((char*)\"%s\",%s,SLANG_VOID_TYPE),\n",
	      					fmap.slname, fmap.gname);
	return;
   }

   % We will override the signature of the underlying C function, by
   % assigning a void arg list to the ftable entry, if the C func
   % signature includes one or more of the following:
   %
   %	struct : in order to to use struct layout functions 
   %	ref    : in order to treat refs and arrays uniformly
   %
   % Instead of relying upon the built-in S-Lang-to-C arg transfer
   % mechanism, we generate additional code to explicitly pop args
   % off the stack, and gain the ability to perform the appropriate
   % finalization (e.g. free an mmt) prior to returning from the
   % glue layer.

   variable args = String_Type[0];
   foreach (fmap.args) {

	variable arg = ();

	switch (arg.ltype)
	{ case SLang_Array_Type: arg = "SLANG_ARRAY_TYPE"; }
	{ arg = arg.typeid; }

	args = [ args, arg];
   }
   slirp_emit("   MAKE_INTRINSIC_%d((char*)\"%s\", %s, SLANG_VOID_TYPE, %s),\n", fmap.nargs,
				fmap.slname, fmap.gname, strjoin(args,","));
}

% }}}

% Wrapper function emission {{{

private define emit_header_includes()
{
   variable h = SC.input_headers;
   h = array_map(String_Type, &path_basename, h);
   slirp_emit( strjoin(array_map(String_Type, &sprintf, "#include \"%s\"\n", h),""));
}

private define create_ref_cleanup_code (fmap, info)
{
   if (fmap.vectorized)
	return sprintf("finalize_refs(%s); ", info.refs);

   variable refs = info, nrefs = length(refs);
   if (nrefs) {
	refs = struct_map(String_Type, refs, "lname");
	refs = array_map(String_Type, &sprintf, "%s_r", refs);
	return sprintf("finalize_refs(%d,%s); ", nrefs, strjoin(refs, ","));
   }

   return EMPTY;
}

private define emit_wrapper (fmap, usage_index, num_funcs_with_same_name)
{
   variable proto = generate_prototype(fmap);
   variable arg_info = EMPTY;
   variable min_nargs = 0;

   if (SC.interface.prototypes[proto] != NULL)
      return 0;					% prohibit duplicate wrappers

   if (fmap.prototype_hook != NULL)
	(@fmap.prototype_hook) (fmap);

   slirp_emit(proto);
   slirp_emit("\n{\n");

   SC.interface.prototypes [ proto ] = EMPTY;

   if (SC.cfront)
     {
	emit_call_block(fmap, arg_info);
	slirp_emit("}\n\n");
	return min_nargs;
     }

   variable nargs_test, usg, marshal_code, cleaner_list;

   emit_declaration_block(fmap);

   if (fmap.vectorized) {
      arg_info = finalize_vectorization(fmap);
      (, marshal_code, cleaner_list) = generate_marshaling_code(fmap);
   }
   else
     (arg_info, marshal_code, cleaner_list) = generate_marshaling_code(fmap);

   % Usage statements are generated even for zero-arg functions; its
   % preferable to leaving crud on the stack when they're called with
   % too many args, as well as didactic for the case when an annotation
   % causes none of the C function arguments to be visible from S-Lang
   (min_nargs, nargs_test, usg) = generate_usage_statement(fmap);

   variable cleanup_code = create_ref_cleanup_code (fmap, arg_info);

   emit_argmap_code_fragments(fmap, AM_Setup, cleanup_code);

   debug_emit ("/* begin slirp usage code fragment */\n");

   variable usage_stmt
     = sprintf ("   if (issue_usage) Slirp_usage (%d, %d, %d);\n",
		usage_index,
		usage_index + num_funcs_with_same_name,
		SC.openmp | fmap.vectorized);

   variable label = "usage_label";
   slirp_emit ("\n   if (%s) goto %s;\n", nargs_test, label);

   variable n = length (marshal_code), i;
   _for i (0, n-1, 1)
     {
	slirp_emit ("   if (%s) goto %s;\n", marshal_code[i], label);

	if (cleaner_list[i] != NULL)
	  {
	     variable j = n - i;
	     if (j != 1)
	       {
		  label = sprintf ("free_and_return_%d", j);
		  cleaner_list[i] = sprintf ("%s:\n   %s", label, cleaner_list[i]);
	       }
	     else cleaner_list[i] = sprintf ("   %s", cleaner_list[i]);
	  }
     }
   list_reverse (cleaner_list);
   slirp_emit ("   issue_usage = 0;\n\n");
   debug_emit ("/* end slirp usage code fragment */\n");

   emit_argmap_code_fragments(fmap, AM_In, cleanup_code);
   emit_argmap_code_fragments(fmap, AM_Init, cleanup_code);

   emit_call_block(fmap, arg_info);

   emit_return_block(fmap, cleanup_code, cleaner_list, usage_stmt);

   slirp_emit("}\n\n");
   return min_nargs;
}

private define funcmap_2_wrapper(fmap)
{
   variable overloads = length(fmap.overloads), dispatch_entry = @List;
   variable usage_index = length(SC.usage_strings), vectorized = 0;

   foreach(fmap.overloads) {

	variable o = (), types;

	if (length(o.args)) {
	   types = map_args_to_type_abbrevs(o.args);
	   types = strjoin(types, EMPTY);
	   o.gname = o.slname + "_" + types;
	}
	else {
	   types = EMPTY;
	   o.gname = o.slname + "_v";
	}

	vectorized |= o.vectorized;
	variable name = SC.wrapper_prefix + o.gname;

	variable min_nargs = emit_wrapper(o, usage_index, overloads);

	_list_append(dispatch_entry, list_node_new(name, {types, min_nargs} ));
   }

   if (overloads)
	emit_dispatcher(fmap, dispatch_entry, usage_index, vectorized);
   else
	() = emit_wrapper(fmap, usage_index, 0);
}

private define emit_wrappers()
{
   slirp_emit("/* Wrapper functions */ %s\n", FOLD_OPEN);
   if (SC.cplusplus && not SC.cfront)
	slirp_emit("static void dispatch (int first, int stop, int usage, int v);\n");

   iterate_over_functions(SC, &funcmap_2_wrapper);
   iterate_over_classes(&iterate_over_functions, &funcmap_2_wrapper);

   invoke_callbacks(PRE_FUNC_TABLE_CB);

   if (SC.cfront) return;

   slirp_emit("\nstatic SLang_Intrin_Fun_Type %s_Funcs [] =\n{\n", SC.modname);

   iterate_over_functions(SC, &emit_function_table_entry);
   iterate_over_classes(&iterate_over_functions, &emit_function_table_entry);

   invoke_callbacks(IN_FUNC_TABLE_CB);

   slirp_emit("   SLANG_END_INTRIN_FUN_TABLE\n};   %s\n\n", FOLD_CLOSE);
}

private define emit_cfront_style_wrappers()
{
   emit_header_includes;

   inject_file("c++cfront.h");

   slirp_emit("#include \"%s_glue.h\"\n", SC.modname);
   slirp_emit("#define INVOKE_METHOD(type,obj,method) ((type*)(obj))->method\n");
   slirp_emit("#define OBJECT(o)		o\n\n");
   emit_wrappers();
   iterate_over_classes(&emit_destructor);

   close_file(SC.outfp);

   (SC.outfp, ) = create_output_file("_glue",".h");

   slirp_emit("#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n");
   foreach(SC.interface.prototypes) using ("keys") {
	variable proto = ();
	slirp_emit( strcat("extern ", proto, ";\n"));
   }
   slirp_emit("\n#ifdef __cplusplus\n}\n#endif\n");

   close_file(SC.outfp);

   return;
}
% }}}

private define generate_code() % {{{
{
   if (SC.cfront && not SC.cplusplus)
	abort("C wrappers not generated, since no C++ code was processed.");

   if (SC.genstubs) {
	(SC.outfp, SC.stubs_fname) = create_output_file("_stubs");
	% Ensure stubs code appears as dependency in generated makefile content
	SC.obj_code = [ SC.obj_code, path_sans_extname(SC.stubs_fname) + ".o" ];

	emit_header_includes;
	emit_stubbed_global_vars(SC.interface);
	generate_stubs(SC.interface);
	foreach(SC.classes) using ("values") {
	   variable class = ();
	   generate_stubs(class.interface);
	}
	close_file(SC.outfp);
	return;
   }

   (SC.outfp, ) = create_output_file("_glue");

#if (SLIRP_HAVE_LONG_LONG)
   slirp_emit("#define HAVE_LONG_LONG 1\n");
#endif

   inject_file("copyright.h");

   if (SC.cfront)
	return emit_cfront_style_wrappers();

   inject_file("intro.h");

   if (SC.fortran_wrapped)  {
	SC.have_refs |= 0x2;
	slirp_emit("/* General Fortran support */ %s\n", FOLD_OPEN);
	inject_file("fortran.h");	% keep .h and .c separate
	inject_file("fortran.c");
	if (SC.fortran_wrapped > 1)
	   inject_file("pack.c");	% convert arrays: fortran <--> slstring
	slirp_emit("%s\n\n", FOLD_CLOSE);
   }

   if (SC.include_headers)
	emit_header_includes;

   if (SC.cplusplus) {
	slirp_emit("#define OBJECT(o)			SAFE_DEREF_OPAQUE(o)\n");
	slirp_emit("#define BEGIN_DECLS			extern \"C\" {\n"+
	     "#define END_DECLS			}\n");
	iterate_over_classes(&emit_destructor);
   }
   else
	slirp_emit("#define BEGIN_DECLS\n"+
	     "#define END_DECLS\n");

   if (SC.gendebug)
	inject_file("debug.c");

   invoke_callbacks(PRE_GEN_CB);

   variable num_new_opaques = generate_opaque_code();
   variable vectorized = SC.vectorize or length(try_vectorize);

   if (vectorized)
	SC.have_refs |= 0x3;

   % Conditional file inclusions
   if (SC.have_refs & 0x1) {

	slirp_emit("static unsigned char map_scalars_to_refs = %d;\n",
						SC.have_refs & 0x2);
	inject_file("refs.c");
   }

   if (vectorized)
	emit_vectorization_code();

   if (SC.have_null_term_str_arrays)
	inject_file("ntstrarr.c");

   if (SC.tmap_outfile != NULL)
	emit_typemaps(SC.tmap_outfile);

   emit_inlines(SC.interface.inlines);
   slirp_emit("\n");

   emit_wrappers();
   emit_usage_strings();
   emit_constants(SC.interface.int_consts,"I");
   emit_constants(SC.interface.long_consts,"L");
   emit_constants(SC.interface.double_consts,"D");
   emit_intrinsic_vars(SC.interface);

   invoke_callbacks(POST_GEN_CB);

   if (SC.geninit)
	generate_module_init(num_new_opaques);

   close_file(SC.outfp);

} % }}}

private define parse_c_or_cpp(fp) % {{{
{
   %while( not feof(fp) )	FIXME: work around slang 2.2 fread() + EOF bug
   while( not SC.eof )
     {
	if (discard_line (fp))
	  continue;
	
	if (macro_or_type_definition(fp, NULL))
	  continue;

	if (cplusplus_class(fp))
	  continue;

	if (variable_declaration(fp))
	  continue;

	function_declaration(fp, NULL, not(SC.cplusplus));
     }
   () = fclose(fp);
}
SC.default_file_parser = &parse_c_or_cpp;
% }}}

private define parse_files() % {{{
{
   variable num_files_processed = 0, output_to_stdout = SC.genstem == "stdout";

   while (SC.argc < __argc) {

	SC.infp = init_file_parser(__argv[SC.argc]);
	if (SC.infp == NULL) { SC.argc++; continue; }

	(@SC.parse_file) (SC.infp);

	num_files_processed++;

	SC.argc++;

	if (output_to_stdout) continue;

	if (num_files_processed > 1) {
	   () = printf(".");		% Output progress indicator
	   () = fflush(stdout);
	}
   }

   % Late bind macros which reference symbols undefined when they were seen
   determine_unresolved_macro_disposition(SC.interface);

   if (num_files_processed > 1) () = printf("\n");

   return num_files_processed;
} % }}}

define slsh_main()  % {{{
{
   slirp_initialize();

   if (parse_files()) {

	if (SC.print_interface)
	   print_public_interface();
	else
	   generate_code();

	if (SC.ignore_fp != NULL && SC.outfp != stdout)
	   tprintf("\nCatalog of ignored symbols written to %S", SC.ignore);
   }

   invoke_callbacks(EXIT_CB);
   exit(0);
} % }}}
