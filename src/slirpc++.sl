%  slirpc++.sl:  Supports generation of S-Lang wrappers for C++ {{{
%
%  This file is part of SLIRP, the (Sl)ang (I)nte(r)face (P)ackage.
%
%  Copyright (c) 2003-2009 Massachusetts Institute of Technology
%  Copyright (C) 2002 Michael S. Noble <mnoble@space.mit.edu> }}}

require("slirpmaps");

% Front matter: type/variable/forward declarations, etc {{{ 

typedef struct {
   name,
   ancestors,
   constructors,
   destructor,
   public_fields,
   abstract,
   nesting,
   interface,		% class-scoped public interface
} Class;

private variable In_Public_Interface;
private define parse_cplusplus_block();
private define clone_class();
private variable Dispatch_Table = {};

% }}}

% Overloaded type mnemonics % {{{

define map_args_to_type_abbrevs(args) % {{{
{
   variable inputs = where( struct_map(Integer_Type, args, "marshal"));
   args = args[inputs];
   variable types = struct_map(String_Type, args, "typeid");
   types = array_map(String_Type, &sltype_abbrev, types);
   variable refs = where(types == "R");
   if (length(refs)) {
	variable refd_types = struct_map(Struct_Type, args[refs], "aux");
	refd_types = struct_map(String_Type, refd_types, "typeid");
	refd_types = array_map(String_Type, &sltype_abbrev, refd_types);
	refd_types = array_map(String_Type, &sltype_ptr_abbrev, refd_types);
	types[refs] = refd_types;
   }
   return types;
} % }}}

define make_dispatch_table_entry(overloaded_funcs)
{
   variable entry = EMPTY;

   foreach(overloaded_funcs.head) {

	variable f = ();
	variable type = f.value[0];
	variable min_num_args = f.value[1];

	entry = sprintf("\n   { %s, (char*)\"%s\", %d, %d },",
			f.name, type, strlen(type), min_num_args);

	list_append(Dispatch_Table, entry);
   }
} % }}}

% Code emission {{{

define emit_dispatcher(fmap, dispatch_entry, usage_index, vectorized) % {{{
{
   if (SC.cfront) return;		% dispatcher not needed

   variable i = length(Dispatch_Table);
   make_dispatch_table_entry(dispatch_entry);
   slirp_emit( generate_prototype(fmap) );
   slirp_emit(" { dispatch(%d, %d, %d, %d); }\n\n",
	i, i + length(fmap.overloads), usage_index, vectorized);
} % }}}

define emit_constructor_synonym(class) % {{{
{
   if (not class.abstract)
	slirp_emit("MAKE_INTRINSIC_0((char*)\"%s\", sl_%s_new, SLANG_VOID_TYPE),\n",
						class.name, class.name);
} % }}}

define emit_destructor(class) % {{{
{
   if (length(class.ancestors))
	return;

   variable proto = sprintf("void %s%s_delete(void *o)",
					SC.wrapper_prefix, class.name);

   SC.interface.prototypes[proto] = EMPTY;
   !if (SC.cfront)
	slirp_emit("static ");

   slirp_emit("%s {delete (%s *)o;}\n", proto, class.name);
} % }}}

private define emit_dispatch_table() % {{{
{
   !if (length(Dispatch_Table)) return;

   slirp_emit("typedef struct _Overloaded_Func {\t%s\n"+
	"   void (*func) (void);\n"+
	"   char *signature;\n"+
	"   int nargs;\n"+
	"   int min_nargs;\n"+
	"} Overloaded_Func;   %s\n", FOLD_OPEN, FOLD_CLOSE);

   slirp_emit("\nstatic Overloaded_Func Dispatch_Table[] =%s\n{", FOLD_OPEN);

   foreach(Dispatch_Table) {
	variable entry = ();
	slirp_emit(entry);
   }
   slirp_emit("\n   { NULL, NULL, 0, 0}\n};   %s\n\n", FOLD_CLOSE);

   inject_file("dispatch.c");
} % }}}

private define emit_public_field_wrappers(class) % {{{
{
   foreach(class.public_fields) {

	variable f = ();

	slirp_emit("FIELD_SET(%s,%s,%s,%s)\n", class.name,f.name,f.type,f.mnemonic);
	slirp_emit("FIELD_GET(%s,%s,%s,%s)\n", class.name,f.name,f.type,f.mnemonic);

	!if (SC.cfront) {
	   slirp_emit("FIELD_GET_OR_SET(%s, %s)\n", class.name, f.name);
	   continue;
	}

	variable stem = SC.wrapper_prefix + class.name + "_" + f.name;
	variable proto = f.ltype + " " + stem + "_get(void *)";
	SC.interface.prototypes[proto] = EMPTY;

	proto = "void " + stem + "_set(void *, " + f.ltype + ")";
	SC.interface.prototypes[proto] = EMPTY;
   }
} % }}}

private define emit_public_field_func_table_entry(class)
{
   variable M = "MAKE_INTRINSIC_0((char*)\"";

   foreach(class.public_fields) {
	variable f = ();
	variable stem = class.name + "_" + f.name;
	slirp_emit("   %s%s\",sl_%s,SLANG_VOID_TYPE),\n", M, stem+"_get", stem+"_get");
	slirp_emit("   %s%s\",sl_%s,SLANG_VOID_TYPE),\n", M, stem+"_set", stem+"_set");
	slirp_emit("   %s%s\",sl_%s,SLANG_VOID_TYPE),\n", M, stem, stem+"_set_or_get");
   }
}

private define emit_public_field_func_table_entries()
{
   iterate_over_classes(&emit_public_field_func_table_entry);
}

% }}}

% Type mapping and supporting code {{{

define cfront_method_referer(meth, argno_ref)
{
   sprintf("INVOKE_METHOD(%s, OBJECT(arg0), %s)", meth.class.name, meth.name);
   @argno_ref = @argno_ref + 1;
}

private define standard_method_referer(method, argno_ref)
{
   if (argno_ref != NULL) @argno_ref = @argno_ref + 1;
   return "arg0->" + method.name;
}

private define cfront_class_referer(arg, typed, ignored)
{
   if (typed)
	sprintf("void* %s", arg.lname);
   else {
	variable ref = DeReferer[arg.cpp_ref];
	sprintf("%s((%s%s)%s)", ref, arg.type, ref, arg.lname);
   }
}

private define class_instance_declarer(arg)
{
   slirp_emit("   %s%s %s;\n", arg.const, arg.type, arg.lname);
   if (arg.marshal && arg.proxy == NULL)
	slirp_emit("   %s %s_o = NULL;\n", SLIRP_OPAQUE_TYPE_NAME, arg.lname);
}

private define class_instance_marshaler(arg, argno)
{
   if (arg.defval == EMPTY)
	sprintf("pop_class_instance(%d,%s,%s,%s,%s_o)", argno, arg.type,
				arg.typeid, arg.lname, arg.lname);
   else
	sprintf("pop_class_instanced(%d,%s,%s,%s,%s_o,%S)", argno, arg.type,
				arg.typeid, arg.lname, arg.lname, arg.defval);
}

private define class_instance_returner(arg)
{
   sprintf("SLang_push_opaque(%s, &%s, 0)", arg.typeid, arg.lname);
}

private define class_instance_referer(arg, typed, ignore)
{
   arg.lname;
}

private define constructor_referer(method, ignore)
{
   return "new " + method.name;
}

private variable method_referer;
private variable class_type_referer;

private define define_class_typemaps(classname, parent, destructor) % {{{
{
   !if (SC.num_reserved_types) define_reserved_opaques();

   destructor = SC.wrapper_prefix + destructor;

   variable ot = _define_opaque(classname, parent, destructor);

   ot.referer = class_type_referer;
   ot.sget = "cpp_obj_sget";
   ot.sput = "cpp_obj_sput";
   ot.clone = &clone_class;

   % The default C handling is to treat <opaque> as a pure virtual base type,
   % meaning: it is not instantiable in the wrapper layer; routines with
   % any argument of <opaque> instance type in their signature WILL NOT be 
   % wrapped; C routines with an <opaque>* pointer type in their signature
   % WILL be wrapped, and ditto for C++ per the following mapping:

   slirp_map_opaque(classname + "*", classname);	% support <classname>*

   % For C++ this behavior is extended in two ways: first, C++ routines
   % with <opaque>& arguments WILL ALSO be wrapped ...

   slirp_map_cpp_ref(classname);			% support <classname>&

   % ... and second, we relax the pure-virtual constraint above by allowing
   % funcs with args of <opaque> instance type TO ALSO be wrapped.  This is
   % done b/c it is a relatively common/safe practice to instantiate objects
   % on the stack in C++ (e.g. in an arg list), but less so for C structs.

   ot.type = classname;					% support <classname>,
   ot.declarer  = &class_instance_declarer;		% by tweaking the base
   ot.marshaler = &class_instance_marshaler;		% opaque definition
   ot.returner  = &class_instance_returner;

   if (not SC.cfront)
	ot.referer  = &class_instance_referer;
} % }}}}

private define finalize_method(meth, class) % {{{
{
   if (meth == NULL) return;

   meth.class = class;
   meth.language = CPLUSPLUS;
   !if (SC.cfront) meth.pop_args = 1;

   if (meth.name == class.name) {		% constructor
	meth.slname += "_new";
	meth.referer = &constructor_referer;

	if (SC.genstubs) {			% Stubbed constructors should
	   meth.retval.type = EMPTY;		% not reflect a return value,
	   meth.retval.ltype = VOID;		% since that's not legal C++ 
	   meth.gname = class.name + "::" + class.name;
	}
	else
	   meth.gname = meth.slname;
   }
   else if (not SC.genstubs) {			   % regular method, so inject
	variable this = @SC.types[class.name+"*"]; % "this" as hidden/first arg
	this.name = EMPTY;
	this.lname = "arg0";
	meth.slname = strcat(class.name,"_",meth.slname);
	meth.args = [this, meth.args];
	meth.nargs++;
	meth.referer = method_referer;
	meth.gname = meth.slname;
   }
   else
	meth.gname = meth.retval.type + " " + class.name + "::" + meth.slname;

   !if (SC.cfront)
	annotate(meth);
} % }}}

private define queue_public_field_wrapper_generation(class, field)
{
   if (class == NULL or field == NULL) return;
   if (field.typeclass != TYPECL_SCALAR) return;

   list_append(class.public_fields, field);
}
% }}}

private define parse_destructor(fp, class) % {{{
{
  variable decl = get_func_decl(fp, get_token(fp));
  if (SC.genstubs) {
	variable fmap = parse_func_decl("void ~"+decl, 0, 0);
	fmap.retval.type = EMPTY;		% not reflect a return value,
	fmap.retval.ltype = VOID;		% since that's not legal C++ 
	fmap.gname = class.name + "::~" + class.name;
   }
} % }}}

private define clone_class(Type, NewType) % {{{
{
   % Cloning allows NewType to be used interchangeably with Type in method
   % & function signatures, but replicates none of the interface of Type

   if (typeof(Type) == TypeMapping)
	Type = Type.sltype;

   define_class_typemaps(NewType, Type, Type + "_delete");

   return SC.types[NewType];
} % }}}

private define parse_class(fp, nesting, publicly_scoped) % {{{
{
   variable class = @Class, derived = 0;

   variable token = prep_get_token(fp);
   if (token == "{")
	class.name = "anonymous";
   else {
	class.name = token;
	token = prep_get_token(fp);
	if (token[-1] == ';')
	   return class;			% a forward declaration
   }

   class.ancestors = String_Type[0];
   class.abstract = 0;
   class.constructors = String_Type[0];
   class.nesting = nesting;
   class.public_fields = {};

   In_Public_Interface = publicly_scoped;

   variable inheriting = 0;

   while (token != "{") {

	switch(token)
	  { case ":" or case "," : inheriting = 1; }
	  { case "public" : derived = 1; }
	  { case "private"  or case "protected" : derived = 0; }

	  {
		if (not inheriting)		% typedef struct Type NewType
		   return clone_class(class.name, token).sltype;

		% Support inheritance only from known classes
		if (derived && SC.types[token] != NULL)
		   class.ancestors = [ class.ancestors, token ];

		inheriting = 0;
	  }

	token = prep_get_token(fp);
   }

   variable prev_interface = save_interface(CPLUSPLUS);
   if (not nesting)
	SC.global_interface = prev_interface;

   SC.interface.name = class.name;

   variable parent = NULL, destructor = "_delete";
   if (length(class.ancestors))	{		% multiple inheritance
	parent = class.ancestors[0];		% not yet unsupported
	class.destructor = SC.classes[parent].destructor;
   }
   else
	class.destructor = class.name + destructor;

   define_class_typemaps(class.name, parent, class.destructor);

   parse_cplusplus_block(fp, class);

   class.interface = restore_interface(prev_interface);

   SC.classes[class.name] = class;

   return class.name;

} % }}}

private define parse_cplusplus_block(fp, class) % {{{
{
   if (SC.debug)
	tprintf("%s: ENTERED (class %s)", _function_name, class.name);

   SC.tablevel++;

   do { 

	variable fmap, decl, token = prep_get_token(fp);

	if (token == ";") continue;		% forward declaration

	if (SC.debug > 1)
	   tprintf("%s: token=<%S>", _function_name, token);

	switch(token)

	{ case "private" or case "protected":
	   	In_Public_Interface = 0;
		() = get_token(fp);		% swallow colon
		continue;
	}
	{ case "public"  :

	   	% Inner classes should never become publicly visible
		In_Public_Interface = (class.nesting == 0);
		() = get_token(fp);			% swallow colon
		continue;
	}

	{ case class.name : 

	   token = get_token(fp);

	   if (token == "(") {

		% A constructor: treat as if it returns type of <class_name>*

		decl = strcat(class.name,"* ", class.name, token);
		decl = get_func_decl(fp, decl);

		% Strip initialization lists, if present
		if (string_match(decl, "\\(.+)\\)[ \t]*:.+", 1))
		   decl = get_match(decl, 1);

		% Defer full treatment of constructors until
		% we know whether or not class is abstract 
		class.constructors = [class.constructors, decl];
		continue;
	   }
	   % fall thru: treat as potential macro/type/field declaration
	}
	   
	{ case "~":	parse_destructor(fp, class); continue; }

	{ case "class": () = parse_class(fp, class.nesting+1, 0); continue; }

	{ case "virtual":

	   token = get_token(fp);
	   if (token == "~") { parse_destructor(fp, class); continue;}

	   decl = get_func_decl(fp, token);
	   variable pure_virtual = string_match(decl,
					")[ \t]*=[ \t]*0[ \t]*;[ \t]*$", 1);

	   if (pure_virtual) {
		decl = strcat(decl[[0:pure_virtual-1]],";");
		class.abstract = 1;
	   }

	   if (In_Public_Interface and not class.nesting) {
		fmap = parse_func_decl(decl, 0, 0);
		finalize_method(fmap, class);
	   }

	   continue;
	}

	{ case "static" : continue; }

	if (token == EMPTY) abort("incomplete class definition");

	if (macro_or_type_definition(fp, token))
	   continue;

	if (In_Public_Interface) {

	   if (get_var_decl(fp)) {

		variable field;

		if (parse_var_decl(&field))
		   foreach field (field)
			queue_public_field_wrapper_generation(class, field);
	   }
	   else {
		decl = get_func_decl(fp, SC.decl);
		fmap = parse_func_decl(decl, 0, 0);
		finalize_method(fmap, class);
		SC.decl = EMPTY;
	   }
	}
	else {
	
	   % Swallow non-public content
	   !if (get_var_decl(fp))
		() = get_func_decl(fp, SC.decl);
	}

   } while (token != "}");

   !if (class.abstract or class.nesting) {

	% Fabricate default constructor if none was explicitly defined in class
	if (length(class.constructors) == 0)
	   class.constructors = [class.name + "* " + class.name + "()" ];

	foreach(class.constructors) {
	   decl = ();
	   fmap = parse_func_decl(decl, 0, 0);
	   finalize_method(fmap, class);
	}
   }

   SC.tablevel--;
   if (SC.debug)
	tprintf("%s: EXITED (class %s)\n",_function_name, class.name);

} % }}}

define parse_struct_as_class(fp) % {{{
{
   return parse_class(fp, 0, 1);
} % }}}

private define activate_cpp_support() % {{{
{
   if (SC.cfront) {
	class_type_referer = &cfront_class_referer;
	method_referer = &cfront_method_referer;
   }
   else {
	class_type_referer = &opaque_referer;
	method_referer = &standard_method_referer;
   }
   add_tokens(Single_Char_Tokens, "~ ( ) , [ ] < + - = * / &");
   _macros["static"] = EMPTY;
   SC.funcprefix = "^[~]?[a-zA-Z]+";
   SC.interface.language = CPLUSPLUS;
   SC.parse_struct = &parse_struct_as_class;
   SC.cplusplus = 1;
} % }}}

define cplusplus_class(fp) % {{{
{
   variable token = get_token(fp);

   switch(token)
   { case "class": activate_cpp_support ();
      () = parse_class(fp, 0, 0); return 1;
   }
   { case "template":

      	activate_cpp_support ();
	swallow_to(fp, '>');

	token = get_token(fp);
	if (token == "class")
	   () = parse_class(fp, 1, 0);
	else
	   swallow_C_block(fp);

	return 1;
   }
   { case "inline" or case "operator": unget_token(fp, token); }
   { case "using": swallow_to(fp, ';');
      activate_cpp_support ();
      return 1;
   }
   {
	unget_token(fp, token);
	return 0;
   }

   activate_cpp_support ();
   return 0;

} % }}}

if (SC.default_language == CPLUSPLUS)
   activate_cpp_support ();

register_callback(POST_GEN_CB, &emit_dispatch_table);
register_callback(PRE_FUNC_TABLE_CB, &iterate_over_classes,
      						&emit_public_field_wrappers);
register_callback(IN_FUNC_TABLE_CB, &emit_public_field_func_table_entries);
register_callback(IN_FUNC_TABLE_CB, &iterate_over_classes,
      						&emit_constructor_synonym);

provide("slirpc++");
