#if (SLIRP_HAVE_FORTRAN)

private variable EntryPoint = struct {  % {{{
   name,		% Fortran function/subroutine name
   mangled_name,	% name as it must be called from C scope
   retval,
   args,		% ordered sequence of all args
   untyped_args,	% assoc array of args with as yet unknown types
   variables,		% assoc array of all variables in the routine
   constants,
   implicit_types,
   funcmap		% corresponding C function mapping
};
private variable Func_Wrapper_Suffix = "sfwrap";
private variable Return_Value_Name = "OUTPUT";
private variable Fortran_String_Type = "character";
private variable BlankCommon = "_BLNK_";
private variable Function_Entry_Points = Struct_Type[0];
private variable Aliases = @List;

private define   parse_fortran();			% forward decl
private define   resolve_symbols();			% forward decl
% }}}

private define is_comment(ch) % {{{
{
   if (ch == 'C' || ch == 'c' || ch == '*')
      return 1;
   return 0;
} % }}}

private define is_white (c)
{
   if (isspace (c))
     {
	if (c == '\n') SC.line++;
	return 1;
     }
   return 0;
}

private define eat_white_lines(fp, eof_ok) % {{{
{
   variable ch, l, buf, nlines = 0, nchars = 0;

   forever {

	if ( fread (&ch, Char_Type, 1, fp) == -1) {
	   !if (eof_ok) eof_error(_function_name);
	   return nlines;
	}

	if (ch == NEWLINE) {
	   nlines++;
	   nchars = 0;
	}
	else if (nchars == 0 && is_comment(ch)) {
	   % Comment line are considered white, and thus skipped
	   if (fgets(&buf,fp) == -1 && not eof_ok)
		eof_error(_function_name);
	   nlines++;
	   nchars = 0;
	}
	else {
	   nchars++;
	   !if (is_white(ch)) break;
	}
   }

   if (nchars) backup(fp, nchars);
   return nlines;
} % }}}

private define get_line(fp, eof_ok) % {{{
{
   variable l;

   do {
	() = eat_white_lines(fp, eof_ok);
	if (fgets(&l,fp) == -1) {
	   if (eof_ok) return NULL;
	   eof_error(_function_name);
	}
	if (string_match(l, "\\(.*\\)!.*$",1))	% remove embedded comments
	   l = get_match(l, 1);

	forever {

	   if (eat_white_lines(fp, 1)) continue;

	   % look ahead for possible line continuation
	   variable buf, n = fread(&buf, Char_Type, 6, fp);
	   if (n < 6 || buf[0]==TAB || buf[5]==SPACE || buf[5]==TAB) {
#ifexists LLong_Type
		n = typecast(n, LLong_Type);
#endif
		() = fseek(fp, -n, SEEK_CUR);
		break;
	   }
	   else {			   % append continuation to prev line
		if (fgets(&buf,fp) == -1)
		   eof_error(_function_name);

	   if (string_match(buf, "\\(.*\\)!.*$",1))  % remove embedded comments
		buf = get_match(buf, 1);

		l += " " + buf;
	   }
	}

	% Strip line labels, if present
	if (string_match(l, "^[ \t]*[0-9]+\\(.*\\)", 1))
	   l = get_match(l, 1);

	% Compress each continguous bit of whitespace into single space
	l = strcompress(l," \t\r\n\f");

   } while (l == EMPTY);		% return only non-blank lines

   return l;
} % }}}

private define symbol_new(name, type, ftype, size, argnum) % {{{
{
   variable v = struct {name, type, ftype, dims, ndims, size, argnum,
   				initializer, common, c_symbol};
   v.name = name;
   v.type = type;
   v.ftype = ftype;
				% argnum > 0: an argument to a routine
   v.argnum = argnum;		% = 0: a return value
   				% < 0: a local variable (e.g. w/in common block)

   v.size = size;		% the (scalar) datatype size
   v.ndims = 0;			% array dimensionality
   v.dims = String_Type[0];

   return v;
}
% }}}

private define mangle(name)  % {{{
{
   if (name == BlankCommon)		% blank common block name is apparently
      return name + "_";		% not mangled the same as other symbols

   variable mangled = name + SLIRP_FORTRAN_MANGLE_SUFFIX;
   if (SLIRP_FORTRAN_MANGLE_USCORE && is_substr(name, "_"))
	mangled += "_";

   if (SLIRP_FORTRAN_MANGLE_UPCASE)
	mangled = strup(mangled);
   else
	mangled = strlow(mangled);

   return mangled;
}  % }}}

private define emit_line() % {{{
{
   variable args = __pop_args(_NARGS);
   slirp_emit("      ");
   slirp_emit( __push_args(args) );
   slirp_emit("\n");
} % }}}

private define emit_comment() % {{{
{
   variable args = __pop_args(_NARGS);
   slirp_emit("c     ");
   slirp_emit( __push_args(args) );
   slirp_emit("\n");
} % }}}

private define marshaler_pop_space_padded (arg, argno)
{
   return sprintf ("pop_space_padded_string ((%s *)&%s, %S)",
                   arg.ltype, arg.lname, arg.marshaler_args);
}

private define cleaner_free_space_padded (arg, argno)
{
   return sprintf("%sfree_space_padded_string (%s);\n",
		  (arg.defval == EMPTY) ? "" : sprintf ("if (SLang_Num_Function_Args > %d) ", argno),
		  arg.lname);
}


private define make_hidden_str_len_arg(fort_arg, ep) % {{{
{
   % Introduce a hidden string length parameter, so that the FORTRAN calls
   % which accept one or more CHARACTER args may be called correctly from C
   % scope.  By default these length args will be appended to the end of the
   % arg list when the Fortran entry point is invoked in the wrapper, but the
   % -fortran_strlen option may be used to insert them directly within the
   % list, just after the CHARACTER arg.  This length parameter will NOT be
   % passed in to the given wrapper from S-Lang scoped calls.  Also, unlike
   % the usual FORTRAN convention, the length argument is passed BY VALUE.

   variable c_arg = fort_arg.c_symbol;
   variable str_arg = c_arg.lname;

   variable hidden = arg_new("size_t", EMPTY, EMPTY, EMPTY,  0);
   hidden.lname = str_arg + "_len";
   hidden.marshal = 0;			% Not passed in from S-Lang scope

   % Initialize with nominal #argmap(init)
   variable init = argmap_new(AM_Init, ep.funcmap, EMPTY);

   if (c_arg.marshal)
     {
        if (not fort_arg.ndims)
          {
             variable min_len = atoi (fort_arg.size);
             % Get Fortran string scalar length from the C string scalar
             % (length for string arrays is determined when unpacking arrays)
             init.code = sprintf("   %s = strlen(%s);\n", hidden.lname, str_arg);
             if (min_len > 0)
               {
                  c_arg.marshaler = &marshaler_pop_space_padded;
                  c_arg.marshaler_args = min_len;
                  c_arg.cleaner = &cleaner_free_space_padded;
               }
          }
     }
   else
     {
	init.code += sprintf("   %s = %S;\n", hidden.lname, fort_arg.size);
     }

   return hidden;
} % }}}

private define make_c_prototype(ep) % {{{
{
   variable ref, p = sprintf("extern void %s ( ", ep.name);

   foreach(ep.args) {

	variable a = (), name = EMPTY;
	variable tmap = get_typemap(a.type, 0);

	if (tmap == NULL)
	   return NULL, warn_ignore( SC.infname + ":" + ep.name,
				"Unsupported type: " + a.type);

        variable proto_type = tmap.ltype;

	if (a.type == Fortran_String_Type) {

	   ref = EMPTY;
	   if (a.ndims) {
		proto_type = "FTN_STR_ARR";
		SC.fortran_wrapped++;		  % indicate need for pack.c
	   }
	}
	else {
	   % If this is a function return val, ensure that it's named
	   % in the signature, so that the OUTPUT annotation is applied
	   % All other args are left unnamed in the prototype, both for
	   % brevity and to avoid clashes with reserved C identifiers
	   if (a.name == Return_Value_Name)
		name = Return_Value_Name;
	   ref = ASTERISK;
	}

	p = sprintf("%s%s%s %s, ", p, proto_type, ref, name);
   }

   return strtrim_end(p, ", ") + ");";
} % }}}

% Input tokenization % {{{
private define make_line_descriptor(fp, eof_ok) % {{{
{
   variable line = get_line(fp, eof_ok);
   if (line == NULL) return NULL;

   variable ld = struct { original, line, index, len };
   ld.index = 0;
   ld.original = line;				% unmodified input
   ld.line = strlow(strtrans(line, ",", " "));  % lowercased, with no commas
   ld.len = strlen(line);

   return ld;
} % }}}

private define get_token(ld) % {{{
{
   % Whitespace-delimited line tokenizer; EOL is indicated
   % by EMPTY return token (and line index is not advanced)

   variable line = ld.line, len = ld.len, i = ld.index, token = EMPTY;
   if (i >= len) return token;

   while (i < len) {				% skip leading whitespace
	variable ch = line[i];
	if (ch == NEWLINE) return token;
	!if (is_white(ch)) break;
	i++;
   }

   while (i < len) {

	ch = char(line[i]);

	if (Single_Char_Tokens[ch]) {
	   if (token == EMPTY) {
		i++;
		token = ch;
	   }
	   break;
	}

	token += ch;
	i++;
	if (is_white(line[i]))
	   break;
   }

   ld.index = i;
   return token;
} % }}}

private define unget_token(tok, ld) % {{{
{
   ld.index -= strlen(tok);
} % }}}

private define get_type_token(token_ref, type_ref, ftype_ref, size_ref, ld) % {{{
{
   variable token = @token_ref, type, size = EMPTY;

   switch(token)
   { case "double" :
	token = get_token(ld);
	if (token == "precision")
	   type = "double precision";
	else
	   type = "double complex";
   }
   { case "complex" or case "real" or case "logical" or
     case "integer" or case Fortran_String_Type :

	type = token;
   }
   { return 0; }

   token = get_token(ld);

   if (token == ASTERISK) {

	size = get_token(ld);

	if (size == "(") {
	   % CHARACTER string of possibly indeterminate size:
	   % 	the parsed size may be either a number or "*"
	   size = get_token(ld);
	   () = get_token(ld);		% close paren
	}

	token = get_token(ld);
   }

   % Couple the size with the type for more rigorous matching later with
   % get_typemap(), but not for CHARACTER b/c we can't store every possible
   % size variation (CHARACTER*1, *2, *3, ...) in the typemap table
   variable ftype = type;
   if (size != EMPTY)
     {
	ftype += ASTERISK + size;
	if (type != Fortran_String_Type)
	  type = ftype;
     }

   @token_ref = token;			% NB: token advanced when type found
   @type_ref  = type;
   @ftype_ref  = ftype;
   @size_ref  = size;

   return 1;
} % }}}

private define match_tok(wanted, ld)  % {{{
{
   variable tok = get_token(ld);
   if (tok != wanted)
	return 0, warn("Rejecting <%S>: missing %S", ld.line, wanted);
   return 1;
}  % }}}
% }}}

% Include file handling {{{
private define open_include_file(incl) % {{{
{
   !if (string_match(incl, "\\C^include[ \t]+'\\(.+\\)'.*", 1))
	return NULL;

   incl = get_match(incl, 1);

   variable fullpath = slirp_find_file_in_path(SC.include_paths, incl);

   if (fullpath != NULL)
	incl = fullpath;

   variable fp = fopen(incl, "rb");
   if (fp == NULL)
	abort("Could not open include file: %S", incl);

   return fp;
} % }}}

private define file_scoped_include(fp) % {{{
{
   SC.decl = get_line(fp, 1);
   if (SC.decl == NULL) return 0;

   fp = open_include_file( SC.decl );
   if (fp == NULL) {
	SC.decl = strlow(SC.decl);		% case is no longer relevant
	return 0;
   }

   parse_fortran(fp);

   return 1;
} % }}}

private define function_scoped_include(ep, ld) % {{{
{
   variable fp = open_include_file( ld.original );
   if (fp == NULL) return NULL;

   return resolve_symbols(fp, ep, 1);

} % }}}
% }}}

% Array dimensionality {{{

private variable Common_Blocks = Empty_Assoc_Array;

private define look_for_array_dimensions(symbol, ld) % {{{
{
   variable tok = get_token(ld);
   if (tok != "(") {
	unget_token(tok, ld);
	return;
   }

   forever {
	tok = get_token(ld);

	if (tok == ")") break;

	if (tok == ":")
	   symbol.dims[-1] += tok + get_token(ld);
	else {
	   symbol.ndims++;
	   symbol.dims = [ symbol.dims, tok];
	}
   }
} % }}}

private define make_array_dim_arg_optional(arg, which, ep) % {{{
{
   % Fortran array dimension args are made optional in calls from S-Lang,
   % by assigning a default value & attaching an #argmap(in) to initialize

   variable fmap = ep.funcmap;
   variable c_arg = arg.c_symbol;
   if (c_arg == NULL || c_arg.lname == NULL) {
	c_arg = fmap.args[arg.argnum - 1];
	arg.c_symbol = c_arg;
   }

   % No need for this if arg has already been tagged as omitted or defaulted
   if (not c_arg.marshal or c_arg.defval != EMPTY)
	return;

   c_arg.defval = arg.name;
   variable code = sprintf("   if (%s == NULL)\n\t", c_arg.lname);
   variable dims = "dims";
   if (which > 0)
	dims += " + " + string(which);

   code += sprintf("arg%d = ", arg.argnum);
   if (fmap.args[arg.initializer.argnum-1].proxy == NULL)
	code += sprintf("arg%d_r->array->%s;\n", arg.initializer.argnum, dims);
   else
	code += sprintf("proxy%d->%s;\n", arg.initializer.argnum, dims);

   () = argmap_new(AM_In, fmap, code);
} % }}}

private define resolve_dim_specifier(ep, dim, which, sym) % {{{
{
   if (_slang_guess_type(dim) != String_Type)
	return dim;				% numeric literal

   variable constant = ep.constants[dim];
   if (constant != NULL)
	return constant;			% parameter() value

   variable var = ep.variables[dim];
   if (var == NULL) {
	if (sym.c_symbol != NULL && not sym.c_symbol.marshal)
	   return NULL, warn_ignore(ep.name,
			sprintf("%s cannot be OUTPUT value: unknown"+
			" dimension(s) in declaration", sym.name));

	return ASTERISK;
   }

   var.initializer = sym;
   if (var.argnum > 0) {
	make_array_dim_arg_optional(var, which, ep);
	return var.name;
   }

   if (var.common != NULL) {

	if (not SC.fortran_wrapcommon)
	   return NULL, warn_ignore(ep.name,
			sprintf("Uses common block %S, but common "+
			"block wrapping suppressed", var.common));

	return sprintf("*((%s*)%s_com[%d].data)", var.c_symbol.ltype,
				var.common, -var.argnum);
   }

   return ASTERISK;
} % }}}

private define resolve_array_dimensions(ep, arg) % {{{
{
   variable c_arg = arg.c_symbol;

   if (not arg.ndims and arg.argnum > 0) {
	% Fortran uses call-by-reference semantics, but we do not want refs
	% to scalars to be misconstrued in usage statements as C array[]s
	c_arg.usage = c_arg.mnemonic;
	if (c_arg.defval != EMPTY)
	   c_arg.usage += "=" + c_arg.defval;
	return 1;
   }
   variable which = 0;
   loop(arg.ndims) {

	variable bounds = strtok(arg.dims[which], ":");
	variable value = resolve_dim_specifier(ep, bounds[-1], which, arg);

	if (value == NULL)
	   return 0;

	if (length(bounds) == 2) {

	   variable lower = resolve_dim_specifier(ep, bounds[0], which, arg);
	   if (lower == NULL)
		return 0;

	   if (value != ASTERISK and lower != ASTERISK) {
		if (lower == "1")
		   lower = EMPTY;
		else
		   lower = "- " + string(int(lower) - 1);
		value = value + lower;
	   }
	}

	arg.dims[which] = value;
	which++;
   }

   if (arg.argnum < 1)
	return 1;

   if (not c_arg.marshal and not c_arg.vectorized) {

	() = argmap_new(AM_Setup, ep.funcmap,
				sprintf("   SLindex_Type %s_dims[%d];\n",
				c_arg.lname, arg.ndims));

	variable code = array_map(String_Type, &sprintf,
						"   %s_dims[%d] = %S;\n",
						c_arg.lname,
						[0:arg.ndims-1],
						reverse(arg.dims));

	() = argmap_new(AM_Init, ep.funcmap, strjoin(code, ""));
   }

   c_arg.dim = arg.ndims;
   if (arg.ndims > 1)
	c_arg.layout = COLUMN_MAJOR;

   if (not any(arg.dims == ASTERISK))
	c_arg.arrayspec = sprintf("[%S]", strjoin(arg.dims, ","));

   return 1;

} % }}}
% }}}

% Common block handling % {{{

private variable TypeFuncs = Assoc_Type[String_Type, "generic"];
TypeFuncs["complex"] = "scomplex";
TypeFuncs[Fortran_String_Type] = "fstring";

private define parse_common_block(ep, ld) % {{{
{
   % Common blocks are parsed even when common block wrapping is
   % suppressed, so that routines referencing them are also ignored

   variable name = EMPTY;
   variable tok = get_token(ld);
   variable maybe_named = (tok == "/");

   if (maybe_named) {
	tok = get_token(ld);
	if (tok != "/") {
	   name = tok;
	   if (not match_tok("/", ld))
		return;
	}
   }
   else
	unget_token(tok, ld);

   % Common block names are global, so programs have at most 1 unnamed block
   if (name == EMPTY)
	name = sprintf("_BLNK_");

   variable nvars = 0, comblock = Struct_Type[0];

   forever {

	tok = get_token(ld);
	if (tok == EMPTY)
	   break;

	variable symbol = ep.variables[tok];
	if (symbol == NULL) {
	   symbol = symbol_new(tok, EMPTY, EMPTY, EMPTY, 0);
	   ep.untyped_args[symbol.name] = symbol;
	   ep.variables[symbol.name] = symbol;
	}

	look_for_array_dimensions(symbol, ld);
	if (not resolve_array_dimensions(ep, symbol) ||
						any(symbol.dims == ASTERISK))
	   return vmessage("Skipping %S common block, could not determine "+
	   		   "dimensions of array: %S", name, symbol.name);

	symbol.common = name;
	symbol.argnum = -nvars;
	symbol.c_symbol = get_typemap(symbol.type, 0);
	comblock = [ comblock, symbol];		% keep variables ordered
	nvars++;
   }

   SC.fortran_wrapped++;		  % indicate need for pack.c

   % Redefinitions of common blocks are ignored
   if (Common_Blocks[name] == NULL)
	Common_Blocks[name] = comblock;
} % }}}

private define emit_c_common_block_wrappers() % {{{
{
   !if (length(Common_Blocks) and SC.fortran_wrapcommon) return;

   slirp_emit("/*  Fortran Common Blocks Support */  %s\n", FOLD_OPEN);

   inject_file("commblock.h");

   variable blockname, vars;
   variable blocks_array = "static CommBlock CommBlocks[] = {\n";

   foreach(Common_Blocks) using ("keys", "values") {

	(blockname, vars ) = ();

	variable vars_array = sprintf("static CommBlockVar %s_com [] = {\n",
								blockname);
	foreach(vars) {

	   variable var = ();
	   variable slang_typeid = var.c_symbol.typeid;

	   if (var.type == Fortran_String_Type) {
		var = @var;
		var.ndims++;
		var.dims = [var.dims, var.size];
	   }

	   variable dims = strjoin(reverse(var.dims), ",");
	   if (dims == EMPTY)
		dims = "0";

	   vars_array += sprintf(
		"  {(char*)\"%s\",%s,NULL,%d,{%s},%s,%s},\n",
		var.name, slang_typeid, var.ndims, dims,
		"push_" + TypeFuncs[var.type], "pop_"+ TypeFuncs[var.type]);
	}

	slirp_emit("\n" + vars_array + "  {NULL, 0, NULL, 0, {0}, NULL, NULL}\n};\n");

	variable mangled_blockname = mangle(Func_Wrapper_Suffix + blockname);

	slirp_emit("extern LINKAGE void %s( void (*) (int*, int*, ...));\n",
							mangled_blockname);

	blocks_array += sprintf("  { (char*)\"%s\", %d, %s_com, %s },\n",
			blockname, length(vars), blockname, mangled_blockname);
   }

   slirp_emit(blocks_array +  "  {NULL, 0, NULL, NULL }\n};\n");

   slirp_emit("static unsigned int Num_CommBlocks = %d;\n", length(Common_Blocks));

   inject_file("commblock.c");

   slirp_emit("%s\n\n", FOLD_CLOSE);
} % }}}

private define emit_fortran_common_block_wrappers() % {{{
{
   !if (length(Common_Blocks) and SC.fortran_wrapcommon) return;

   % Fortran init functions are used to obtain pointers to common block
   % variables, instead of C structures, to avoid portability problems
   % (such as structure alignment/padding diffs between C and Fortran)

   variable name, vars, nblocks = 0;

   foreach(Common_Blocks) using ("keys", "values") {

	(name, vars ) = ();

	slirp_emit("      subroutine %s%s (initfunc)\n", Func_Wrapper_Suffix, name);
	slirp_emit("      external initfunc\n");

	variable intro = "      common        ", decl = EMPTY;
	if (name != BlankCommon)
	   intro += "/" + name + "/ ";

 	variable pos = strlen(intro), i = 0;
	foreach(vars) {
	   variable var = ();
	   if (i) { decl += ","; pos++; }
	   if (pos >= 60) {			% precaution to avoid lines
		decl += "\n     $ ";		% that are too long
		pos = 8;
	   }
	   decl += var.name;
	   pos += strlen(var.name);
	   i++;

	   variable dims = strjoin(var.dims, ",");
	   if (dims != EMPTY) dims = "(" + dims + ")";

	   slirp_emit("      %s %s%s\n", var.ftype, var.name, dims);
	}

	slirp_emit("%s%s\n", intro, decl);
	slirp_emit("      call initfunc(%d,%d,%s)\n", nblocks, length(vars), decl);
	slirp_emit("      end\n\n");

	nblocks++;
   }
} % }}}
% }}}

% Implicit variable handling  % {{{

private variable Default_Implicits = String_Type[26];
Default_Implicits[[0:7]] = "real";
Default_Implicits[[8:13]] = "integer";
Default_Implicits[[14:25]] = "real";

private define parse_implicit_type_spec(ep, tok, ld) % {{{
{
   variable type, size, ftype;
   while (tok != EMPTY) {

	() = get_type_token(&tok, &type, &ftype, &size, ld);	% "(" ignored here
	tok = get_token(ld);				% get first letter
	do {
	   variable lower = tok, upper;
	   tok = get_token(ld);
	   if (tok == "-") {
		upper = get_token(ld);
		tok = get_token(ld);
	   }
	   else
		upper = lower;

	   lower = int(lower) - 97;
	   upper = int(upper) - 97;
	   ep.implicit_types[[lower:upper]] = type;

	} while (tok != ")");

	tok = get_token(ld);
   }
} % }}}

private define resolve_implicit_variables(ep) % {{{
{
   if (not length(ep.untyped_args))
  	return 1;

   if (ep.implicit_types == NULL)
	return 0, warn_ignore(SC.infname + ":" + ep.name,
		sprintf("IMPLICIT NONE with implicitly typed args: %s",
		strjoin( assoc_get_keys(ep.untyped_args)," ")));

   foreach(ep.untyped_args) {

	variable typechar, a = ();
	if (a.name == Return_Value_Name)
	   typechar = ep.name[0];
	else
	   typechar = a.name[0];

	if (typechar < 97 or typechar > 122)
	   return 0, warn_ignore(SC.infname + ":" + ep.name,
			sprintf("argument of unknown type: %c", typechar));

	a.type = ep.implicit_types[ typechar - 97 ];
	a.c_symbol = get_typemap(a.type, 0);
   }

   return 1;
} % }}}
% }}}

private define parse_parameter_statement(ep, ld) % {{{
{
   if (not match_tok("(", ld))
	return;

   variable s = strlow(strtrim_end(ld.original[[ld.index:]], ")"));
   foreach(strtok(s, ",")) {
	s = ();
	s = strtok(s, "=");
	variable name = str_delete_chars(s[0], "\\s");
	variable value = str_delete_chars(s[1], "\\s");
	ep.constants[name] = value;
   }
} % }}}

private define resolve_symbols(fp, ep, nested) % {{{
{
   forever {

	variable type = EMPTY, size = EMPTY, ftype = EMPTY;
	variable ld = make_line_descriptor(fp, nested);

	if (ld == NULL)
	   break;

	variable tok = get_token(ld);

	while (tok != EMPTY) {

	   if (get_type_token(&tok, &type, &ftype, &size, ld)) {

		while (tok != EMPTY) {

		   % See if this symbol declaration matches a named argument
		   variable symbol = ep.untyped_args[tok];
		   if (symbol == NULL) {
			symbol = symbol_new(tok, type, ftype, size, 0);
			ep.variables[tok] = symbol;
		   }
		   else {
			assoc_delete_key (ep.untyped_args, tok);
			symbol.type = type;
			symbol.size = size;
			symbol.c_symbol = get_typemap(type, 0);
		   }

		   look_for_array_dimensions(symbol, ld);
		   tok = get_token(ld);
		}
	   }
	   else if (tok == "implicit") {

		tok = get_token(ld);

		if (tok == "none")
		   ep.implicit_types = NULL;
		else if (ep.implicit_types != NULL)
		   parse_implicit_type_spec(ep, tok, ld);

		break;
	   }
	   else if (tok == "parameter") {
		parse_parameter_statement(ep, ld);
		break;
	   }
	   else if (tok == "common") {
		parse_common_block(ep, ld);
		break;
	   }
	   else if (tok == "include") {
		ep = function_scoped_include(ep, ld);
		break;
	   }
	   else if (tok == "dimension") {
		% F77 dimension statements implicitly assign a type, so
		% they're redundant in presence of "implicit none" (b/c
		% variable still has to be declared, so why not with
		% dims?) FIXME: array dims in dimension statements
		break;
	   }
	   else if (tok == "entry") {
		_list_append(Aliases, list_node_new(ep.name, get_token(ld)));
		break;
	   }
	   else if (tok == "external") {

		tok = get_token(ld);

		while (tok != EMPTY) {

		   symbol = ep.untyped_args[tok];
		   if (symbol != NULL)
			return NULL, warn_ignore( SC.infname + ":" + ep.name,
			"EXTERNAL function arguments not supported : "+ tok);

		   tok = get_token(ld);

		}
	   }
	   else
		break;
	}

	if (tok == "end")		% We only need variable declarations,
	   break;			% so its ok to stop after first "end"
   }

   if (nested || resolve_implicit_variables(ep))
	return ep;

   return NULL;
} % }}}

private define entry_point_new(retval, signature) % {{{
{
   variable argnum = 0;

   variable ep = @EntryPoint;
   ep.args = Struct_Type[0];
   ep.untyped_args = Empty_Assoc_Array;
   ep.constants = Empty_Assoc_Array;
   ep.variables =  Empty_Assoc_Array;
   ep.implicit_types =  @Default_Implicits;

   () = string_match(signature,"^\\([^(]+\\)\\(.*\\)",1);
   ep.name = strtrim(get_match(signature, 1));
   ep.mangled_name = ep.name;
   variable arglist = str_delete_chars(get_match(signature, 2),"() \t");

   ep.retval = symbol_new(Return_Value_Name, retval, retval, EMPTY, 0);
   if (retval != VOID) {
	argnum++;
	ep.args = [ep.retval, ep.args ];
	ep.mangled_name += Func_Wrapper_Suffix;	% C scope calls functions via
   }						% subroutine wrappers

   foreach (strtok(arglist, ",")) {
	variable arg = ();
	argnum++;
	arg = symbol_new(arg, EMPTY, EMPTY, EMPTY, argnum);
	ep.untyped_args[arg.name] = arg;
	ep.variables[arg.name] = arg;
	ep.args = [ep.args, arg];
   }

   ep.mangled_name = mangle(ep.mangled_name);
   if (retval == EMPTY)
	ep.untyped_args[ep.name] = ep.retval;

   return ep;
} % }}}

private define entry_point_referer(fmap, ignored) % {{{
{
   return fmap.data.mangled_name;
} % }}}

private define emit_c_prototype(fmap) % {{{
{
   if (fmap.nargs) {
	variable signature = struct_map(String_Type, fmap.args, "type");
	signature = strjoin(signature, ",");
   }
   else
	signature = VOID;
   slirp_emit("extern LINKAGE void %s (%s);\n", fmap.data.mangled_name, signature);
} % }}}

private define fortran_arg_sizer(arg, what) % {{{
{
   abort("Fortran array dimension metadata: Not Implemented Yet");
} % }}}

private define map_to_c(entryp) % {{{
{
   if (entryp == NULL) return 0;

   % Vectorizing FORTRAN funcs with args > 2D is unsupported, b/c
   % transpose(3D) != [transpose(2D), transpose(2D) ]; supporting
   % this requires that each stride be transposed independently

   variable maxdim = max([0, struct_map(Int_Type, entryp.args, "ndims")]);
   if (maxdim > 1 && (SC.vectorize || try_vectorize[entryp.name])) {
	warn_novec(entryp.name, "%dD FORTRAN arrays not vectorizable", maxdim);
	dont_vectorize[entryp.name] = 1;
	assoc_delete_key(SC.interface.functions, entryp.name);
   }

   variable c_prototype = make_c_prototype(entryp);
   if (c_prototype == NULL) return 0;

   % Prohibit overloading and apply annotations when parsing f2c prototypes
   variable fmap = parse_func_decl( c_prototype, 1, 1);
   if (fmap == NULL) return 0;

   % Post-process:
   %  (a) perform Fortran customizations to C funcmap (language, sizer, etc)
   %  (b) store corresponding C arg in Fortran symbol
   %  (c) resolve array dims to literals, args, common blk vars (or dim stmts)
   %  (d) fabricate hidden length parameter(s) for CHARACTER arguments

   fmap.data = entryp;						% (a)
   fmap.language = FORTRAN;
   fmap.sizer = &fortran_arg_sizer;
   fmap.referer = &entry_point_referer;
   fmap.prototype_hook = &emit_c_prototype;
   entryp.funcmap = fmap;

   variable i = 0, all_args = Struct_Type[0], appended_args = @Struct_Type[0];

   loop (fmap.nargs) {

	variable fort_arg = entryp.args[i];
	fort_arg.c_symbol = fmap.args[i];			% (b)

	if (not resolve_array_dimensions(entryp, fort_arg))	% (c)
	   return 0;

	all_args = [ all_args, fort_arg.c_symbol ];

	if (fort_arg.type == Fortran_String_Type) {		% (d)

	   variable strlen_arg = make_hidden_str_len_arg(fort_arg, entryp);
	   if (strlen_arg == NULL) return 0;

	   if (SC.fortran_strlen == 1) 				% inlined
		all_args = [ all_args, strlen_arg ];
	   else
		appended_args = [ appended_args, strlen_arg ];	% appended
	}
	i++;
   }

   if (entryp.retval.type != VOID) {				% (d)
	variable retmap = fmap.argmaps[AM_Out][-1];
	retmap.args[0].usage = retmap.args[0].mnemonic;
   }

   fmap.args  = [ all_args, appended_args ];
   fmap.nargs = length(fmap.args);
   return 1;
} % }}}

% Function/Subroutine detection & wrapper emission % {{{

private define emit_aliases() % {{{
{
   if (Aliases.head == NULL) return;

   slirp_emit("static SLang_Intrin_Fun_Type Fortran_Aliases[] =%S\n{\n", FOLD_OPEN);

   foreach(Aliases.head) {

	variable alias = ();

	if (ignored[alias.name])
	   continue;

	slirp_emit("   MAKE_INTRINSIC_0( (char*)\"%s\", %s, SLANG_VOID_TYPE),\n",
					alias.value, "sl_" + alias.name);
   }

   slirp_emit("   SLANG_END_INTRIN_FUN_TABLE\n};   %s\n\n", FOLD_CLOSE);

} % }}}

private define emit_function_wrappers() % {{{
{
   % FIXME: need an option to turn off subroutine(func) wrapping
   foreach(Function_Entry_Points) {

	variable f = (), args = String_Type[0], decls = String_Type[0], arg;
	if (f.retval.type == VOID) continue;

	foreach([f.args])  {
	   arg = ();
	   args = [args, arg.name];
           if (arg.type == "character")
             {
                if ((arg.size != NULL) && (atoi (arg.size) != 0))
                  decls = [sprintf("%s*%S %s", arg.type, arg.size, arg.name), decls ];
                else
                  decls = [sprintf("%s %s", arg.type, arg.name), decls ];
             }
           else
             decls = [sprintf("%s %s", arg.type, arg.name), decls ];
	}

	% First the subroutine declaration
	variable d = sprintf("      subroutine %s%s (", f.name,
	      						Func_Wrapper_Suffix);
	slirp_emit(d);
	variable pos = strlen(d) + 1, i = 0, nargs = length(args);
	while (i < nargs) {
	   if (i) { slirp_emit(","); pos++; }
	   arg = args[i];
	   if (pos >= 60) {			% precaution to avoid lines
		slirp_emit("\n     $ ");		% that are too long
		pos = 8;
	   }
	   slirp_emit("%s",arg);
	   pos += strlen(arg);
	   i++;
	}
	slirp_emit(")\n");

	emit_line("external %s",f.name);	% declare wrapped func as
	decls[-1] = decls[-1] + "," + f.name;	% external, typed accordingly

	% Now the variable declarations
	array_map(Void_Type, &emit_line, decls);

	% And finally the actual function call
	emit_line("%s = %s(%s)",f.retval.name,f.name,strjoin( args[[1:]], ","));
	emit_line("end\n");
   }
} % }}}

private define emit_fortran_wrappers() % {{{
{
   variable out = SC.gendir + "/" + Func_Wrapper_Suffix + "_" + SC.modname+".f";
   variable fp = fopen(out, "w+");
   if (fp == NULL)
	abort("Could not create file for Fortran function wrappers: %s", out);

   variable save_fp = SC.outfp;
   SC.outfp = fp;

   % Ensure this appears as a dependency in generated makefile content
   SC.obj_code = [ SC.obj_code, path_sans_extname(out) + ".o" ];
   SC.ldflags = [ SC.ldflags, "$(FCLIBS)" ];

   emit_comment("This file was generated by SLIRP (version %s)", SC.version);
   emit_comment("(c) 2005-2008 Massachusetts Institute of Technology");
   emit_comment("It contains subroutine wrappers for the Fortran functions");
   emit_comment("(and possibly COMMON blocks) wrapped by the %s module,",
								SC.modname);
   emit_comment("to maximize portability.");
   emit_comment("mnoble@space.mit.edu\n");

   emit_function_wrappers();

   if (SC.fortran_wrapcommon)
	emit_fortran_common_block_wrappers();

   () = fclose(fp);
   SC.outfp = save_fp;

}  % }}}

private define function_decl(fp) % {{{
{
   if (SC.decl == NULL) return 0;
   !if (string_match(SC.decl,
		"\\C^\\([A-Za-z \t*12468()]*\\)[  \t]*function\\(.*\\)", 1))
	return 0;

   variable retval = strtrim_end(get_match(SC.decl,1));
   variable entryp = entry_point_new(retval, get_match(SC.decl,2));
   !if (map_to_c( resolve_symbols(fp, entryp, 0) ))
	 return 0;

   Function_Entry_Points = [ Function_Entry_Points, entryp];

   return 1;
} % }}}

private define subroutine_decl(fp) % {{{
{
   if (SC.decl == NULL) return 0;
   !if (string_match(SC.decl, "\\Csubroutine\\(.*\\)", 1)) return 0;

   variable entryp = entry_point_new(VOID, get_match(SC.decl, 1));
   return map_to_c( resolve_symbols(fp, entryp, 0) );

} % }}}

% }}}

private define parse_fortran(fp) % {{{
{
   while( not feof(fp) )
     {
	if (file_scoped_include(fp))
          continue;

        if (function_decl(fp))
          continue;

        if (subroutine_decl(fp))
          continue;
     }

   () = fclose(fp);
} % }}}

private define start_fortran_parse(fp) % {{{
{
   add_tokens(Single_Char_Tokens, "* - = /");

   parse_fortran(fp);

   remove_tokens(Single_Char_Tokens, "* - = /");
} % }}}

define f2cprotos(file) % {{{
{
   variable ffp = fopen(file, "rb");
   if (ffp == NULL) return NULL;

   SC.infname = file;
   SC.parse_file = &start_fortran_parse;

   !if (SC.fortran_wrapped)	   % fortran_wrapped is a multi-valued flag,
	SC.fortran_wrapped = 1;	   % so we do not reset it if already set

   % Convenience:  automatically add .o file to link line
   SC.obj_code = [ SC.obj_code, path_sans_extname(file) + ".o" ];

   % Ensure SLang-scoped function names do not reflect subroutine wrapping
   SC.renames["\\(.+\\)"+Func_Wrapper_Suffix+"$"] = EMPTY;

   return ffp;
} % }}}

private define emit_module_init_code() % {{{
{
   if (not SC.fortran_wrapped)
      return;

   slirp_emit("\n");

   if (length(Common_Blocks) and SC.fortran_wrapcommon)
	slirp_emit("   if (init_common_blocks(ns) != 0)\n\treturn -1;\n");

   if (Aliases.head != NULL)
	slirp_emit("\n   if (-1 == SLns_add_intrin_fun_table ("+
	     "ns,\n\t\t\tFortran_Aliases, (char*)\"__%s_aliases__\"))\n"+
	     "\treturn -1;\n", SC.modname);

   slirp_emit("\n   transposer = SLang_get_function( (char*)\"transpose\");\n");

} % }}}

register_callback(PRE_GEN_CB, &emit_c_common_block_wrappers);
register_callback(POST_GEN_CB, &emit_fortran_wrappers);
register_callback(POST_GEN_CB, &emit_aliases);
register_callback(POST_INIT_CB, &emit_module_init_code);

#else

define f2cprotos(file) % {{{
{
   return NULL;
} % }}}

#endif

provide("slirpf2c");
