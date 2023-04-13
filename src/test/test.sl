#ifnexists preproc_activate
import ("./preproc");
preproc_activate ();
() = evalfile (__FILE__);
#stop
#endif

static define check_exists(h)
{
   if (preproc_handler_exists(h))
	vmessage("preproc handler for %s IS installed",h);
   else
	vmessage("preproc handler for %s IS NOT installed!",h);
}

static define funcmap_handler(line, context)
{
   if (context.buf == NULL) {
	() = printf("\nFUNCMAP HANDLER: BEGIN\n");
	context.buf = line;
	return;
   }

   if (line == NULL)
	() = printf("<%S>\nFUNCMAP HANDLER: END\n",context.buf);
   else
	context.buf = strcat(context.buf, line);
}

static define tcl_handler(line)
{
   vmessage("tcl_handler: line = <%S>",line);
}

variable Prep_Context = struct {buf};
preproc_handler_add("funcmap", , &funcmap_handler, @Prep_Context);
preproc_handler_add("inline_tcl","end_tcl", &tcl_handler);

#funcmap This_Is_The_Blah_Funcmap
blah1
blah2 blah2 blah2 blah2 blah2 blah2 blah2 blah2
blah3			blah3						blah3			blah3

	blah4
	continued ....
#end

#inline_tcl
let's
	pretend that this 
				is 
	some Tcl/Tk
code
#end_tcl

() = printf("\n");
check_exists("funcmap");
check_exists("inline_tcl");
check_exists("DummY");

preproc_handler_remove("funcmap");
check_exists("funcmap");

preproc_handler_remove("inline_tcl");
preproc_handler_remove("inline_tcl");
check_exists("tcl");

preproc_handler_add("inline_tcl","end_tcl", &tcl_handler);

#inline_tcl
	another
		silly
			example
				snippet
#end_tcl
