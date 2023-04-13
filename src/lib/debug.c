
static void slirp_debug_pause(char *module_name) /* {{{ */
{
   const char *var = getenv("SLIRP_DEBUG_PAUSE");
   if (var == NULL)
	return;
 {
   const char *prefix  = "slirp_debug_pause:";
   int length = atoi(var);

   if (module_name == NULL)
	module_name = (char*)"";

   if (length < 0) {
	length = abs(length);
	fprintf(stderr,"\n%s entered, ", prefix);
	fprintf(stderr,"will exit after %d second(s) ...\n",length);
#ifdef __WIN32__
	_sleep(length * 1000);
#else
	sleep(length);
#endif
	return;
   }

   fprintf(stderr,"\n%s %s module symbols have been loaded",prefix,module_name);
   fprintf(stderr,"\n%s set breakpoint(s) in debugger window, then",prefix);
   fprintf(stderr,"\n%s press any key in this window to continue\n",prefix);
   length = getchar();
 }
} /* }}} */

