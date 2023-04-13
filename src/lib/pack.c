
static int pack2for(SLang_Array_Type *slstrs, char** Packed, /* {{{ */
							size_t *Widest)
{
   /* Pack a S-Lang array of slstrings into a sequence */
   /* of space-padded, fixed-length Fortran strings    */
   char *packed, *p, **strings;
   size_t i, *widths, n, width = 0;

   if (slstrs->data_type != SLANG_STRING_TYPE) {
	SLang_verror(SL_USAGE_ERROR,(char*)"String array is required here");
	return -1;
   }

   strings = (char**)slstrs->data;
   n = slstrs->num_elements;

   if ( (widths = (size_t*)SLmalloc( n * sizeof(size_t) )) == NULL)
	return -1;

   for (i=0; i < n; i++) {
	widths[i] = strlen(strings[i]);
	if (widths[i] > width)
	   width = widths[i];
   }

   if ( (packed = SLmalloc( n * width )) == NULL) {
	SLfree((char*)widths);
	return -1;
   }
   memset(packed, ' ', n*width);

   p = packed;
   for (i=0; i<n; i++) {
	strncpy(p, strings[i], widths[i]);
	p += width;
   }

   *Packed = packed;
   *Widest = width;
   SLfree((char*)widths);
   return 0;
} /*}}}*/

static int unpack2c(char *p, SLang_Array_Type *slstrs, size_t width) /*{{{*/
{
   /* Unpack a sequence p of space-padded, fixed-length */
   /* Fortran strings into a SLang array of slstrings   */
   size_t i, n = slstrs->num_elements;
   char *str, **unpacked = (char**)slstrs->data;
   unsigned char free_elements = (slstrs->client_data == NULL);

   if ( (str = SLmalloc(width+1)) == NULL)
	return -1;

   for (i=0; i < n; i++, p += width) {
	memcpy(str, p, width);
	str[width] = 0;
	if (free_elements)
	   SLang_free_slstring(unpacked[i]);
	unpacked[i] = SLang_create_slstring(str);
   }

   SLfree(str);
   return 0;
} /*}}}*/


