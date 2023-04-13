#define SLang_push_scomplex(c)		SLang_push_complex(c.r, c.i)
#define SLang_push_dcomplex(c)		SLang_push_complex(c.r, c.i)
#define SLang_pop_dcomplex(dc)	SLang_pop_complex((double*)dc,((double*)dc)+1)
#define TRANSPOSE(i,ARRAY)	(ARRAY)->num_dims > 1 ? transpose(i,(ARRAY)) : 0

static int SLang_pop_scomplex (spcomplex *c) /* {{{ */
{
   double dc[2];
   if (SLang_pop_complex(dc, dc+1) == -1)
	return -1;
   c->r = (float) dc[0]; /* information may be lost to truncation */
   c->i = (float) dc[1];
   return 0;
} /* }}} */

static SLang_Name_Type *transposer;

static int transpose(int to_row_major, SLang_Array_Type *a)  /* {{{ */
{
   void *tmpdata;
   SLang_Array_Type *transposed;
   static SLindex_Type dims[SLARRAY_MAX_DIMS];

   if (to_row_major) {
	unsigned int i = a->num_dims, j = 0;
	memcpy(dims, a->dims, a->num_dims * sizeof(SLindex_Type));
	while (i--)
	   a->dims[i] = dims[j++];
   }

   if (SLang_push_array(a, 0) == -1 || SLexecute_function(transposer) == -1
	 			    || SLang_pop_array(&transposed)   == -1)
	return -1;

   tmpdata = a->data;
   a->data = transposed->data;
   transposed->data = tmpdata;
   SLang_free_array(transposed);

   if (to_row_major)
	memcpy(a->dims, dims, a->num_dims * sizeof(SLindex_Type));

   return 0;
} /* }}} */

static int pop_space_padded_string (char **sp, size_t min_len)
{
   char *s;
   char *b;
   size_t len;

   *sp = NULL;
   if (-1 == SLang_pop_slstring (&s))
     return -1;

   if (NULL == (b = (char *)SLmalloc (min_len+1)))
     {
        SLang_free_slstring (s);
        return -1;
     }
   len = strlen (s);
   if (len > min_len) len = min_len;
   memcpy (b, s, len);
   memset (b+len, ' ', min_len-len);
   b[min_len] = 0;

   SLang_free_slstring (s);
   *sp = b;
   return 0;
}

static void free_space_padded_string (char *s)
{
   SLfree (s);                         /* NULL ok */
}
