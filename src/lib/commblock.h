
typedef struct _CommBlockVar CommBlockVar;

struct _CommBlockVar {
   char *name;
   SLtype sltype;
   void *data;
   int rank;
   SLindex_Type dims[SLARRAY_MAX_DIMS];
   int (*push) (CommBlockVar *);
   int (*pop) (CommBlockVar *);
} ;

typedef void (*CommBlock_Initializer) (void (*) (int*, int*, ...));
typedef struct {
   char *name;
   int num_vars;
   CommBlockVar *vars;
   CommBlock_Initializer init_func;
} CommBlock;

static int push_generic(CommBlockVar *v);
static int pop_generic(CommBlockVar *v);
static int push_fstring(CommBlockVar *v);
static int pop_fstring(CommBlockVar *v);
static int push_scomplex(CommBlockVar *v);
static int pop_scomplex(CommBlockVar *v);
