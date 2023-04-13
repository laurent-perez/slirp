
#define FIELD_SET(klass, field, ctype, sltype)\
void c__##klass##_##field##_set (void *arg1, sltype arg2) \
{\
   ((klass*)arg1)->field = (ctype) arg2;\
}

#define FIELD_GET(klass, field, ctype, sltype)\
sltype c__##klass##_##field##_get (void *arg1) \
{\
   return (ctype) ((klass*)arg1)->field;\
}

