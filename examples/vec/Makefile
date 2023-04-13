# This example demonstrates the SLIRP automatic vectorization capability
# C, C++, and FORTRAN codes are vectorized

MODULE_NAME	= vec
CPP_HEADERS	= computation.hh compat.hh pi.hh
CPP_SRC		= computation.cc pi.cc
CPP_CODE	= $(CPP_HEADERS) $(CPP_SRC)
SLIRP_INPUT	= $(MODULE_NAME).h $(FSRC) $(CPP_HEADERS)
SRC		= $(MODULE_NAME).c $(FSRC) $(SFWRAP) $(CPP_SRC)
OBJ		= $(MODULE_NAME).o $(CPP_SRC:.cc=.o) \
					$(FSRC:.f=.o) $(SFWRAP:.f=.o)
MODULE_OBJ	= $(GLUE).o $(OBJ)
OTHER_CLEAN	= $(GLUE).cc $(CPP_CODE) sfwrap_* slirprc \
					$(TEST_OUTPUT) $(TEST_BASELINE)

fcheck:
	@if [ -z "$(FC)" ] ; then \
	   $(MAKE) all ; \
	else \
	   $(MAKE) -e FSRC=fvec.f SFWRAP=sfwrap_$(MODULE_NAME).f all ; \
	fi

include ../Makefile.inc

MODULE_LIBS =  $(SLANGLIB) $(CXX_LIBS) $(FCLIBS)
$(MODULE): $(MODULE_OBJ)
	@echo
	$(CXX_LINK) -o $@ $(MODULE_OBJ) $(MODULE_LIBS)
	@echo

$(GLUE).cc: slirprc $(SLIRP_INPUT)
	@echo
	$(SLIRP) -d -vec $(SLIRP_INPUT)

TEST_OUTPUT   = test.out
TEST_BASELINE = test.ref
TEST_SCRIPT   = ./test.sl

test:
	@if [ -z "$(FC)" ] ; then \
	   $(MAKE) test_real ; \
	else \
	   $(MAKE) -e FSRC=fvec.f SFWRAP=sfwrap_$(MODULE_NAME).f test_real; \
	fi

test_extra:
	@$(SLSH) vmult-doc-examples.sl
	@echo
	@$(SLSH) tprint.sl

test_real: $(TEST_BASELINE) $(MODULE) $(TEST_OUTPUT)
	$(DIFF) $(TEST_BASELINE) $(TEST_OUTPUT)

$(TEST_OUTPUT): $(MODULE) $(TEST_SCRIPT) ../examples-common.sl
	slsh -n $(TEST_SCRIPT) > $(TEST_OUTPUT) 2>&1

$(CPP_CODE):
	$(CP) ../cpp/$@ .

sfwrap_$(MODULE_NAME).o: sfwrap_$(MODULE_NAME).f

# fmprint will not be vectorized, due to 2D array transposition constraint
FPROTOS = "void fadd(double *x, double *y, int *DIM1, double *OUT);"\
	  "void fmprint(double *matrix, int *DIM1, int *DIM2);"\
	  "void faprint(double *array, int *DIM1);"
slirprc:
	$(RM) $@
	cat renames >> $@
	echo "#vectorize" >> $@
	echo "   void vmult(double *x, int DIM1, double *y, double *OUT);" >> $@
	echo "   void sub1_2d(int *matrix, int DIM1, int DIM2);" >> $@
	echo "   void output(double *, unsigned long DIM1);" >> $@
	if [ ! -z "$(FC)" ] ; then \
	   for p in $(FPROTOS) ; do \
		echo "   $$p" >> $@ ; \
	   done ; \
	fi
	echo "#end" >> $@


$(TEST_BASELINE): slang1.ref slang2.ref
	$(RM) $@
	$(CP) slang`echo $(SLANG_VERSION) | cut -c1`.ref $@
