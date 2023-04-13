
FDEMO		= fdemo
FSUBS		= fsubs
FUNC_WRAPPERS	= sfwrap_$(FSUBS)
INC		= -I..
CSUBS		= csubs
CDEMO		= cdemo
MODULE_NAME	= fsubs
SLSH		= slsh -n
PRE_ALL		= test_iobug
OTHER_ALL	= $(CDEMO) $(FDEMO)
OTHER_CLEAN	= $(TEST_OUTPUT) $(GLUE).c $(TEST_BASELINE) iobug \
  		$(MODULE_NAME).f.h fdemo cdemo *.out $(FUNC_WRAPPERS).* \
		*fsubs_alt.* ignored.txt

sanity:
	@if [ -z "$(FC)" ] ; then \
	   echo "No FORTRAN compiler was found on your system." ; \
	else \
	   $(MAKE) all ; \
	fi

include ../Makefile.inc

MODULE_DEPS = $(GLUE).o $(FSUBS).o $(CSUBS).o $(FUNC_WRAPPERS).o 

EXAMPLE_SCRIPT = example.sl

demo:	$(MODULE)
	@echo
	@echo Now running the example script ...
	@echo
	$(SLSH) $(EXAMPLE_SCRIPT)

$(MODULE): $(MODULE_DEPS)
	@echo
	$(C_LINK) -o $@ $(MODULE_DEPS) $(SLANGLIB) $(FCLIBS)
	@echo

$(GLUE).o: $(GLUE).c
	@echo
	$(C_COMPILE) $(GLUE).c

$(GLUE).c: slirprc $(FSUBS).f $(CSUBS).h
	@echo
	@echo Generating mixed FORTRAN/C wrappers ...
	@echo
	$(SLIRP) $(INC) -d $(FSUBS).f $(CSUBS).h

$(FSUBS).o: $(FSUBS).f
	@echo
	$(FORTRAN_COMPILE) $(INC) $(FSUBS).f
bug_glue.c: bug.f
	@echo
	sldb $(SLIRP) $(INC) -d bug.f
bug.o: bug.f
	$(FORTRAN_COMPILE) $(INC) bug.f

TEST_SCRIPT = ./sldemo.sl
TEST_SCRIPT_REF = test.ref
DEMO_EXE_REF = test.ref
DEMO_INPUT = coeffs.dat

test:
	@if [ -z "$(FC)" ] ; then \
	   echo "No FORTRAN compiler was found on your system." 1>&2 ; \
	else \
	   $(MAKE) test_iobug test_fdemo test_cdemo test_sldemo test_misc ; \
	fi

$(FUNC_WRAPPERS).o: $(FUNC_WRAPPERS).f

# C compiler used to link cdemo, to help avoid main() issues with FORTRAN
CDEMO_DEPS = $(CDEMO).o $(FSUBS).o $(FUNC_WRAPPERS).o $(CSUBS).o
$(CDEMO): $(MODULE) $(CDEMO_DEPS)
	$(CC) $(CFLAGS) -o $@ $(CDEMO_DEPS) $(FCLIBS)

$(FDEMO): $(FSUBS).o $(FDEMO).f
	$(FORTRAN_LINK) $(FCFLAGS) -o $@ $@.f $(FSUBS).o $(FCLIBS)

test_cdemo: $(CDEMO)
	./$(CDEMO) < $(DEMO_INPUT) > $(CDEMO).out
	$(DIFF) $(DEMO_EXE_REF) $(CDEMO).out
	@echo

test_fdemo: $(FDEMO)
	@# Avoid problems with FORTRAN compilers that choke on read-only files
	@chmod +w $(DEMO_INPUT)
	./$(FDEMO) > $(FDEMO).out
	$(DIFF) -b $(DEMO_EXE_REF) $(FDEMO).out
	@echo

test_sldemo: $(MODULE) $(TEST_SCRIPT) $(TEST_SCRIPT_REF)
	$(SLSH) $(TEST_SCRIPT) < $(DEMO_INPUT) > $(TEST_SCRIPT).out 2>&1
	$(DIFF) $(TEST_SCRIPT_REF) $(TEST_SCRIPT).out

test_misc: $(MODULE) misc.sl
	$(SLSH) misc.sl > misc.out 2>&1
	$(DIFF) -b misc.ref misc.out
	@# Now sanity check non-default behavior for hidden string length args
	$(SLIRP) $(INC) -d -m fsubs_alt -stdout -fortran_strlen 1 \
		$(FSUBS).f $(CSUBS).h | tee fsubs_alt.c | \
	 	grep continuatorsfwrap | grep "char\*,size_t,"
	$(C_COMPILE) fsubs_alt.c

iobug: iobug.f
	$(FORTRAN_LINK) $(FCFLAGS) $(LDFLAGS) -o $@ $@.f $(FCLIBS)

test_iobug: iobug
	./iobug > iobug.out
	 if test -s iobug.out ; then cat iobug.out >&2 ; false; fi
