
CPP_DEMO	= cppdemo
MODULE_NAME	= $(CPP_DEMO)
C_DEMO		= cdemo
CPP_GLUE	= $(CPP_DEMO)_glue
C_GLUE		= $(C_DEMO)_glue
MISC		= misc
OTHER_ALL	= $(CPP_DEMO) $(C_DEMO) $(MODULE) $(MISC)_deps
OTHER_CLEAN	= $(OTHER_ALL) $(TEST_SCRIPT_REF) $(CPP_GLUE).* $(C_GLUE).* \
   		  *.out *$(SH_EXT) $(MISC) $(MISC)_glue.* foo*

include ../Makefile.inc

CPP_HEADERS = cdecl.hh computation.hh pi.hh err.hh vec.hh
CPP_OBJS    = computation.o pi.o
MODULE_DEPS = $(CPP_GLUE).o $(CPP_OBJS)

EXAMPLE_SCRIPT = $(CPP_DEMO).sl

$(MODULE): $(MODULE_DEPS)
	@echo
	$(CXX_LINK) -o $@ $(MODULE_DEPS) $(SLANGLIB) \
						$(SO_DEP_LIBS) $(CXX_LIBS)
	@echo

$(CPP_GLUE).o: $(CPP_GLUE).cc
	@echo
	$(CXX_COMPILE) $?

$(CPP_GLUE).cc: $(CPP_HEADERS)
	@echo
	@echo Generating S-Lang wrappers for C++ code ...
	@echo
	$(SLIRP) -m $(CPP_DEMO) $(CPP_HEADERS)

$(C_GLUE).cc: $(CPP_HEADERS)
	@echo
	@echo Generating pure C wrappers for C++ code ...
	@echo
	$(SLIRP) -m $(C_DEMO) -cfront $(CPP_HEADERS)

pi.o: pi.cc pi.hh computation.hh
computation.o: computation.cc computation.hh
cppdemo.o: cppdemo.cc err.hh
#
$(CPP_DEMO): cppdemo.o $(CPP_OBJS)
	$(CXX) $(LDFLAGS) -o $@ cppdemo.o $(CPP_OBJS) $(SLANGLIB) $(SO_DEP_LIBS)

$(C_DEMO): $(C_DEMO).o $(C_GLUE).o $(CPP_OBJS)
	$(CXX) $(LDFLAGS) -o $@ $@.o $(CPP_OBJS) $(C_GLUE).o $(SLANGLIB) $(SO_DEP_LIBS)

$(C_GLUE).o:  $(C_GLUE).cc  $(C_GLUE).h
$(C_DEMO).o: $(C_DEMO).c $(C_GLUE).cc

TEST_SCRIPT = ./test.sl
TEST_SCRIPT_REF = test.ref
DEMO_EXE_REF = exe-output.ref

test: test_cppdemo test_cdemo test_script test_misc
	@true

test_cdemo: $(C_DEMO)
	./$(C_DEMO) > $(C_DEMO).out
	$(DIFF) $(DEMO_EXE_REF) $(C_DEMO).out
	@echo

test_cppdemo: $(CPP_DEMO)
	./$(CPP_DEMO) > $(CPP_DEMO).out
	$(DIFF) $(DEMO_EXE_REF) $(CPP_DEMO).out
	@echo

test_script: $(MODULE) $(TEST_SCRIPT) $(EXAMPLE_SCRIPT) $(TEST_SCRIPT_REF)
	slsh $(TEST_SCRIPT) > $(TEST_SCRIPT).out 2>&1
	$(DIFF) $(TEST_SCRIPT_REF) $(TEST_SCRIPT).out

QUIET = 2>errs.out || (tail -20 errs.out && false)
# Miscellaneous C++ tests
misc_deps: misc-module$(SH_EXT) $(MISC)_test.sl $(MISC)
test_misc: misc_deps
	@echo
	slsh $(MISC)_test.sl > $(MISC).sl.out 2>&1
	./$(MISC) > $(MISC).out 2>&1
	$(DIFF) $(MISC).sl.out $(MISC).out
	$(SLIRP) -m foo -cfront -rc /dev/null misc.hh $(QUIET)
	@echo
	@echo "Ensure #argmap(ignore) is applied for string arrays with -cfront"
	test `grep stringlist_print foo_glue.cc | wc -l` = 0 || false

misc_glue.o: misc_glue.cc
misc-module$(SH_EXT): misc_glue.o
	$(CXX_LINK) -o $@ misc_glue.o $(SLANGLIB) $(SO_DEP_LIBS) $(CXX_LIBS)

misc_glue.cc: misc.hh slirprc
	@echo
	@echo Generating S-Lang wrappers for miscellaneous C++ code ...
	@echo
	$(SLIRP) -rc /dev/null misc.hh $(QUIET)

$(MISC): $(MISC).cc
	$(CXX) -o $@ $? $(SO_DEP_LIBS) $(CXX_LIBS)

$(TEST_SCRIPT_REF): slang1.ref slang2.ref
	$(CP) slang`echo $(SLANG_VERSION) | cut -c1`.ref $@
