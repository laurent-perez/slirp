
MODULE_NAME	= stubs
HEADERS		= $(MODULE_NAME).h vec.hh
SOURCE		= $(MODULE_NAME).cc
OTHER_CLEAN	= $(TEST_OUTPUT) $(GLUE).cc $(SOURCE) vec.hh

include ../Makefile.inc

MODULE_DEPS = $(GLUE).o stubs.o

$(MODULE): $(MODULE_DEPS)
	@echo
	$(CXX_LINK) -o $@ $(MODULE_DEPS) $(SLANGLIB) $(CXX_LIBS)
	@echo

$(GLUE).cc: $(HEADERS) slirprc
	@echo
	@echo "Generating wrappers ... (SLIRP warnings intentionally generated)"
	@echo
	$(SLIRP) $(HEADERS)

$(SOURCE): $(HEADERS)
	@echo
	@echo Generating stubs ...
	@echo
	$(SLIRP) -stubs -stdout $(HEADERS) > $@

stubs.o: $(SOURCE)
	@echo
	$(CXX_COMPILE) $(SOURCE)

TEST_OUTPUT   = test.out
TEST_BASELINE = test.ref
TEST_SCRIPT   = ./test.sl

test: $(TEST_BASELINE) $(MODULE) $(TEST_OUTPUT)
	$(DIFF) $(TEST_BASELINE) $(TEST_OUTPUT)

$(TEST_OUTPUT): $(MODULE) $(TEST_SCRIPT) ../examples-common.sl
	slsh -n $(TEST_SCRIPT) > $(TEST_OUTPUT) 2>&1

# Grab simple C++ class from cpp examples directory, and ensure that at
# least 1 of its methods is not inlined (and so will be stubbed here)
vec.hh: ../cpp/vec.hh
	cat $? | sed 's/^.*dot.*$$/        double dot(Vec\& v2);/' > $@
	echo "extern int global1;" >> $@
	echo "extern char* global2;" >> $@
