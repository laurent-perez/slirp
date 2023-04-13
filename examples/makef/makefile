
# Simple demonstrattion of SLIRP make file generation

MODULE_NAME	= cos

GENERATED_MAKEFILE = $(MODULE_NAME).mf
TEST_OUTPUT	= test.out
TEST_BASELINE	= test.ref
TEST_SCRIPT	= $(MODULE_NAME)-test.sl
OTHER_CLEAN	= $(TEST_OUTPUT) $(GLUE).c $(GENERATED_MAKEFILE) \
				sfwrap_* $(TEST_SCRIPT)
PRE_ALL		= $(GENERATED_MAKEFILE)
GENMAKE		= $(MAKE) -f $(GENERATED_MAKEFILE)

include ../Makefile.inc

DEPS = cos.h one.f
# NB: the -lm library/link flag implicitly turns on -make
$(GENERATED_MAKEFILE): $(DEPS)
	$(SLIRP) -lm $(DEPS)

$(MODULE): $(GENERATED_MAKEFILE)
	$(GENMAKE) $(MODULE)

TEST_SCRIPT_REF = test.ref

test: $(MODULE)
	$(GENMAKE) test \
		| grep -v "^make" > $(TEST_OUTPUT)
	diff $(TEST_SCRIPT_REF) $(TEST_OUTPUT)
