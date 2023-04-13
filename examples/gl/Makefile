
# OpenGL demo module:	exercises the SLIRP conditional preprocessor, as well
# 			as the generation of stub code and Makefiles

MODULE_NAME	= gl
GENERATED_MAKEFILE = $(MODULE_NAME).mf
TEST_SCRIPT     = $(MODULE_NAME)-test.sl
OTHER_CLEAN	= $(TEST_OUTPUT) *_glue.c $(GENERATED_MAKEFILE) \
				$(TEST_SCRIPT) $(MODULE_NAME)_stubs.c
PRE_ALL		= $(GENERATED_MAKEFILE)
GENMAKE		= $(MAKE) -f $(GENERATED_MAKEFILE)

include ../Makefile.inc

DEPS = gl.h glext.h

$(GENERATED_MAKEFILE): $(DEPS)
	$(SLIRP) -m $(MODULE_NAME) -stubs -make $(DEPS)

$(MODULE): $(GENERATED_MAKEFILE)
	$(GENMAKE) $(MODULE) | grep -v test

test: $(MODULE)
	$(GENMAKE) test
#	$(GENMAKE) TEST_APP="../../slsh -n" test
