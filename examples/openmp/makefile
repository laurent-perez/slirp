
# Demonstration of SLIRP OpenMP support (also uses SLIRP makefile generation)
#
# TODO: add support for
# 	#vectorize(openmp)
# so that parallelization can be activated on a per-function basis,
# instead of for all vectorized functions (as -openmp cl switch does)

MODULE_NAME	= par
HDR		= $(MODULE_NAME).h

GENERATED_MAKEFILE = $(MODULE_NAME).mf
TEST_OUTPUT	= test.out
TEST_BASELINE	= test.ref
TEST_SCRIPT	= $(MODULE_NAME)-test.sl
OTHER_CLEAN	= $(TEST_OUTPUT) $(GLUE).c $(GENERATED_MAKEFILE) *.ps \
				sfwrap_* $(TEST_SCRIPT)
PRE_ALL		= $(GENERATED_MAKEFILE)
GENMAKE		= $(MAKE) -f $(GENERATED_MAKEFILE)

include ../Makefile.inc

$(GENERATED_MAKEFILE): $(HDR)
	$(SLIRP) -openmp -make -m $(MODULE_NAME) \
	   		-rename "\\([a-z]*\\)" p $(HDR)

$(MODULE): $(GENERATED_MAKEFILE)
	$(GENMAKE) $(MODULE)

TEST_APP = slsh -n

test: $(MODULE)
	@$(GENMAKE) test TEST_APP="$(TEST_APP)" | grep -v "^make"
	@$(TEST_APP) verify.sl
