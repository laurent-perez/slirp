
MODULE_NAME	= ksink
OTHER_CLEAN	= test.out $(GLUE).c

include ../Makefile.inc

SLIRP       = ../../src/slirp_exe
SLIRP_ARGS  = $(SLIRP_DEBUG) -refscalars -nopop \
  -mapnames datum_copy ksink_datum_copy \
  -rename "ksink_print_error" Print_Error
SLIRP_CMD = $(SLIRP) $(SLIRP_ARGS)

MODULE_DEPS = $(GLUE).o ksink.o

EXAMPLE_SCRIPT = example.sl

demo:	$(MODULE)
	@echo
	@echo Now running the example script ...
	@echo
	$(SLSH) $(EXAMPLE_SCRIPT)

$(MODULE): $(MODULE_DEPS)
	@echo
	$(C_LINK) -o $@ $(MODULE_DEPS) $(SLANGLIB)
	@echo

$(GLUE).o: $(GLUE).c
	@echo
	$(C_COMPILE) $(GLUE).c

$(GLUE).c: slirprc ksink.h
	@echo
	@echo Generating code ...
	@echo
	$(SLIRP_CMD) ksink.h
	@# Now verify SLIRP idempotence
	@$(SLIRP_CMD) ksink.h 2>/dev/null

ksink.o: ksink.c ksink.h
	@echo
	$(C_COMPILE) ksink.c

test: $(MODULE)
	slsh ./test.sl > test.out 2>&1
	diff slang2.ref test.out
#
.PHONY: test clean distclean
