
SHELL		= /bin/sh
DATA		= i686linux.dat solaris8.dat
PLOTS		= $(DATA:.dat=.ps)
TABLE		= perf.eps
OUTPUT		= $(PLOTS) $(TABLE)
TABLETEX	= perf.tex
GRAPH		= ./graph
TABULATE	= ./tabulate
LINK		= vec-module.so
OTHER_CLEAN	= *.log *.aux *.dvi *.*ps $(TABLETEX) $(PLOTS)

.SUFFIX:  .dat .ps

figures: $(LINK) $(GRAPH) $(TABULATE) $(OUTPUT) msg

include ../../Makefile.inc

msg:
	@echo Generated output products are: $(PLOTS) $(TABLE)

view:  figures
	for file in $(OUTPUT) ; do \
	   (gv $$file & ) ; \
	done

$(PLOTS): $(DATA)
	$(GRAPH) -ps $(DATA)

$(TABLE): $(TABLETEX)
	tex2ps -ev $?

$(TABLETEX): $(TABULATE) $(DATA)
	$(TABULATE) $(DATA) > $@

$(LINK):
	ln -s ../$@ .
