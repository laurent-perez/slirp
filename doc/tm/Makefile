#
# To create the SGML files, you will need to install the tm-utils
# package.  See http://www.jedsoft.org/ for more information.

TMDIST   =  /nfs/aluche/d1/web/tm-dist
TM2SGML  = $(TMDIST)/bin/tmexpand
MACRODIR = $(TMDIST)/macros
DB2MAN   = docbook2man

STEM       = manual
TXT_FILES  = $(STEM).txt
SGML_FILES = $(STEM).sgml
HTML_FILES = $(STEM).html
TEX_FILES  = $(STEM).tex
#PS_FILES   = $(STEM).ps
PDF_FILES  = $(STEM).pdf
MAN_PAGES  = slirpsh.1 slirp.1
MAN_SGML   = $(MAN_PAGES:%.1=%.sgml) 

SAMPLE_CODE=munged_ksink.h

SGML2LATEX = sgml2latex -p letter -o tex
SGML2HTML = sgml2html
SGML2TXT = sgml2txt -f
LATEX = latex
PDFLATEX = pdflatex

DOC_IMAGES = combined.png  numiters.png  stride2.png  vecperf.png \
		vecperf2.png numelems.png  stride.png    vacuous.png \
		linux-n2.png solaris-n4.png

TEXTDIR = ../text
PSDIR = ../ps
HTMLDIR = ../html
SGMLDIR = ../sgml
PDFDIR = ../pdf
MANDIR = ../man/man1

.PHONY:  all install uninstall clean distclean texclean
.SUFFIXES: .sgml .tm .1 .man

SUBDIRS = $(TEXTDIR) $(HTMLDIR) $(SGMLDIR) $(PDFDIR) $(MANDIR) #$(PSDIR)
SRCDIR = `pwd`

all: $(SGML_FILES) $(HTML_FILES) $(TEX_FILES) $(TXT_FILES) $(PDF_FILES) \
								$(MAN_PAGES)
dummy:

text-files: $(TXT_FILES)

#----- SGML Files -----------------------------------------------------------
$(STEM).sgml : $(STEM).tm preface.tm $(SAMPLE_CODE)
	$(TM2SGML) -I$(MACRODIR) $(STEM).tm $(STEM).sgml

#----- HTML Files -----------------------------------------------------------
$(STEM).html : $(STEM).sgml
	$(SGML2HTML) $(STEM).sgml

#----- TeX Files ------------------------------------------------------------

$(STEM).tex : $(STEM).sgml fixtex.sl
	$(SGML2LATEX) $(STEM).sgml
	jed -script ./fixtex.sl $(STEM).tex

#----- PDF Files -----------------------------------------------------------

$(STEM).pdf : $(STEM).tex
	-\rm $@
	$(PDFLATEX) $(STEM).tex
	$(PDFLATEX) $(STEM).tex

#----- PS Files -----------------------------------------------------------

$(STEM).ps : $(STEM).pdf
#	$(LATEX) $(STEM).tex
#	$(LATEX) $(STEM).tex
#	dvips -o $(STEM).ps $(STEM).dvi

#----- Text Files -----------------------------------------------------------

$(STEM).txt: $(STEM).sgml
	$(SGML2TXT) $(STEM).sgml

$(SAMPLE_CODE): ../../examples/kitchensink/ksink.h
	@# Recall that some seds do not support + char to denote "one or more"
	sed -e 's/\/\* COMMENT EMBEDDED HERE FOR TESTING PURPOSES\*\//		/' \
	-e 's/([ 	][ 	]*/(/' \
	-e 's/[ 	]*,[ 	]*/, /' \
	-e '/^[ 	]*#define.*$$/d' \
	-e '/^.*#include.*$$/d' \
	-e '/^.*ksink_mult2.*$$/d' \
	-e '/^.*ksink_ignorable_func.*$$/d' \
	-e '/^.*VoidStar.*$$/d' \
	-e '/^.*ksink_subtract_ptrs.*$$/d' \
	-e '/^.*KSINK_IGNORABLE_.*$$/d' \
	-e '/^.*ksink_sizeof_int.*$$/d' \
	-e '/^.*ksink_equals_array.*$$/d' \
	-e '/^.*ksink_arg_dropper.*$$/d' \
	-e '/^.*ksink_copy_string.*$$/d' \
	-e '/^.*datum_copy.*$$/d' \
	-e '/^.*print_array_datum.*$$/d' \
	-e '/^.*make_array_datum.*$$/d' \
	-e '/^.*[Dd]ummy.*$$/d' \
	-e '/^.*ksink_toggle_error_hook.*$$/d' \
	-e '/^.*ksink_ftell.*$$/d' \
	-e '/^.*KInt.*$$/d' \
	-e '/^[ 	][ 	]*double.*$$/d' \
	-e 's/DECL(\(.*\))\([ 	][ 	]*\)/extern	\1\2/' \
	-e 's/		ksink_params_new/	ksink_params_new/' \
	-e '/\/\*.*\*\//d' $? | cat -s > $@

.tm.sgml:
	$(TM2SGML) -I$(MACRODIR) $? $*.sgml

.sgml.1:
	$(DB2MAN) $?
	@# Undo docbook2man unilateral conversion of \ to \\
	cat $@ | sed 's/\\\\/\\/g' > $@.tmp
	mv $@.tmp $@

#----------------------------------------------------------------------------
clean:
	-rm -f *~ *.dvi *.log *.aux *.toc *.out $(PDF_FILES) $(SAMPLE_CODE) \
		manpage.refs manpage.links $(MAN_PAGES) $(MAN_SGML)

distclean: clean
	-rm -f *.html $(STEM).tm $(TXT_FILES) $(TEX_FILES) \
				$(SGML_FILES) *.ps

install-txt-files: $(TXT_FILES)
	-mv $(TXT_FILES) ../text

install: all move_files clean
   
move_files: #$(PS_FILES)
	-mkdir -p $(SUBDIRS)
	-mv $(TXT_FILES) ../text
	-mv *.html ../html
	-cp -f $(DOC_IMAGES) $(HTMLDIR)
	#-mv $(PS_FILES) ../ps
	-mv $(SGML_FILES) ../sgml
	-mv $(PDF_FILES) $(PDFDIR)
	-mv $(MAN_PAGES) $(MANDIR)

uninstall:
	if [ ! -z "$(SUBDIRS)" ] ; then \
	  \rm -rf $(SUBDIRS) ; \
	fi
