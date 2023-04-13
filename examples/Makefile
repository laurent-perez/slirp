EXAMPLES = kitchensink cpp fortran makef stubs gl vec

all: build

build:
	@topdir=`pwd`; \
	for dir in $(EXAMPLES); \
	do \
	   echo Building $$dir; \
	   cd $$dir && $(MAKE) > /dev/null 2>test.log ; \
	   if [ "$$?" != "0" ]; then \
	     echo "*** Build of $$dir failed"; \
	   fi; \
	   cd $$topdir; \
	done

clean:
	/bin/rm -f *~
	cd openmp; $(MAKE) clean
	@topdir=`pwd`; \
	for dir in $(EXAMPLES); \
	do \
	   echo Cleaning $$dir; \
	   /bin/rm -f $$dir/test.log; \
	   cd $$dir && $(MAKE) clean; \
	   cd $$topdir; \
	done
#
distclean: clean
	cd openmp; $(MAKE) clean
	@topdir=`pwd`; \
	for dir in $(EXAMPLES); \
	do \
	   cd $$dir && $(MAKE) distclean; \
	   cd $$topdir; \
	done
	/bin/rm -f Makefile.inc
#
check:
	@topdir=`pwd`; \
	for dir in $(EXAMPLES); \
	do \
	   echo Testing $$dir; \
	   cd $$dir && $(MAKE) > /dev/null 2>test.log && $(MAKE) test >test.log; \
	   if [ "$$?" != "0" ]; then \
	     echo "*** Test in $$dir failed"; \
	   else \
	     echo "Passed"; \
	   fi; \
	   cd $$topdir; \
	done
#
