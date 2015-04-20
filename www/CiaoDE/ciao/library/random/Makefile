MODULE=random

include ../../SETTINGS
include ../../SHARED
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

REQLIBS=

# jf- added the _glue suffix, until this lib is compiled with foreign
SRCNAME=$(MODULE).c
OBJNAME=$(MODULE)_$(OSNAME)$(ARCHNAME)_glue.o
LIBNAME=$(MODULE)_$(OSNAME)$(ARCHNAME).so
HEADERS=common_headers.h

phony:

all: $(LIBNAME)

$(LIBNAME): headers $(OBJNAME) 
	$(LD) $(LDSHARED) -o $(LIBNAME) $(OBJNAME) $(REQLIBS)
	$(SETLOCALCIAO) $(CIAOC) -c $(MODULE)
	-chmod $(DATAMODE) $(LIBNAME)

$(OBJNAME): headers $(SRCNAME)
	$(CC) $(CFLAGS) $(CCSHARED) -c $(SRCNAME) -o $(OBJNAME)
	-chmod $(DATAMODE) $(OBJNAME)

headers:
	/bin/rm -f  $(HEADERS)
	echo \#include \"$(CIAOSRC)/bin/$(CIAOARCH)$(CIAODEBUG)/datadefs.h\" > $(HEADERS)
	echo \#include \"$(CIAOSRC)/bin/$(CIAOARCH)$(CIAODEBUG)/support.h\" >> $(HEADERS)
	-chmod $(DATAMODE) $(HEADERS)

clean:
	/bin/rm -f $(OBJNAME)

realclean: clean
	/bin/rm -f $(LIBNAME) $(HEADERS)

distclean:
	/bin/rm -f *.~ $(HEADERS) $(MODULE)_*

allarchs:
	@for machine in $(REMOTEMACHINES); do \
		echo ; \
		echo -------------------------------------------- ; \
		echo ; \
		echo Making in $$machine; \
		echo "unsetenv CIAODEBUG; cd $(CIAOSRC)/library/random; $(MAKE)" | rsh $$machine csh; \
	done
