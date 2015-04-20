MODULE=fastrw

include ../../SETTINGS
include ../../SHARED
include $(CIAOSRC)/makefile-sysindep
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

SRCNAME=$(MODULE).c
OBJNAME=$(MODULE)_$(OSNAME)$(ARCHNAME).o
LIBNAME=$(MODULE)_$(OSNAME)$(ARCHNAME).so
HEADERS=common_headers.h

$(LIBNAME): headers $(OBJNAME) 
	$(LD) $(LDSHARED) -o $(LIBNAME) $(OBJNAME) $(REQLIBS)
	-@chmod $(EXECMODE) $(LIBNAME)

$(OBJNAME): headers $(SRCNAME)
	$(CC) $(CFLAGS) $(CCSHARED) -c $(SRCNAME) -o $(OBJNAME)

headers:
	/bin/rm -f  $(HEADERS)
	echo \#include \"$(CIAOSRC)/bin/$(CIAOARCH)/datadefs.h\" > $(HEADERS)
	echo \#include \"$(CIAOSRC)/bin/$(CIAOARCH)/support.h\" >> $(HEADERS)

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
		echo "unsetenv CIAODEBUG; cd $(CIAOSRC)/library/fastrw; gmake" | rsh $$machine csh; \
	done
