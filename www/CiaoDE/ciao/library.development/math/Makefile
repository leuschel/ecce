MODULE=math_c
REQLIBS=-lm

include ../../SETTINGS
include $(CIAOSRC)/makefile-sysindep
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

SRCNAME=$(MODULE).c
OBJNAME=$(MODULE)_$(CIAOARCH).o
LIBNAME=$(MODULE)_$(CIAOARCH).so
HEADERS=common_headers.h

$(LIBNAME): headers $(OBJNAME) 
	$(LD) $(LDSHARED) -o $(LIBNAME) $(OBJNAME) $(REQLIBS)

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
	/bin/rm -f $(HEADERS) $(MODULE)_*
