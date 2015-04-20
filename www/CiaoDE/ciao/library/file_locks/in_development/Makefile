MODULE=file_locks
REQLIBS=

include ../../../SETTINGS
include ../../../SHARED
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

SRCNAME=$(MODULE).c
OBJNAME=$(MODULE)_$(OSNAME)$(ARCHNAME).o
LIBNAME=$(MODULE)_$(OSNAME)$(ARCHNAME).so
HEADERS=common_headers.h

all:	$(OBJNAME) $(LIBNAME) 
	$(SETLOCALCIAO) $(CIAOC) -x -c $(MODULE)

$(LIBNAME): $(HEADERS) $(OBJNAME) 
	$(LD) $(LDSHARED) -o $(LIBNAME) $(OBJNAME) $(REQLIBS)

$(OBJNAME): $(HEADERS) $(SRCNAME)
	$(CC) $(CFLAGS) $(CCSHARED) -c $(SRCNAME) -o $(OBJNAME)

$(HEADERS):
	/bin/rm -f  $(HEADERS)
	echo \#include \"$(CIAOSRC)/bin/$(CIAOARCH)$(CIAODEBUG)/datadefs.h\" > $(HEADERS)
	echo \#include \"$(CIAOSRC)/bin/$(CIAOARCH)$(CIAODEBUG)/support.h\" >> $(HEADERS)

clean:
	/bin/rm -f $(OBJNAME)

realclean: clean
	/bin/rm -f $(LIBNAME) $(HEADERS)

distclean:
	/bin/rm -f $(HEADERS) $(MODULE)_* *~ *.itf *.po *.asr
