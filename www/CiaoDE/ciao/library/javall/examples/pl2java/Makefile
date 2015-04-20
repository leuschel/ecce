include ../../../../SETTINGS
include ../../../../SHARED
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

PROLOGSOURCES=*.pl

all: prolog

prolog:
	$(SETLOCALCIAO) $(CIAOC) -c $(PROLOGSOURCES)
	$(SETLOCALCIAO) $(CIAOC) $(PROLOGSOURCES)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	/bin/rm -f *.itf *.po *.dep *.asr

distclean: realclean
	/bin/rm -f *~ calculator



