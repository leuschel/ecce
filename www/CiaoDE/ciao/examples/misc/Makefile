# Copyright (C) 1997, UPM-CLIP

include ../../SETTINGS
include ../../SHARED

suite:
	$(SETLOCALCIAO) $(CIAOC) suite

exec_suite: suite
	$(SETLOCALCIAO) ./suite

clean:
	/bin/rm -rf *.itf *.dep *.po 

realclean: clean
	/bin/rm -rf suite suite.cpx

distclean: realclean
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up tests directory."
	@echo "*** ---------------------------------------------------------"
	/bin/rm -rf *~
