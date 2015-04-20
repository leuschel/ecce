# Copyright (C) 1997, UPM-CLIP

include ../../SETTINGS
include ../../SHARED

suite:
	$(SETLOCALCIAO) $(CIAOC) suite

exec_suite:
	$(SETLOCALCIAO) ./suite

clean:
	/bin/rm -rf *.itf *.dep *.po 

realclean: clean
	/bin/rm -rf suite

distclean: realclean
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up tests directory."
	@echo "*** ---------------------------------------------------------"
	/bin/rm -rf *~
