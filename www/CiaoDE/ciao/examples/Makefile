#------- You should not change ANYTHING in this file ----------------
#------- All customizable options are in the file ../SETTINGS -------
#------- CAUTION: any change in this directory should be also made in the 

include ../SETTINGS
include ../SHARED


EXAMPLESDIR = $(REALLIBDIR)/examples
ALLFILES = *

phony:

# install:
# 	@echo "*** ---------------------------------------------------------"
# 	@echo "*** Installing $(BASEMAIN) examples..."
# 	@echo "*** (in $(EXAMPLESDIR))"
# 	@echo "*** ---------------------------------------------------------"
# 	mkdir -p $(EXAMPLESDIR)
# 	-chmod $(EXECMODE) $(EXAMPLESDIR)
# 	-cp -pR $(ALLFILES) $(EXAMPLESDIR)
# 	-@(cd $(EXAMPLESDIR) && find . -type d -exec chmod $(EXECMODE) {} \; )
# 	-@(cd $(EXAMPLESDIR) && find . ! -type d -exec chmod $(DATAMODE) {} \; )
# 	-@(cd $(EXAMPLESDIR) && find . -name '*~' -exec 'rm' -f {} \; )
# 	-@(cd $(EXAMPLESDIR) && find . -name '*.po' -exec 'rm' -f {} \; )
# 	-@(cd $(EXAMPLESDIR) && find . -name '*.itf' -exec 'rm' -f {} \; )

# uninstall:
# 	@echo "*** ---------------------------------------------------------"
# 	@echo "*** Uninstalling $(BASEMAIN) examples..."
# 	@echo "*** (in $(EXAMPLESDIR))"
# 	@echo "*** ---------------------------------------------------------"
# 	-rm -r $(EXAMPLESDIR)

clean:
	/bin/rm -rf *.itf *.dep *.po 

realclean: clean
	/bin/rm -rf hw hw.cpx
	cd misc && $(MAKE) realclean

distclean: realclean
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up directory."
	@echo "*** ---------------------------------------------------------"
	/bin/rm -rf *~
	cd misc && $(MAKE) distclean
