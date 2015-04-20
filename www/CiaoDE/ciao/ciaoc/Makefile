#------- You should not change ANYTHING in this file ----------------
#------- All customizable options are in the file ../SETTINGS -------

include ../SETTINGS
include ../SHARED

CIAOCNAME=ciaoc
ASRGEN=gen_asr

compiler:
#	$(SETBOOTSTRAPCIAO) $(BOOTSTRAP_CIAOC) -s -x $(CIAOCNAME)
	-rm -f $(CIAOC)
	$(SETLOCALCIAO) $(BOOTSTRAP_CIAOC) -s  -x -o $(CIAOC) $(CIAOCNAME)
	-ln -sf $(CIAOCNAME)-$(VERSION)$(CIAOSUFFIX) $(SRCBINDIR)/$(CIAOCNAME)
	$(SETLOCALCIAO) $(CIAOC) -s -x $(ASRGEN)
	-rm -f $(ASRGEN)$(CIAOSUFFIX)
	-rm -f $(ASRGEN).bat

install:
	mkdir -p $(BUILDBINDIR)
	-cp $(CIAOC) \
		$(BUILDBINDIR)/$(CIAOCNAME)-$(VERSION)$(CIAOSUFFIX)
	-chmod $(EXECMODE) $(BUILDBINDIR)/$(CIAOCNAME)-$(VERSION)$(CIAOSUFFIX)
	-rm -f $(BUILDBINDIR)/$(CIAOCNAME)$(CIAOSUFFIX)
	-ln -s $(CIAOCNAME)-$(VERSION)$(CIAOSUFFIX) \
		$(BUILDBINDIR)/$(CIAOCNAME)$(CIAOSUFFIX) 

uninstall: uninstall_$(INSTYPE)

uninstall_src:
	@echo: "NOTE: Skipping removal of compiler files ..."

uninstall_ins:
	@/bin/rm -f $(BUILDBINDIR)/$(CIAOCNAME)-$(VERSION)$(CIAOSUFFIX) 
	@if [ ! -f $(BUILDBINDIR)/$(CIAOCNAME)$(CIAOSUFFIX) ] ; then \
		/bin/rm -f $(BUILDBINDIR)/$(CIAOCNAME)$(CIAOSUFFIX) ; \
	fi

distclean: cleantmpfiles cleanexecs

realclean: cleantmpfiles

cleanbackups:
	/bin/rm -f *~ *bak
cleantmpfiles:
	/bin/rm -f *.po *.itf *.asr

# Remember not to erase our only static compiler!
cleanexecs:
	-/bin/mv $(STATICCOMPNAME) temp-$(STATICCOMPNAME)
# Mask out owner-[executable,readable,writable] files
	find . -name 'ciaoc*' -perm -000700 -exec /bin/rm -f {} \;
# Include also any stray .cpx compiler (sometimes not executable!)
	find . -name 'ciaoc.cpx' -exec /bin/rm -f {} \;
	-/bin/mv temp-$(STATICCOMPNAME) $(STATICCOMPNAME)
	rm -f $(ASRGEN) $(ASRGEN)$(CIAOSUFFIX)
