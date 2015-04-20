# ----------------------------------------------------------------------------
# Makefile used to build and install the Ciao-related emacs files
# You should probably not change anything in here (see ../SETTINGS)
# 
# Here is how this all works: 
# 
# - During installation (make install): 
#   * Paths in ciao-mode-init.el.skel are fixed and ciao-mode-init.el (which contains
#     the suggestions at to what users should put in their .emacs files)
#     generated and installed in the libraries.
#   * ciao.el.header, a commented version of ciao-mode-init.el, 
#     ciao.el.body, and a commented version of CiaoMode.pl (which contains
#     version info)  are concatenated to produce ciao.el, which is
#     installed in the libraries. A link to it called prolog.el is made.
#     The current Ciao version is also included in ciao.el at this time.
#   * word-help.el is also installed in the libraries.
#   * All .el files in the libraries are byte-compiled.
#    
# - Version control: is now done automatically on CiaoMode.pl upon saving 
#   ciao.el.body.
# 
# - Generating the documentation (make doc): 
#   * CiaoMode.lpdoc is generated from ciao.el.body using emacs
#     (contains the documentation for all the functions in
#     ciao.el.body). CiaoMode.pl is included as a chapter in the Ciao
#     manual.
# 
# - Generating a standalone manual for ciao.el (make dvi): 
#   * Also, CiaoMode.dvi is generated in the doc directory using lpdoc
#     from CiaoMode.pl and CiaoMode.lpdoc. This is for tests only. 
#
#-----------------------------------------------------------------------------
include ../SETTINGS
include ../SHARED
SUBSTITUTE_STRING=../etc/substitute_string
#include /home/clip/Systems/ciao/SETTINGS
#include /home/clip/Systems/ciao/SHARED
#SUBSTITUTE_STRING = /home/clip/Systems/ciao/etc/substitute_string
#-----------------------------------------------------------------------------
ICONS = cancel2.png cb_arrow.png go.png \
        checkassertions.xpm ciao.xpm ciaoanalysis.xpm ciaoasr.xpm \
        ciaoclearquery.xpm ciaocustomize.xpm ciaodebug.xpm ciaoexeout.xpm \
	ciaoitf.xpm ciaoload.xpm ciaopeval.xpm ciaopl.xpm ciaopo.xpm \
	ciaopreproc.xpm ciaopreprocask.xpm ciaopreprocsee.xpm \
	ciaoprompt.xpm ciaorehighlight.xpm ciaoscrt.xpm ciaostorequery.xpm \
	clear.xpm clip.xpm close.xpm complete.xpm copy.xpm cut.xpm \
	find_error.xpm jump_to.xpm lpdoc.xpm lpdocview.xpm  manuals.xpm \
	open.xpm paste.xpm preferences.xpm print.xpm  save.xpm saveas.xpm \
	search.xpm stop.xpm undo.xpm wordhelp.xpm
PICONS = cancel2.png  cb_arrow.png  go.png
ICONDIR = $(LIBDIR)/$(VERSIONMAIN)/icons
BUILDICONDIR = $(RPM_BUILD_ROOT)$(ICONDIR)
#-----------------------------------------------------------------------------

ifeq ($(INSTYPE),src)
CIAOLIBDIR=$(CIAOSRC)/emacs-mode
CIAOREALLIBDIR=$(CIAOSRC)
LPDOCLIBDIR=$(CIAODESRC)/lpdoc/lib
else
CIAOLIBDIR=$(LIBDIR)
CIAOREALLIBDIR=$(REALLIBDIR)
LPDOCLIBDIR=$(LIBROOT)/lpdoc
endif

compile:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Compiling emacs library files"
	@echo "*** ---------------------------------------------------------"
# Generate ciao-mode-init
	$(SUBSTITUTE_STRING) "<CIAOLIBDIR>"     $(CIAOLIBDIR) \
	     ciao-mode-init.el.skel ciao-mode-init.el
	-chmod $(DATAMODE) ciao-mode-init.el
	-chgrp $(INSTALLGROUP) ciao-mode-init.el
# Generate commented ciao-mode-init, CiaoMode
	$(SUBSTITUTE_STRING) '^' ';;' ciao-mode-init.el ciao-mode-init.tmp
	$(SUBSTITUTE_STRING) '^' ';;' CiaoMode.pl CiaoMode.pl.tmp
	-chmod $(DATAMODE) ciao-mode-init.tmp
	-chgrp $(INSTALLGROUP) ciao-mode-init.tmp
	-chmod $(DATAMODE) CiaoMode.pl.tmp
	-chgrp $(INSTALLGROUP) CiaoMode.pl.tmp
# Generate ciao.el by collating the body and the commented versions
	$(SUBSTITUTE_STRING) \
	     'ciao-mode-version "Development Version"' \
	            'ciao-mode-version "$(VERSION)p$(PATCH)"' \
	    "<CIAOREALLIBDIR>" $(CIAOREALLIBDIR) \
	    "<LPDOCDIR>"       $(DOCDIR) \
	    "<LPDOCLIBDIR>"    $(LPDOCLIBDIR) \
	     ciao.el.body ciao.el.body.tmp
	cat ciao.el.header ciao-mode-init.tmp CiaoMode.pl.tmp \
	    ciao.el.body.tmp > ciao.el.tmp
	-chmod $(DATAMODE) ciao.el.tmp
	-chgrp $(INSTALLGROUP) ciao.el.tmp
	cp ciao.el.tmp ciao.el
	-chmod $(DATAMODE) ciao.el
	-chgrp $(INSTALLGROUP) ciao.el
	emacs -batch -l word-help.el -l ciao.el -f compile-ciao-mode
	-chmod $(DATAMODE) ciao.elc 
	-chgrp $(INSTALLGROUP) ciao.elc 
	-chmod $(DATAMODE) word-help.elc 
	-chgrp $(INSTALLGROUP) word-help.elc
	rm -rf *.tmp

install: install_msg install_common install_xemacs_$(VERIFY_XEMACS) \
	install_emacs_$(VERIFY_EMACS) install_links

install_msg:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing emacs library files in $(REALLIBDIR)..."
	@echo "*** ---------------------------------------------------------"

install_common:
# Make directories
	-mkdir -p $(BUILDREALLIBDIR) $(BUILDICONDIR) $(BUILDCIAOMODEINITDIR) \
		$(BUILDXEMACSINITDIR)
	-chmod $(EXECMODE) $(BUILDREALLIBDIR) $(BUILDICONDIR)
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR) $(BUILDICONDIR)
# Copy icons
	-cd $(BUILDICONDIR) && 'rm' -f $(ICONS)
	cd icons && cp $(ICONS) $(BUILDICONDIR)
	-cd $(BUILDICONDIR) && chmod $(DATAMODE) $(ICONS) && \
            chgrp $(INSTALLGROUP) $(ICONS) && 'rm' -f *~
# Copy png icons
	-cd $(BUILDREALLIBDIR) && 'rm' -f $(PICONS)
	cd icons && cp $(PICONS) $(BUILDREALLIBDIR) 
	-cd $(BUILDREALLIBDIR)  && chmod $(DATAMODE) $(PICONS) && \
            chgrp $(INSTALLGROUP) $(PICONS) && 'rm' -f *~
# Copy ciao.el, set mode and owner
	-cp ciao.el $(BUILDREALLIBDIR)/ciao.el
	-chmod $(DATAMODE) $(BUILDREALLIBDIR)/ciao.el
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR)/ciao.el
# Copy ciao.elc, set mode and owner
	-cp ciao.elc $(BUILDREALLIBDIR)/ciao.elc
	-chmod $(DATAMODE) $(BUILDREALLIBDIR)/ciao.elc
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR)/ciao.elc
# Copy word-help.el, set mode and owner
	-cp word-help.el $(BUILDREALLIBDIR)/word-help.el
	-chmod $(DATAMODE) $(BUILDREALLIBDIR)/word-help.el
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR)/word-help.el
# Copy word-help.elc, set mode and owner
	-cp word-help.elc $(BUILDREALLIBDIR)/word-help.elc
	-chmod $(DATAMODE) $(BUILDREALLIBDIR)/word-help.elc 
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR)/word-help.elc 

install_links:
# Set up links for all of the above
	-cd $(BUILDLIBDIR) && \
	'rm' -f ciao.el       && ln -s $(REALLIBDIR)/ciao.el             && \
	'rm' -f ciao.elc      && ln -s $(REALLIBDIR)/ciao.elc            && \
	'rm' -f word-help.el  && ln -s $(REALLIBDIR)/word-help.el        && \
	'rm' -f word-help.elc && ln -s $(REALLIBDIR)/word-help.elc       && \
	'rm' -f prolog.el     && ln -s $(REALLIBDIR)/ciao.el prolog.el   && \
	'rm' -f prolog.elc    && ln -s $(REALLIBDIR)/ciao.elc prolog.elc

install_emacs_yes:
# Copy ciao-mode-init.el, set mode and owner
	-cp ciao-mode-init.el $(BUILDCIAOMODEINITDIR)/ciao-mode-init.el
	-chmod $(DATAMODE) $(BUILDCIAOMODEINITDIR)/ciao-mode-init.el
	-chgrp $(INSTALLGROUP) $(BUILDCIAOMODEINITDIR)/ciao-mode-init.el
	-cd $(BUILDLIBDIR) && \
	'rm' -f ciao-mode-init.el && \
		ln -s $(CIAOMODEINITDIR)/ciao-mode-init.el

install_emacs_: install_emacs_yes
install_emacs_no:

install_xemacs_yes:
# Copy ciao-mode-init.el, set mode and owner
	-cp ciao-mode-init.el $(BUILDXEMACSINITDIR)/ciao-mode-init.el
	-chmod $(DATAMODE) $(BUILDXEMACSINITDIR)/ciao-mode-init.el
	-chgrp $(INSTALLGROUP) $(BUILDXEMACSINITDIR)/ciao-mode-init.el
	-cd $(BUILDLIBDIR) && \
	'rm' -f ciao-mode-init.el && \
		ln -s $(XEMACSINITDIR)/ciao-mode-init.el

install_xemacs_: install_xemacs_yes
install_xemacs_no:

.PHONY: doc dvi info

docs CiaoMode.pl CiaoMode.lpdoc: ciao.el.body 
	emacs -batch -l ./word-help.el -l ./ciao.el.body \
	      -f ciao-mode-documentation
	-chmod $(DATAMODE) CiaoMode.lpdoc
	-chgrp $(INSTALLGROUP) CiaoMode.lpdoc
	touch CiaoMode.pl

dvi: CiaoMode.lpdoc
	cd doc &&  $(MAKE) dvi

view: dvi
	cd doc &&  $(MAKE) view

info: CiaoMode.lpdoc
	cd doc &&  $(MAKE) info

infoview: info
	cd doc &&  $(MAKE) infoview

# ----------------------------------------------------------------------------

uninstall: uninstall_$(INSTYPE)

uninstall_src:
	@echo: "NOTE: Skipping removal of emacs files ..."

uninstall_ins:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Uninstalling emacs library files from $(REALLIBDIR) ..."
	@echo "*** ---------------------------------------------------------"
	-( cd $(BUILDICONDIR) && 'rm' -f $(ICONS) )
	-( cd $(BUILDREALLIBDIR) && 'rm' -f $(PICONS) )
	'rm' -f $(BUILDLIBDIR)/ciao-mode-init.el $(BUILDLIBDIR)/ciao.el \
		$(BUILDLIBDIR)/ciao.elc 
	'rm' -f $(BUILDLIBDIR)/word-help.el $(BUILDLIBDIR)/word-help.elc 
	'rm' -f $(BUILDLIBDIR)/prolog.el $(BUILDLIBDIR)/prolog.elc 
	'rm' -f $(BUILDREALLIBDIR)/ciao-mode-init.el \
		$(BUILDREALLIBDIR)/ciao.el \
	        $(BUILDREALLIBDIR)/ciao.elc 
	'rm' -f $(BUILDREALLIBDIR)/word-help.el \
		$(BUILDREALLIBDIR)/word-help.elc
	-rmdir $(BUILDICONDIR)
#-----------------------------------------------------------------------------
clean:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up $(BASEMAIN) emacs directory..."
	@echo "*** ---------------------------------------------------------"
	-rm -f ciao-mode-init.tmp CiaoMode.pl.tmp ciao.el.body.tmp ciao.el.tmp
	'rm' -f *~

realclean distclean: clean
	'rm' -f CiaoMode.lpdoc
	'rm' -f *.itf *.po *.asr
	'rm' -f *.elc
# END
#-----------------------------------------------------------------------------

