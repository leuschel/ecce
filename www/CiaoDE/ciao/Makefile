# Copyright (C) 1997, UPM-CLIP

# This Makefile *needs* GNU make

# make menuconfig       Show the menu for configure the system.  To avoid
#                       problems better use the ./configure script.
#
# make all              compile the whole Ciao system (engine, libraries, ...)
#                       (but does not regenerate docs: see make docs below)
# make install          install the whole Ciao system (must have been compiled
#                       before)
#
# make fullinstall      compile, generate docs and install the Ciao system
#
# make fullinstalleng	Compile and Install (or only
# make engine           compile) the Ciao engine for this particular arch.
#			This is the only make action needed for using Ciao
#                       executables in several architectures at once.
# 
# make cleanbackups     delete backup files
# make distclean        delete all files which can be automatically generated
# make engclean		delete all engines created
# make totalclean       cleanbackups + distclean
# 
# make docs             regenerate all manuals from the sources. 
#                       NOTE: This can only be done if lpdoc has been 
#                       installed previously.   
#                       It does need not be done during a normal install, 
#                       since the distribution comes with up to date 
#                       documentation.
# 
# make installdoc       a subset of 'make install', which only installs the 
#                       documentation. Useful after 'make docs'. 


#------- You should not change ANYTHING in this file -------------
#------- All customizable options are in the file SETTINGS -------

# Compiling (the engine) in Windows under cygwin:
# You can compile directly on a Windows machine, because CygWin
# allows mounting samba shares as Unix directories: if it is not done,
# just do:
# 
# $ mkdir --parents /home/clip          # Create a path to mount on
# $ mount -f -s //clip/clip /home/clip  # Mount the Samba share
# $ cd /home/clip/Systems/ciao-x.y
# $ make eng
# 
# ... etc.  This makes compiling, developing and testing much easier.
#
#-----------------------------------------------------------------
# Important note: you whould preffer the scheme:
#
# $(LOCALENGINE) [prolog_args] -C -b bootfile
#
# instead of this:
#
# $(SETLOCALCIAO) bootfile [prolog_args]
#
# The reason is that the first one is more portable.  The other
# will not work well in Win32 unless you add the .cpx extension to 
# the bootfile.
#

include SETTINGS
include SHARED


MFLAGS=-j$(PROCESSORS)

default: all$(ALT)

all:
	@echo "*** ======================================================================"
	@echo "*** Compiling Ciao"
	@echo "*** ======================================================================"
	$(MAKE) init_library engine compiler applications libraries environment
	@echo "*** ======================================================================"
	@echo "*** Ciao compilation completed"
	@echo "*** ======================================================================"

# Used for debug:
allnolibraries:
	$(MAKE) init_library engine compiler applications environment

crossengwin32: 
	$(MAKE) bin/Win32i86$(CIAODEBUG) CIAOARCH=Win32i86
	$(MAKE) include/Win32i86 CIAOARCH=Win32i86
	$(MAKE) commoneng CIAOARCH=Win32i86
	(umask 002; cd $(CIAOSRC)/bin/Win32i86$(CIAODEBUG) && \
	$(MAKE) configure && \
	$(MAKE) crossconfigure.h CIAORCH=crossWin32i86 && \
	$(MAKE) $(MFLAGS) ciaoemulator CIAOARCH=crossWin32i86 \
	ADDOBJ='$(STATOBJ)' \
	CURRLIBS='$(LIBS) $(STAT_LIBS)')

crossengine:
	/usr/bin/scp $(CIAOSRC)/etc/ciao_get_arch $(CROSS_COMPILER_HOST):/tmp && \
	$(MAKE) CIAOARCH=$(shell /usr/bin/ssh $(CROSS_COMPILER_HOST) /tmp/ciao_get_arch) DEFAULTYPE=crossdyn doengine

preparecrosslib:
	/bin/rm -rf ../clean-ciao-libs && \
	/bin/mkdir ../clean-ciao-libs && \
	rsync -a --exclude='.svn*' --exclude='*.asr' lib ../clean-ciao-libs 
# 	&& \
# 	rsync -a --exclude='.svn*' --exclude='*.asr' library ../clean-ciao-libs

# allwin32: init_library engwin32 compiler applications libraries

allpl: compiler applications libraries

engine: doengine$(ALT) stateng englinks

# These links are needed to run ciao from the sources
# Note: The links are a bit trickly because this should work with the ciao header.
englinks:
	cd $(CIAOSRC)/bin && \
            rm -f $(ENGINEBASE).$(CIAOARCH)$(EXECSUFFIX) && \
	    ln -s $(CIAOARCH)$(CIAODEBUG)/$(ENGINEBASE) \
		  $(ENGINEBASE).$(CIAOARCH)$(EXECSUFFIX) && \
            rm -f $(ENGINEBASE) && \
	    ln -s $(ENGINEBASE).$(CIAOARCH)$(EXECSUFFIX) $(ENGINEBASE)

dowindowsbat:
	$(SETLOCALCIAO) $(CIAOC) -x Win32/setup_bat
	cd $(CIAODESRC) && $(LOCALENGINE) \"$(OBJDIR)/$(ENGINENAME)\" -C -b $(CIAOSRC)/Win32/setup_bat

doengine: bin/$(CIAOARCH)$(CIAODEBUG) include/$(CIAOARCH)$(CIAODEBUG) \
	$(DEFAULTYPE)eng exe_header # dowindowsbat

doenginewin32: doengine

# doenginewin32: copysrcfiles doengine dowindowsbat
# 	rm -f $(CIAOSRC)/Win32/bin/$(ENGINENAME)
# 	cp $(OBJDIR)/$(ENGINENAME) $(CIAOSRC)/Win32/bin

crossdyneng: commoneng 
	( \
		umask 002 ; cd $(OBJDIR) &&  \
	 	$(MAKE) crossconfigure.h && \
	 	$(MAKE) $(MFLAGS) $(ENGINENAME) LCC=$(LCC) CURRLIBS='$(LIBS)' \
	)

dyneng: commoneng stateng # libciao
	(umask 002; cd $(OBJDIR) &&  \
	 $(MAKE) configure.h && \
	 $(MAKE) $(MFLAGS) $(ENGINENAME) CURRLIBS='$(LIBS)')

libciao:
	(cd $(OBJDIR) && \
	$(MAKE) libciao CURRLIBS='$(LIBS)' && \
	$(MAKE) ciaoobject CURRLIBS='$(LIBS)' )

stateng: commoneng
# In Windows we need an executable built without console support 
# ifeq ($(OSNAME),Win32)
# 	cd $(OBJDIR) &&	$(MAKE) clean
# 	$(MAKE) $(MFLAGS) CONSOLEFLAG='$(NOCONSOLEFLAG)' dostateng
# 	/bin/mv $(OBJDIR)/$(ENGINAME).sta $(OBJDIR)/$(ENGINAME)_nc.sta
# 	cd $(OBJDIR) &&	$(MAKE) clean
# endif
	$(MAKE) $(MFLAGS) dostateng


dostateng:
	(umask 002 && cd $(OBJDIR) && $(MAKE) configure.h && \
	 $(MAKE) $(MFLAGS) $(ENGINENAME) ADDOBJ='$(STATOBJ)' \
	CURRLIBS='$(LIBS) $(STAT_LIBS)')
	-cp -f $(OBJDIR)/$(ENGINENAME) $(OBJDIR)/$(ENGINENAMESTAT)
	@-chmod $(EXECMODE) $(OBJDIR)/$(ENGINENAMESTAT)
	@-chgrp $(INSTALLGROUP) $(OBJDIR)/$(ENGINENAMESTAT)

commoneng:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Compiling $(BASEMAIN) engine for $(OSNAME)$(ARCHNAME)..."
	@echo "*** ----------------------------------------------------------------------"
	$(MAKE) $(MFLAGS) version-ciao

allengs:
	@for machine in $(REMOTEMACHINES); do \
		echo ; \
		echo -------------------------------------------- ; \
		echo ; \
		echo Making engine in $$machine; \
		echo "unsetenv CIAODEBUG; cd $(CIAOSRC); $(MAKE) eng" | rsh $$machine csh; \
		echo "setenv CIAODEBUG -debug; cd $(CIAOSRC); $(MAKE) eng" | rsh $$machine csh; \
	done

installallengs:
	@for machine in $(REMOTEMACHINES); do \
		echo ; \
		echo -------------------------------------------- ; \
		echo ; \
		echo Installing engine in $$machine; \
		echo "unsetenv CIAODEBUG; cd $(CIAOSRC); $(MAKE) fullinstalleng" | rsh $$machine csh; \
	done

exe_header:
	cd lib/compiler && $(MAKE) exe_header

compiler:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Compiling $(BASEMAIN) standalone compiler"
	@echo "*** (this may take a while and is silent; please be patient)"
	@echo "*** ----------------------------------------------------------------------"
	cd ciaoc && $(MAKE) compiler
	$(MAKE) sysdep$(ALT)

sysdep:

sysdepwin32: dowindowsbat

lpmake:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Generating static lpmake to init Ciao configuration...   "
	@echo "*** ----------------------------------------------------------------------"
	mkdir -p $(SRCBINDIR)
	touch $(SRCBINDIR)/NODISTRIBUTE
	cd etc && $(SETLOCALCIAO) $(BOOTSTRAP_CIAOC) -s -x -o $(LPMAKE) lpmake
	chmod $(EXECMODE) $(LPMAKE)

applications:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Compiling $(BASEMAIN) toplevel shell & script interpreter ..."
	@echo "*** (this may take a while and is silent; please be patient)"
	@echo "*** ----------------------------------------------------------------------"
	@$(MAKE) appsshell
	@$(MAKE) appsetc

appsshell:
	@cd shell && $(MAKE) all

appsetc:
	@cd etc && $(LPMAKECMD) all

libraries:
	$(LPMAKECMD) alllib alllibrary$(DEFAULTYPE) allcontrib
#	cd lib && $(MAKE) all
#	cd library && $(MAKE) all$(DEFAULTYPE)
#	$(LPMAKECMD) alllibrary$(DEFAULTTYPE)
#	cd contrib && $(MAKE) all
#	$(LPMAKECMD) allcontrib

alllib:
	$(LPMAKECMD) alllib
alllibrary:
	$(LPMAKECMD) alllibrary$(DEFAULTYPE)
allcontrib:
	$(LPMAKECMD) allcontrib

copysrcfiles: createsrcdir
	cd engine && for File in *.[ch] *.pl Makefile ; \
	do if [ ! -f $(OBJDIR)/$${File} -o $${File} -nt $(OBJDIR)/$${File} ]; \
             then rm -f $(OBJDIR)/$${File} ; cp $${File} $(OBJDIR)/$${File} ; \
	   fi ; \
	done ;

include/$(CIAOARCH)$(CIAODEBUG):
	$(MAKE) createincludedir

bin/$(CIAOARCH)$(CIAODEBUG):
	$(MAKE) createsrcdir
	cd $(OBJDIR) &&	                   \
	   ln -s ../../engine/*.[ch] . &&   \
	   ln -s ../../engine/*.pl . &&   \
	   ln -s ../../engine/Makefile . && \
	   rm -f configure.h

createsrcdir:
	-if test ! -d $(CIAOSRC)/bin ; then \
	  mkdir $(CIAOSRC)/bin ; \
	  touch $(CIAOSRC)/bin/NODISTRIBUTE ; \
	  chmod $(EXECMODE) $(CIAOSRC)/bin ; \
	  chmod $(DATAMODE) $(CIAOSRC)/bin/NODISTRIBUTE ; \
        fi
	-if test ! -d $(OBJDIR) ; then \
	  mkdir $(OBJDIR) ; chmod $(EXECMODE) $(OBJDIR) ; fi

createincludedir:
	if test ! -d $(CIAOSRC)/include ; then \
	  mkdir $(CIAOSRC)/include ; \
	  chmod $(EXECMODE) $(CIAOSRC)/include ; \
	  touch $(CIAOSRC)/include/NODISTRIBUTE ; fi
	if test ! -d $(SRCINCLUDEDIR) ; then \
	  mkdir $(SRCINCLUDEDIR) ; chmod $(EXECMODE) $(SRCINCLUDEDIR) ; fi

version-ciao:
	-rm -f $(OBJDIR)/version.c
ifeq ($(DEBUG_LEVEL),nodebug)
	SHOW_DEBUG_LEVEL="" $(MAKE) version.c
else
	SHOW_DEBUG_LEVEL=" [$(DEBUG_LEVEL)]" $(MAKE) version.c
endif

version.c:
	@(umask 002; \
	echo 'char *emulator_version = "Ciao '$(VERSION)'.'$(PATCH)'-'$(REVISION)': '`date`''$(SHOW_DEBUG_LEVEL)'";' \
		> $(OBJDIR)/version.c; \
	echo 'char *emulator_architecture = "$(ARCHNAME)";' \
						       >> $(OBJDIR)/version.c;\
	echo 'char *emulator_os =    "$(OSNAME)";'     >> $(OBJDIR)/version.c;\
	echo 'char *emulator_debug = "$(CIAODEBUG)";'  >> $(OBJDIR)/version.c;\
	echo 'double ciao_version =  $(VERSION);'      >> $(OBJDIR)/version.c;\
	echo 'int ciao_patch =       $(PATCH);'        >> $(OBJDIR)/version.c;\
	echo 'char *ciao_suffix =    "$(CIAOSUFFIX)";' >> $(OBJDIR)/version.c;\
	echo 'char *exec_suffix =    "$(EXECSUFFIX)";' >> $(OBJDIR)/version.c;\
	echo 'char *installibdir =   "$(REALLIBDIR)";' >> $(OBJDIR)/version.c )

	-chmod $(DATAMODE) $(OBJDIR)/version.c
	-chgrp $(INSTALLGROUP) $(OBJDIR)/version.c

environment:
ifeq ($(INSTALL_EMACS_SUPPORT),yes)
	cd emacs-mode && $(MAKE) compile
else
	@echo "{NOTE: Emacs support will not be installed}"
endif

fullinstalleng: engine installincludes installeng

installeng:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Installing $(BASEMAIN) engine for $(OSNAME)$(ARCHNAME)..."
	@echo "*** ----------------------------------------------------------------------"
	-(umask 002; mkdir -p $(RPM_BUILD_ROOT)$(REALLIBDIR) ; \
	 cd $(OBJDIR) && $(MAKE) install LD=$(LD) \
	 CC=$(CC) CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)' LIBS=$(LIBS))

uninstalleng:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Uninstalling $(BASEMAIN) engine for $(OSNAME)$(ARCHNAME)..."
	@echo "*** ----------------------------------------------------------------------"
#       Recreate the compilation directory in case it has been removed
	$(MAKE) createsrcdir && \
	cd $(OBJDIR) &&	 \
	rm -f Makefile && \
	ln -s ../../engine/Makefile . && \
	$(MAKE) uninstall

installincludes:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Installing C include files for $(OSNAME)$(ARCHNAME)..."
	@echo "*** ----------------------------------------------------------------------"
	-mkdir -p $(BUILDREALLIBDIR)/include
	-chmod $(EXECMODE) $(BUILDREALLIBDIR)/include
	-mkdir -p $(RPM_BUILD_ROOT)$(INSTALLEDINCLUDEDIR)
	-chmod $(EXECMODE) $(RPM_BUILD_ROOT)$(INSTALLEDINCLUDEDIR)
	-cp $(NODEBUGSRCINCLUDEDIR)/* $(RPM_BUILD_ROOT)$(INSTALLEDINCLUDEDIR)
	-chmod $(DATAMODE) $(RPM_BUILD_ROOT)$(INSTALLEDINCLUDEDIR)/*
	-mkdir -p $(RPM_BUILD_ROOT)$(INCLUDEROOT)
	-rm -f $(RPM_BUILD_ROOT)$(INCLUDEROOT)/ciao_prolog.h
	-ln -s $(INSTALLEDINCLUDEDIR)/ciao_prolog.h $(RPM_BUILD_ROOT)$(INCLUDEROOT)

# uninstallincludes:
# 	@echo "*** ----------------------------------------------------------------------"
# 	@echo "*** Uninstalling C include files for $(OSNAME)$(ARCHNAME)..."
# 	@echo "*** ----------------------------------------------------------------------"
# 	-rm -rf $(RPM_BUILD_ROOT)$(INSTALLEDINCLUDEDIR)
# 	-rm  -f $(RPM_BUILD_ROOT)$(INCLUDEROOT)/ciao_prolog.h

# This option requires that lpdoc-2.0 are already installed

fullinstall:
	$(MAKE) all
	$(MAKE) docs
	$(MAKE) install

# install:
# 	@$(MAKE) all
# 	@$(MAKE) justinstall

installsrc: reconfiguresrc

init_library: force_auto_compile_options config_source_components

config_source_components: build_config_source_components run_config_source_components

build_config_source_components:
	@mkdir -p $(SRCBINDIR)
	@$(SETLOCALCIAO) $(BOOTSTRAP_CIAOC) -x -o $(CONFIG_SOURCE_COMPONENTS) \
		$(CIAODESRC)/installer/config_source_components

run_config_source_components:
	@$(SETLOCALCIAO) $(CONFIG_SOURCE_COMPONENTS) $(CIAODESRC)

force_auto_compile_options:
	@rm -f lib/auto_compile_options.pl
	@$(MAKE) auto_compile_options >> $(INSTALL_LOG)

auto_compile_options:
	cd lib && $(CIAOSRC)/etc/generate_foreign_compile_options $(MAKE) \
		$(CIAOSRC)/makefile-sysdep $(CIAOSRC)/lib/auto_compile_options.pl
	-chmod $(DATAMODE) $(CIAOSRC)/lib/auto_compile_options.pl

prepare: engcleanlocal enginelocal init_library lpmakelocal
	@echo "*** ======================================================================"
	@echo "*** Ciao preparation done"
	@echo "*** ======================================================================"

lpmakelocal:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Creating static lpmake for automatic configuration"
	@echo "*** ----------------------------------------------------------------------"
	@$(MAKE) lpmake >> $(INSTALL_LOG)

enginelocal:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Creating local ciao engine for automatic configuration"
	@echo "*** ----------------------------------------------------------------------"
	@$(MAKE) engine >> $(INSTALL_LOG)

justmenuconfig: config_source_components
	@unset USE_THREADS USE_POSIX_LOCKS OPTIM_LEVEL COMPRESS_LIB EXECMODE \
	DATAMODE DEBUG_LEVEL INSTALLGROUP; \
	$(LPMAKECMD) menuconfig
	@$(MAKE) init_library >> $(INSTALL_LOG)

menuconfig: prepare justmenuconfig engcleanlocal

autoconfig:
	LPMAKEOPTS="-d CONFIGLEVEL=1" $(MAKE) justmenuconfig

reconfigure:
	$(LPMAKECMD) reconfigure

reconfiguresrc:
	$(LPMAKECMD) reconfiguresrc

unreconfigure:
	$(LPMAKECMD) unreconfigure

install:
	@echo "*** ======================================================================"
	@echo "*** Installing Ciao"
	@echo "*** ======================================================================"
	$(MAKE) justinstall
	$(LPMAKECMD) reconfigure
	@echo "*** ======================================================================"
	@echo "*** Ciao installation completed"
	@echo "*** ======================================================================"

justinstalllib:
	cd lib && $(LPMAKECMD) install

justinstall:
#	-mkdir -p $(BUILDREALLIBDIR)
#	-chmod $(EXECMODE) $(BUILDLIBDIR)
#	-chmod $(EXECMODE) $(BUILDREALLIBDIR)
	$(LPMAKECMD) justinstall
#	find $(BUILDREALLIBDIR) -type d -exec chmod $(EXECMODE) {} \;

.PHONY: docs installdoc

docs:
#	cd emacs-mode && $(MAKE) docs
	$(LPMAKECMD) docs

docsemacs:
	$(LPMAKECMD) docsemacs

installdoc:
	$(LPMAKECMD) installdoc

uninstalletc:
	cd etc && $(LPMAKECMD) uninstall

uninstall:
	@echo "*** ======================================================================"
	@echo "*** Uninstalling Ciao"
	@echo "*** ======================================================================"
	$(LPMAKECMD) uninstall
	$(MAKE) uninstalleng
#	-rm -rf $(BUILDREALLIBDIR)
#	-rm -r $(LIBDIR)
	@echo "*** ======================================================================"
	@echo "*** Ciao uninstallation completed"
	@echo "*** ======================================================================"

uninstalldoc:
	$(LPMAKECMD) uninstalldoc
#	cd doc && $(MAKE) uninstall

test:
	cd examples/misc && $(MAKE) distclean suite && $(MAKE) exec_suite

ciaoconfig_clean:
	rm -rf SETTINGS SETTINGS_AUTO lib/autoconfig/*_auto.pl
	cat SETTINGS_DEFAULT           > SETTINGS
	cd .. && echo "CIAOSRC=`pwd`" >> SETTINGS
#	echo "ABSSRC=`pwd`"      >> SETTINGS

clean: engclean
#	cd ciaoc &&          $(MAKE) clean
	cd lib &&            $(MAKE) clean
	cd shell &&          $(MAKE) clean
#	cd emacs &&          $(MAKE) clean
	cd emacs-mode &&     $(MAKE) clean
	cd examples &&          $(MAKE) clean
	$(MAKE) cleangmon

realclean:
	cd ciaoc && $(MAKE) realclean
	cd lib && $(MAKE) realclean
	cd shell && $(MAKE) realclean
#	cd emacs && $(MAKE) realclean
	cd emacs-mode && $(MAKE) realclean
	cd examples && $(MAKE) realclean
	$(MAKE) init_library
	$(LPMAKECMD) realcleandoc
#	cd doc && $(MAKE) realclean
	$(MAKE) ciaoconfig_clean
	$(MAKE) engrealclean
	$(MAKE) recursiveclean

tar:
	(cd .. ; tar cf - $(notdir $(CIAOSRC)) | gzip -c > ciao-$(VERSION).$(PATCH).tar.gz)



totalclean: cleanbackups distclean

engrealclean engclean:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Removing $(BASEMAIN) engine for all architectures..."
	@echo "*** ----------------------------------------------------------------------"
	-rm -rf $(CIAOSRC)/bin
	-rm -rf $(CIAOSRC)/include

engcleanlocal:
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Removing $(BASEMAIN) engine for architecture $(CIAOARCH)..."
	@echo "*** ----------------------------------------------------------------------"
	-rm -rf $(OBJDIR)
	-rm -rf $(SRCINCLUDEDIR)

cleanbackups:
	(cd $(CIAOSRC) && find . -name '*~' -exec /bin/rm -f {} \;)
	(cd $(CIAOSRC) && find $(CIAOSRC) -name '#*' -exec /bin/rm -f {} \;)

cleanmpp:
	(cd $(CIAOSRC) && find . -name '*.mpp' -exec /bin/rm -f {} \;)

cleangmon:
	(cd $(CIAOSRC) && find . -name 'gmon.out' -exec /bin/rm -f {} \;)

cleanasrs:
	(cd $(CIAOSRC) && find . -name '*.asr' -exec /bin/rm -f {} \;)

cleanasts:
	(cd $(CIAOSRC) && find . -name '*.ast' -exec /bin/rm -f {} \;)

clean_co:
	(cd $(CIAOSRC) && find . -name '*_co.pl' -exec /bin/rm -f {} \;)

clean_opt:
	(cd $(CIAOSRC) && find . -name '*_opt.pl' -exec /bin/rm -f {} \;)

clean_glue:
	(cd $(CIAOSRC) && find . -name '*_glue.c' -exec /bin/rm -f {} \;)

clean_inline:
	(cd $(CIAOSRC) && find . -name '*_inline.c' -exec /bin/rm -f {} \;)

clean_nodistribute:
	(cd $(CIAOSRC) && find . -name 'NODISTRIBUTE' -exec etc/remove_dirname {} \;)

recursiveclean: cleangmon clean_co clean_glue clean_inline engclean cleanmpp clean_opt

# Note that to avoid bootstrap complications, the next will not delete
# the lpmake.sta file
old_distclean: init_library
	@echo "*** ----------------------------------------------------------------------"
	@echo "*** Cleaning $(CIAOSRC) distribution tree... (unix)"
	@echo "*** ----------------------------------------------------------------------"
	cd ciaoc && $(MAKE) distclean
	cd shell && $(MAKE) distclean
	cd etc && $(LPMAKECMD) distclean
	cd emacs-mode && $(MAKE) distclean
	cd Win32 && $(MAKE) distclean
#	cd tests && $(MAKE) distclean
	cd examples && $(MAKE) distclean
#	cd doc && $(LPMAKECMD) distclean
	$(CIAOSRC)/etc/recursive_make_or_clean $(CIAOSRC)/doc     "$(LPMAKEREADME)" \
		"$(LPMAKEINSTALL)" $(MAKE) distclean
	$(CIAOSRC)/etc/recursive_make_or_clean $(CIAOSRC)/library "$(LPMAKEREADME)" \
		"$(LPMAKEINSTALL)" $(MAKE) distclean
	$(CIAOSRC)/etc/recursive_make_or_clean $(CIAOSRC)/contrib "$(LPMAKEREADME)" \
		"$(LPMAKEINSTALL)" $(MAKE) distclean
	$(CIAOSRC)/etc/recursive_make_or_clean $(CIAOSRC)/lib     "$(LPMAKEREADME)" \
		"$(LPMAKEINSTALL)" $(MAKE) distclean
	@rm -f TAGS *.po *.itf *.asr
	$(MAKE) recursiveclean
	$(MAKE) ciaoconfig_clean

cflow:
	cd $(OBJDIR) && cflow -i -D$(CIAOARCH) *.c > $(CIAOSRC)/etc/cflow.out

cxref:
	cd $(OBJDIR) && cxref -xref-function -D$(ARCHNAME) -D$(OSNAME) \
		$(THREAD_FLAG) $(FOREIGN_FILES_FLAG) *.[ch] -O$(CIAOSRC)/etc/cxref

mysqllibs:
	$(LPMAKECMD) mysqllibs

