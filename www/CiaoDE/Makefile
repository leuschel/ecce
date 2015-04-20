# -*- mode: Makefile; -*-
#
# This Makefile *needs* GNU make
# ----------------------------------------------------------------------------
# Ciao Development Environment
# Initial Makefile to compile and install everything from scratch
# ----------------------------------------------------------------------------
# You should not need to change anything in here.
# 
# To see a description of each option see the 'help' target.
# 
# Important: installation modifies the .bashrc/.cshrc files. In order
# for these changes to be visible you need to log out and back in.
# 
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Steps to make and publish a distribution:
# ./configure
# make all
# make docs
# cd dist
# lpmake install
# ----------------------------------------------------------------------------

CIAODESRC=$(PWD)
SRC=$(CIAODESRC)

-include ciao/SETTINGS
-include ciao/SHARED

# default all: allciao allextra

help:
	@echo "Performs actual CiaoDE compilation and installation"
	@echo
	@echo "Usage: $(MAKE) [Option]."
	@echo
	@echo "Where option is one of the following:"
	@echo
	@echo "  all          Compile the whole CiaoDE system (engine, libraries, ...)"
	@echo "               (but does not regenerate docs: see $(MAKE) docs below)."
	@echo
	@echo "  install      Install the whole CiaoDE system (must have been"
	@echo "               compiled before)"
	@echo
	@echo "  uninstall    Uninstall CiaoDE."
	@echo
	@echo "  clean        Clean CiaoDE auto generated files, excluding documentation"
	@echo "               and additional files such as logs, temporary files, etc..."
	@echo
	@echo "  distclean    Clean CiaoDE auto generated files, excluding documentation."
	@echo
	@echo "  realclean    Clean CiaoDE auto generated files, including documentation."
	@echo
	@echo "  bootclean    Remove binary files."
	@echo
	@echo "  docs         (Re)generate all documentation using LPdoc."
	@echo "               Documentation is normally already in the distribution,"
	@echo "               but this may be needed when checking out of repository"
	@echo "               or after changes in the sources."
	@echo
	@echo "Note: currently, GNU Make is required to compile CiaoDE"
	@echo

# ----------------------------------------------------------------------------
# Most of the following options are wrappers to lpmake options.
# ----------------------------------------------------------------------------

alldocs: all docs

default all:
	@echo "*** ======================================================================"
	@echo "*** Compiling CiaoDE"
	@echo "*** ======================================================================"
	$(MAKE) allciao allextra
	@echo "*** ======================================================================"
	@echo "*** CiaoDE compilation completed"
	@echo "*** ======================================================================"

allciao:
	@cd ciao && $(MAKE) all

engine:
	@cd ciao && $(MAKE) engine

crossengine:
	@cd ciao && $(MAKE) crossengine

allextra:
	@$(LPMAKECMD) allextra

docs:
	@echo "*** ======================================================================"
	@echo "*** Building CiaoDE documentation"
	@echo "*** ======================================================================"
	@$(LPMAKECMD) docs
	@echo "*** ======================================================================"
	@echo "*** Finished building CiaoDE documentation"
	@echo "*** ======================================================================"

install:
	@echo "*** ======================================================================"
	@echo "*** Installing CiaoDE"
	@echo "*** ======================================================================"
	@$(LPMAKECMD) install
	@echo "*** ======================================================================"
	@echo "*** CiaoDE installation completed"
	@echo "*** ======================================================================"

justinstall:
	@$(LPMAKECMD) justinstall

fullinstall: all docs install

alllpdoc:
	@cd lpdoc && $(LPMAKECMD) all

docsciaode:
	@$(LPMAKECMD) docsciaode

reconfigure:
	@$(LPMAKECMD) reconfigure

uninstall:
	@$(LPMAKECMD) uninstall

uninstallciao:
	@$(LPMAKECMD) uninstallciao

menuconfig:
	@cd ciao && $(LPMAKECMD) menuconfig

justmenuconfig:
	@cd ciao && $(MAKE) justmenuconfig

clean:
	@$(LPMAKECMD) clean

rawclean:
	@$(LPMAKECMD) rawclean

distclean:
	@$(LPMAKECMD) distclean

realclean:
	@$(LPMAKECMD) realclean

bootclean:
	@rm -rf bin
#	$(LPMAKECMD) bootclean

braveclean: bootclean
	cd $(CIAODESRC) && find . -name '*.po' -exec /bin/rm -f {} \;
	cd $(CIAODESRC) && find . -name '*.itf' -exec /bin/rm -f {} \;
	cd $(CIAODESRC) && find . -name '*.asr' -exec /bin/rm -f {} \;
	cd $(CIAODESRC) && find . -name '*.ast' -exec /bin/rm -f {} \;
	cd $(CIAODESRC) && find . -name '*_co.pl' -exec /bin/rm -f {} \;
	cd $(CIAODESRC) && find . -name '*_opt.pl' -exec /bin/rm -f {} \;
	cd $(CIAODESRC) && find . -name '*_glue.pl' -exec /bin/rm -f {} \;
	cd $(CIAODESRC) && find . -name '*_inline.pl' -exec /bin/rm -f {} \;

installer_Win32:
	$(LPMAKECMD) installer_Win32

# ----------------------------------------------------------------------------
# End.
# ----------------------------------------------------------------------------

