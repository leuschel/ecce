# CIAOD Makefile

MODE=775
PROLOG=ciaod-engine
#CODE=fastcode #Watch out: SICStus0.7 of course does not understand this
CODE=compactcode
COMPOPT=prolog_flag(compiling,_,$(CODE)),
CIAOVERSION=0.8
PROLOGLIB=/usr/local/src/ciaod-engine/library

BINDIR=/usr/local/bin
# BINDIR=/home/bardo/CIAOd-0.8.2/bin

# done by hand only because of the /mounted disparity
# CIAODIR=`pwd`
CIAODIR=/home/bardo/CIAOd-0.8.2
CIAOBB=$(BINDIR)/ciaod_bb
XTERM='/usr/bin/X11/xterm -e '

SHELL=/bin/sh 

ciaod_system: ciaod ciaod_bb

ciaod: ciaod.pl linda.pl ciao_c_lib.pl ciao_tcltk.pl ciaod_user.pl ciaod_expansion.pl
	echo "$(COMPOPT) \
              retractall(library_directory(_)),\
              assert(library_directory('$(PROLOGLIB)')),\
              compile(ciaod),\
              ciaod:assert(ciao_version('$(CIAOVERSION)')),\
              ciaod:assert(ciao_version_date('"`date`"')),\
              ciaod:assert(ciao_blackboard('$(CIAOBB)')),\
              ciaod:assert(xterm_executable($(XTERM))),\
              compile(ciao_c_lib),\
              compile(ciao_tcltk),\
              compile(ciaod_user),\
              compile(ciaod_expansion),\
              global_silent(on),\
              psave(ciaod,1),\
              ciaod:top." | $(PROLOG)
	-@chmod $(MODE) ciaod

ciaod_bb: ciaod_bb.pl
	echo "$(COMPOPT) \
              retractall(library_directory(_)),\
              assert(user:library_directory('$(PROLOGLIB)')),\
	      compile(ciaod_bb),\
	      gen_system(ciaod_bb)." | $(PROLOG)
	-@chmod $(MODE) ciaod_bb

install: ciaod ciaod_bb.pl
	\rm -f $(BINDIR)/ciaod ; ln -s $(CIAODIR)/ciaod $(BINDIR)/ciaod 
	\rm -f $(BINDIR)/ciaod_bb ; ln -s $(CIAODIR)/ciaod_bb $(BINDIR)/ciaod_bb 

clean:
		\rm -f $(CIAODIR)/ciaod
		\rm -f $(CIAODIR)/ciaod_bb

touch:
		touch *.pl




