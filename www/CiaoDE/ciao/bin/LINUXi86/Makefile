# Copyright (C) 1996, UPM-CLIP

# Do NOT use tabs for the commands inside if/endif!
#
# ifneq (,$(findstring FOREIGN_FILES, $(CFLAGS)))
#     FOREIGN=foreign.o
# endif

SHELL=/bin/sh
OBJFILES=wam.o format.o inout.o misc.o startgoal.o ciao.o \
	 nondet.o objareas.o streams.o timing.o wambuiltin.o wamfunction.o \
	 unix_utils.o attr.o dynlink.o tasks.o locks.o prolog_tasks.o \
	 own_malloc_linear.o alloc.o heapgc.o indexing.o initial.o \
	registers.o stacks.o support.o term_support.o wamsupport.o \
	qread.o qget.o builtin.o interrupt.o gauge.o bignum.o \
	float_consts.o float_tostr.o \
	ciao_prolog.o start.o version.o

MAIN_FILE=main.o 
#MAIN_EXEC=main_exec.o version.o qinsert.o

DATADEFS=datadefs.h configure.h alloc.h registers.h termdefs.h access.h locks.h objareas.h
SUPPORT=support.h debug.h initial.h compat.h ciao_prolog.h ciao_gluecode.h threads.h task_areas.h timing_defs.h \
	predtyp.h profile_defs.h alloc_defs.h support_defs.h builtin_defs.h

CCOMPILATIONSUPPORT=

include ../../SETTINGS
include ../../SHARED

#include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

OBJSFORLIB=$(OBJFILES) $(ADDOBJ)
OBJ=$(OBJSFORLIB) $(MAIN_FILE)
CIAOSHLIB=lib$(ENGINENAME).so

#ifeq ($(OSNAME),Solaris)
#  LIBSTA=-lsocket
#else
#  LIBSTA=
#endif

install: # $(ENGINENAME)
	-mkdir -p $(RPM_BUILD_ROOT)$(ENGINEDIR)
	-chmod $(EXECMODE) $(RPM_BUILD_ROOT)$(ENGINEDIR)
	-rm -f $(RPM_BUILD_ROOT)$(INSTALLEDENGINEARCH)
	-rm -f $(RPM_BUILD_ROOT)$(INSTALLEDENGINEARCHSTAT)
	-rm -f $(RPM_BUILD_ROOT)$(INSTALLEDENGINESTAT)
	-rm -f $(RPM_BUILD_ROOT)$(INSTALLEDENGINE)
	-cp $(ENGINENAME) $(RPM_BUILD_ROOT)$(INSTALLEDENGINEARCH)
	-cp $(ENGINENAMESTAT) $(RPM_BUILD_ROOT)$(INSTALLEDENGINEARCHSTAT)
# MH fixed these permissions
	-chmod $(EXECMODE) $(RPM_BUILD_ROOT)$(INSTALLEDENGINEARCH)
	-chmod $(EXECMODE) $(RPM_BUILD_ROOT)$(INSTALLEDENGINEARCHSTAT)
	-ln -s $(INSTALLEDENGINEARCH) $(RPM_BUILD_ROOT)$(INSTALLEDENGINE)
	-ln -s $(INSTALLEDENGINEARCHSTAT) \
		$(RPM_BUILD_ROOT)$(INSTALLEDENGINESTAT)
# ifeq ($(OSNAME),Win32)
# 	-rm -f $(INSTALLEDENGINE)_nc.$(CIAOARCH)
# 	-rm -f $(INSTALLEDENGINE)_nc.$(CIAOARCH).sta
# 	-rm -f $(INSTALLEDENGINE)_nc
# 	-cp $(ENGINENAME)_nc $(INSTALLEDENGINE)_nc.$(CIAOARCH)
# 	-cp $(ENGINENAME)_nc.sta $(INSTALLEDENGINE)_nc.$(CIAOARCH).sta
# 	-ln -s $(INSTALLEDENGINE)_nc.$(CIAOARCH) $(INSTALLEDENGINE)_nc
# 	-ln -s $(INSTALLEDENGINE)_nc.$(CIAOARCH).sta $(INSTALLEDENGINE)_nc.sta
# # MH these permissions need to looked at
# 	-chmod $(EXECMODE)_nc $(ENGINEDIR)
# 	-chmod $(EXECMODE)_nc $(INSTALLEDENGINE)_nc
# endif
#	-cp $(DATADEFS) $(SUPPORT) $(INSTALLEDINCLUDEDIR)
#	-chmod $(DATAMODE) $(INSTALLEDINCLUDEDIR)/*

uninstall:
	-rm -f $(RPM_BUILD_ROOT)$(INSTALLEDENGINE)
	-rm -f $(RPM_BUILD_ROOT)$(INSTALLEDENGINEARCH)
	-rm -f $(RPM_BUILD_ROOT)$(INSTALLEDENGINESTAT)
	-rm -f $(RPM_BUILD_ROOT)$(INSTALLEDENGINEARCHSTAT)


## We produce three files: 
### the combined .o file (for static linking),
### the .so file (for dynamic linking), and
### the executable itself

$(ENGINENAME): configure.h ciaoemulator
	true

# fix_size: it does not depend on the architecture; it is executed locally,
# so it has to be compiled in the local (host) architecture
fix_size_exec: # $(CIAOSRC)/etc/fix_size
	cd $(CIAOSRC)/etc && $(LCC) -o fix_size fix_size.c


# The emulator is built from all the .o files

ciaoemulator: fix_size_exec $(OBJ) 
	$(CC) $(CONSOLEFLAG) $(LDFLAGS) $(OBJ) $(CURRLIBS) -o $(ENGINENAME)
	-chmod $(DATAMODE) *.o
	-chgrp $(INSTALLGROUP) *.o
	$(CIAOSRC)/etc/fix_size$(EXECSUFFIX) $(ENGINENAME)
	-chmod $(EXECMODE) $(ENGINENAME)
	-chgrp $(INSTALLGROUP) $(ENGINENAME)
	if test ! -f $(SRCINCLUDEDIR)/configure.h ; then \
	  for f in $(DATADEFS) $(SUPPORT) $(CCOMPILATIONSUPPORT); do \
	    ln -f -s $(OBJDIR)/$$f $(SRCINCLUDEDIR); \
	  done ; \
        fi


# The archive to be linked with other programs

libciao: $(OBJFILES)
#	$(LD) $(LDSHARED) $(OBJFILES) -o $(CIAOLIBNAME) $(CURRLIBS)
	ar -r libciao.a $(OBJFILES)
	-@chmod $(DATAMODE) libciao.a

# The big ciaoengine in object format

ciaoobject: $(OBJFILES)
	$(LD) $(LDCOMBINE) $(OBJFILES) -o $(ENGINENAME).o

clean:
	/bin/rm -f makecase configure \
	version.c configure.h *.o 
	/bin/rm -f metaengine metaengine.po metaengine.itf
#	kernel_con kernel.con 
#	def_kernel.c 68k_kernel.s

realclean:
	/bin/rm -f $(ENGINENAME) $(ENGINENAME).o $(CIAOLIBNAME)

distclean:
	/bin/rm -f *~
	/bin/rm -f TAGS
	/bin/rm -f depends

configure.h: configure
	/bin/rm -f configure.h
	./configure "$(CFLAGS)" > configure.h

crossconfigure.h: configure
	/bin/rm -f configure.h
	/usr/bin/scp configure $(CROSS_COMPILER_HOST):/tmp && \
	/usr/bin/ssh $(CROSS_COMPILER_HOST) /tmp/configure "$(CFLAGS)" > configure.h

configure: configure.c compat.h termdefs.h own_malloc_defs.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o configure configure.c

alloc.o: $(DATADEFS) $(SUPPORT) compat.h own_malloc_defs.h alloc_defs.h streams_defs.h wamsupport_defs.h
attr.o: $(DATADEFS)  $(SUPPORT) predtyp.h attr_defs.h stacks_defs.h
builtin.o: $(DATADEFS) $(SUPPORT) predtyp.h builtin_defs.h wam_defs.h profile_defs.h
cmnd.o: $(DATADEFS) instrdefs.h predtyp.h interface.h commands.h compat.h
dynlink.o: $(DATADEFS) $(SUPPORT) dynlink_defs.h
format.o: $(DATADEFS) $(SUPPORT) format_defs.h wamfunction_defs.h term_support_defs.h
float_consts.o: float_consts.h
float_tostr.o: float_tostr.h float_consts.h
heapgc.o: $(DATADEFS) $(SUPPORT) gcdatadefs.h gcsupport.h threads.h heapgc_defs.h stacks_defs.h
help.o: commands.h
indexing.o: $(DATADEFS) $(SUPPORT) predtyp.h indexing_defs.h wamsupport_defs.h support_defs.h alloc_defs.h objareas_defs.h stacks_defs.h bignum_defs.h locks_defs.h start_defs.h initial_defs.h

initial.o: $(DATADEFS) $(SUPPORT) addmodules.c predtyp.h wamfunction.h wambuiltin.h threads.h task_areas.h  initial_defs.h interrupt_defs.h unix_utils_defs.h term_support_defs.h support_defs.h timing_defs.h locks_defs.h streams_defs.h prolog_tasks_defs.h indexing_defs.h

inout.o: $(DATADEFS) $(SUPPORT) ciao.h term_support_defs.h inout_defs.h streams_defs.h tasks_defs.h
interface.o: $(DATADEFS) $(SUPPORT) interface.h predtyp.h
interrupt.o: $(DATADEFS) $(SUPPORT) compat.h interrupt_defs.h tasks_defs.h
kprim.o: $(DATADEFS) kprim.h instrdefs.h
locks.o: $(DATADEFS) $(SUPPORT) alloc.h locks.h locks_defs.h
main.o: main.h debug.h initial.h threads.h locks.h  wam_defs.h tasks_defs.h unix_utils_defs.h initial_defs.h start_defs.h qread_defs.h builtin_defs.h alloc_defs.h support_defs.h own_malloc_defs.h profile_defs.h
main_exec.o: $(DATADEFS)
misc.o: $(DATADEFS) $(SUPPORT) predtyp.h threads.h tasks_defs.h misc_defs.h bignum_defs.h stacks_defs.h term_support_defs.h timing_defs.h profile_defs.h
nondet.o: $(DATADEFS) $(SUPPORT) predtyp.h  nondet_defs.h stacks_defs.h
objareas.o: $(DATADEFS) $(SUPPORT) predtyp.h objareas_defs.h stacks_defs.h indexing_defs.h alloc_defs.h nondet_defs.h
own_malloc_bin.o: configure.h termdefs.h own_malloc_defs.h
own_malloc_linear.o: configure.h termdefs.h own_malloc_defs.h
prolog_tasks.o: $(DATADEFS) $(SUPPORT) threads.h task_areas.h  wam_defs.h prolog_tasks_defs.h tasks_defs.h
qget.o: $(DATADEFS) $(SUPPORT) threads.h qget_defs.h qread_defs.h bignum_defs.h
qinsert.o: $(DATADEFS) $(SUPPORT) qinstrdefs.h threads.h qinsert_defs.h bignum_defs.h stacks_defs.h
qread.o: $(DATADEFS) $(SUPPORT) qinstrdefs.h qread_defs.h alloc_defs.h qget_defs.h stacks_defs.h
registers.o: $(DATADEFS)
ciao.o: ciao.h ENG_defs.h
stacks.o: $(DATADEFS) $(SUPPORT) threads.h stacks_defs.h heapgc_defs.h
start.o: profile_defs.h
streams.o: $(DATADEFS) $(SUPPORT) alloc_defs.h support_defs.h initial_defs.h
string.o: $(DATADEFS)
support.o: $(DATADEFS) $(SUPPORT) predtyp.h  support_defs.h alloc_defs.h stacks_defs.h bignum_defs.h profile_defs.h
tasks.o: $(DATADEFS) $(SUPPORT) configure.h threads.h task_areas.h  tasks_defs.h initial_defs.h
# threads.o: $(DATADEFS) $(SUPPORT) threads.h
term_support.o: $(DATADEFS) $(SUPPORT) instrdefs.h threads.h task_areas.h term_support_defs.h
timing.o: $(DATADEFS) $(SUPPORT) timing_defs.h
unix_utils.o: $(DATADEFS) $(SUPPORT) compat.h unix_utils_defs.h streams_defs.h stacks_defs.h
version.o:
startgoal.o: $(DATADEFS) $(SUPPORT) wam.h threads.h task_areas.h \
	initial_defs.h inout_defs.h start_defs.h prolog_tasks_defs.h \
	startgoal_defs.h tasks_defs.h term_support_defs.h wam_defs.h
wam.o: $(DATADEFS) $(SUPPORT) \
	wam.h instrdefs.h predtyp.h \
	wamread.c wamwrite.c \
	task_areas.h\
	attr_defs.h builtin_defs.h initial_defs.h \
	inout_defs.h interrupt_defs.h start_defs.h misc_defs.h nondet_defs.h\
	objareas_defs.h stacks_defs.h support_defs.h \
	term_support_defs.h wam_defs.h profile_defs.h
ciao_prolog.o: $(DATADEFS) $(SUPPORT) \
	wam.h instrdefs.h predtyp.h \
	wamread.c wamwrite.c \
	task_areas.h\
	attr_defs.h builtin_defs.h initial_defs.h \
	inout_defs.h interrupt_defs.h start_defs.h misc_defs.h nondet_defs.h\
	objareas_defs.h stacks_defs.h support_defs.h \
	term_support_defs.h wam_defs.h profile_defs.h
wambuiltin.o: $(DATADEFS) $(SUPPORT) wambuiltin.h wambuiltin_defs.h misc_defs.h bignum_defs.h
wamfunction.o: $(DATADEFS) $(SUPPORT) wamfunction.h wamfunction_defs.h
wamsupport.o: $(DATADEFS) $(SUPPORT) instrdefs.h wamsupport_defs.h
gauge.o: $(DATADEFS) $(SUPPORT) predtyp.h gauge_defs.h
bignum.o: $(DATADEFS) $(SUPPORT)

# hashtab.o: hashtab.h recycle.h standard.h lookupa.h
# lookupa.o: lookupa.h standard.h
# recycle.o: recycle.h standard.h

# kernel.o:	$(DATADEFS) kernel_con.c Makefile
# 	$(MAKE) def_kernel.o $(MFLAGS) CC=$(CC) CFLAGS='$(CFLAGS)'
# 	cp def_kernel.o kernel.o;

# def_kernel.c: Makefile
# 	/bin/rm -f def_kernel.c
# 	echo 'static char ignore;' >> def_kernel.c

# def_kernel.o:

# kernel_con: kernel_con.c $(DATADEFS)
# 	$(CC) $(CFLAGS) kernel_con.c -o kernel_con

# kernel.con: kernel_con
# 	/bin/rm -f kernel.con
# 	./kernel_con > kernel.con

# This is for Win32
sockets_c.o:
	cd ../../library/sockets && $(MAKE) sockets_c_$(CIAOARCH)_glue.o
	cp ../../library/sockets/sockets_c_$(CIAOARCH)_glue.o ./sockets_c.o

random.o:
	cd ../../library/random && $(MAKE) random_$(CIAOARCH)_glue.o
	cp ../../library/random/random_$(CIAOARCH)_glue.o ./random.o

# Needs external libs in windows!
# mysql_client.o:
# 	cp ../../library/persdb_mysql/mysql_client.c .
# 	$(CC) $(CFLAGS) -c mysql_client.c

#fastrw.o:
#	cd ../../library/fastwr ; $(MAKE) fastrw_$(CIAOARCH).o
#	cp ../../library/fastrw/fastrw_$(CIAOARCH).o ./fastrw.o

## JF
#instrdefs.h wamread.c wamwrite.c: metaengine
#	./metaengine
#
#metaengine: metaengine.pl
#	ciaoc metaengine.pl

touch:
	touch *.[ch]

tags:
	$(ETAGS)

mode:
	umask 002
