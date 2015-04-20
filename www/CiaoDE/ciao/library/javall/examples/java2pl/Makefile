include ../../../../SETTINGS
include ../../../../SHARED
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

JAVASOURCES=*.java
PROLOGSOURCES=*.pl
JAVACLASSES=*.class
JAVAC=javac
JAVAEXECUTE=java
JAVAFLAGS=-cp $(CIAOSRC)/library/javall:./
JAVACFLAGS=-classpath $(CIAOSRC)/library/javall
LOGFILES=plserver.out plserver.err
PROLOGSERVER=$(CIAOSRC)/library/javall/plserver

all: classes prolog

run:
	$(JAVAEXECUTE) $(JAVAFLAGS) $(PROLOGSOURCES) ./$(PROLOGSERVER)

classes:
	-$(JAVAC) $(JAVACFLAGS) $(JAVASOURCES)
	-chmod $(DATAMODE) $(JAVACLASSES)

prolog:
	$(SETLOCALCIAO) $(CIAOC) -c $(PROLOGSOURCES)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	/bin/rm -f *.itf *.po *.dep *.asr
	/bin/rm -f $(LOGFILES)

distclean: realclean
	/bin/rm -f *~ 

tags:
	-etags -l java $(JAVASOURCES)


