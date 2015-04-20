include /home/clip/Systems/ciao/SETTINGS
include $(CIAOSRC)/SHARED
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

# Known suffixes.  This allos make to take care of time dependencies.
.SUFFIXES: .java .class


# Java sources, etc.
JAVAC=javac
JAVAEXECUTE=java
JAVAFLAGS=-classpath ./:$(CIAOSRC)/library/javall/
# We need to list them explicitly because there are no .class file
# in a clean distribution
JAVACLASSES=PhoneInsert.class PhoneList.class PhoneSearch.class
JAVASOURCES=*.java

# Main Prolog file
PROLOGMAIN=agenda.pl

# How to compile a .java into a .class
.java.class:
	$(JAVAC) $(JAVAFLAGS) $<


all: classes prolog

classes: $(JAVACLASSES)

prolog:
	$(SETLOCALCIAO) $(CIAOC) $(PROLOGMAIN)

clean realclean:
	/bin/rm -f *.class
	/bin/rm -f *.itf *.po *.dep *.asr

distclean: realclean
	/bin/rm -f *~ 

tags:
	-etags -l java $(JAVASOURCES)


