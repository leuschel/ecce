MODULE=PLToJava/java_daemon
REQLIBS=

SRCNAME=$(MODULE).java
LIBNAME=
HDRNAME=
OBJNAME=$(MODULE).class
HEADERS= 
JAVAC=javac
CFLAGS=-deprecation -sourcepath PLToJava -classpath ${CLASSPATH}:.
OBJECTS= PLToJava/PLTerm.class PLToJava/PLAtom.class PLToJava/PLInteger.class \
	 PLToJava/PLList.class PLToJava/PLStructure.class \
	 PLToJava/PLVariable.class PLToJava/PLActionListener.class \
	 PLToJava/PLEventListener.class PLToJava/PLInterpreter.class \
	 PLToJava/TCPServer.class PLToJava/PLTalker.class \
	 PLToJava/PLGoal.class PLToJava/PLConnection.class \
	 PLToJava/PLGoalException.class


$(OBJNAME): $(SRCNAME) $(OBJECTS)
	$(JAVAC) $(CFLAGS) $(SRCNAME)

%.class : %.java
	$(JAVAC) $(CFLAGS) $<

clean:
	/bin/rm -f $(OBJNAME)

realclean: clean
	/bin/rm -f $(LIBNAME) $(OBJECTS) $(OBJNAME)

distclean: realclean itfclean jclean

itfclean:
	/bin/rm -f -r *.itf
	/bin/rm -f -r *.po
	/bin/rm -f -r *.dep
	/bin/rm -f -r ../*.itf
	/bin/rm -f -r ../*.po
	/bin/rm -f -r ../*.dep
	/bin/rm -f -r ../Examples/*.itf
	/bin/rm -f -r ../Examples/*.po
	/bin/rm -f -r ../Examples/*.dep
	/bin/rm -f -r ../java/lang/*.itf
	/bin/rm -f -r ../java/lang/*.po
	/bin/rm -f -r ../java/lang/*.dep
	/bin/rm -f -r ../java/awt/*.itf
	/bin/rm -f -r ../java/awt/*.po
	/bin/rm -f -r ../java/awt/*.dep

jclean:
	/bin/rm -f -r ../java/*

jsample1:
	java java2pl ro java.awt.Frame ../
	java java2pl ro java.awt.Button ../
	java java2pl ro java.awt.GridLayout ../
	java java2pl ro java.awt.BorderLayout ../
	java java2pl ro java.awt.TextField ../
	java java2pl ro java.awt.event.ActionEvent ../

tags:
#	etags -l none -r '/^[a-zA-Z0-9_]+(/' *.pl
#	etags --append -l java *.java
	etags -l java -r *.java


