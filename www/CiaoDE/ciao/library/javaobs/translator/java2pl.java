/**
 * java2pl
 * This program reads a JAR file given as argument, and generates the
 * Prolog O'CIAO code describing all the classes included
 * in the JAR file.
 * For an explanation on line arguments, see {@see main}.
 */

import java.lang.*;
import java.lang.reflect.*;
import java.util.jar.*;
import java.util.*;
import java.io.*;

public class java2pl {

  /**
   * main method. Receives the line arguments for java to o'ciao conversion
   * and launches the translation itself.
   *
   * The arguments that can be received in the command line are:
   *   
   *  java2pl {crfom} {class-name|jar-file} dest-dir
   *
   * where:
   * c, r and f concerns the classes to be translated, in the following way 
   * (options are exclusive):
   *  c means one-class conversion: the second argument must be a full named
   *    class name.
   *  r means one-class conversion and declared classes recursion: translates
   *    the class refered as second argument, and all the classes that are
   *    used so the prolog translation can work correctly.
   *  f means jar-file conversion: translates the classes included in the
   *    jar file received as second argument.
   *  
   * o and m concerns the prolog syntax to be used (exclusive too):
   *  o means o'ciao syntax (default).
   *  m means ciao module syntax.
   **/
  public static void main(String argv[]) throws Exception {

    /*
     * Command line arguments.
     */
    if (argv.length == 3) {
      String destdir = argv[2];

      if (argv[0].indexOf("c") != -1) {
	Class cl = null;
	try {
	  cl = Class.forName(argv[1]);
	} catch (ClassNotFoundException e) {
	  System.err.println("Error: Cannot get information on " + argv[1]);
	  System.exit(1);
	}

        /*
         * For each class the prolog file will be created, and
         * syntax selection will be made. "m" means module syntax.
         * Otherwise (or "o") means O'CIAO object syntax.
         */
        if (argv[0].indexOf("m") == -1)
          (new PrologClassFile(argv[1], destdir)).generate(cl);
       else
          (new PrologModuleFile(argv[1], destdir)).generate(cl);
      }
      else if (argv[0].indexOf("r") != -1) {
	/*
	 * Recursive file generation.
	 */
	Class cl = null;
	try {
	  cl = Class.forName(argv[1]);
	} catch (ClassNotFoundException e) {
	  System.err.println("Error: Cannot get information on " + argv[1]);
	  System.exit(1);
	}

        if (argv[0].indexOf("m") == -1)
          (new PrologClassFile(argv[1], destdir)).generateRecursive(cl);
        else
          (new PrologModuleFile(argv[1], destdir)).generateRecursive(cl);
      }
      else {
        String jarfile = argv[1];
        
        JarFile j = new JarFile(jarfile);
        
	int count = 0;
        Enumeration e = j.entries();
        while (e.hasMoreElements()) {
          String file = ((JarEntry)e.nextElement()).getName();
          System.err.println(file);
          /*
           * Only .class files that are not inner classes are needed.
           */
          if (file.endsWith(PrologFile.CLASS) &&
              file.lastIndexOf(PrologFile.INNER_CLASS_CHAR) < 0) {

	    count++;
        
            /*
             * Set the java class naming format.
             */
            String className = file.replace(File.separatorChar,
                                            PrologFile.PKG_SEP);
            className = 
	      className.substring(0, className.lastIndexOf(PrologFile.CLASS));

	    Class cl = null;
	    try {
	      cl = Class.forName(className);
	    } catch (ClassNotFoundException er) {
	      System.err.println("Error: Cannot get information on " + argv[1]);
	      System.exit(1);
	    }

            /*
             * For each class the prolog file will be created, and
             * syntax selection will be made. "m" means module syntax.
             * Otherwise (or "o") means O'CIAO object syntax.
             */
            if (argv[0].indexOf("m") == -1)
              (new PrologClassFile(jarfile, destdir)).generate(cl);
            else
              (new PrologModuleFile(jarfile, destdir)).generate(cl);
          }
        }
	System.err.println("Number of classes processed: " + count);
      }
    }
    else
      usage();

    System.exit(0);
  }

  /**
   * usage
   * Usage instructions.
   */
  private static void usage() {
    System.err.println("Usage: java2pl {crfom} {class-name jar-file} dest-dir");
    System.err.println("       c: one class conversion");
    System.err.println("       r: like class conversion, but converts all");
    System.err.println("          the classes referenced by class-name");
    System.err.println("       f: jar file conversion");
    System.err.println("       o: O'CIAO syntax (default)");
    System.err.println("       m: CIAO module syntax");
  }

}

/**
 * class PrologFile
 * Abstract class that represents the prolog file generation. Is used to
 * gather the common code useful to generate module syntax prolog files
 * and O'CIAO syntax prolog files.
 * Only one method is abstract: generate().
 *
 */
abstract class PrologFile {

  /*
   * General constants.
   */
  public static final String CLASS = ".class";
  public static final char INNER_CLASS_CHAR = '$';
  public static final char PKG_SEP = '.';
  public static final String VAR_NAME = "V";
  public static final String TAB = "    ";
  public static final String JAVART = "javart.pl";
  public static final String JAVA_INHERITANCE = "java_inheritance.pl";
  public static final char UNDERSCORE = '_';
  /*
   * Prolog predicates and operators.
   */
  private static final String NONVAR_PRED = "nonvar";
  protected static final String ASSERTA_FACT = "asserta_fact";
  protected static final String INSTANCE_OF = "instance_of";

  protected static final String JAVA_SUPERCLASS = "java_superclass";
  protected static final String INTERNAL_JAVA_SUPERCLASS = "'$java_superclass'";

  /*
   * Protected fields
   */
  protected PrintWriter out = null;
  protected String headerTitle = "";
  protected String destdir = ".";

  /**
   * Abstract Methods
   */
  protected abstract void genInheritance(Class curClass);
  protected abstract void genMiscelanea(Class curClass);
  protected abstract void genInterfaces(Class curClass);
  protected abstract void printDeclaration(Class curClass, Iterator it);
  protected abstract void printImportation(Class module, Class curClass);
  protected abstract void printField(Field f);
  protected abstract void printFieldAssertion(Field f);
  protected abstract void printConstructor(Constructor c, Class curClass);
  protected abstract void printMethod(Method m);
  protected abstract Iterator getDeclElements(Class curClass);
  protected abstract Iterator getDeclElementSet(Class curClass);
  protected abstract String printObjectTypeTesting(String var, Class cl);

  /**
   * PrologFile
   * Constructor. Creates a new PrologFile object given a class name, and
   * a title (to put at the top of the file).
   */
  public PrologFile(String title, String destdir) {

    this.headerTitle = title;
    this.destdir = destdir;

  }


  /**
   * generate
   * Generate a .pl file with the prolog code associated to the java
   * class of the same name.
   *
   */
  void generate(Class curClass) {

    // Get the prolog object name.
    String sName = getPrologName(curClass.getName());
    String clName = curClass.getName();

    // Interfaces are not translated. Interface information is gathered in
    // the classes that implement them (see genInterfaces below).
    //@    if (curClass.isInterface())
    //@      return;
    

    // File creation.
    String fileName;
    if (this.destdir.substring(destdir.length()) == File.separator)
      fileName = this.destdir + getFileName(clName) + ".pl";
    else
      fileName = this.destdir + File.separator + getFileName(clName) + ".pl";

    try {
      File ddir = new File(getFilePath(fileName));
      if (!ddir.exists())
        if (!ddir.mkdirs())
          throw new IOException("Error: Path " + ddir + " cannot be created.");
      
      out = new PrintWriter(new FileOutputStream(fileName));
    } catch (IOException e) {
      System.err.println("Error: Cannot open output file " + fileName);
      System.err.println(e);
      System.exit(1);
    }

    genHeaderComments(clName, this.headerTitle);
    genDeclarations(curClass);
    genInterfaces(curClass);
    if (!curClass.isInterface()) {
      genInheritance(curClass);
//      genDeclaredClasses(curClass);
      genMiscelanea(curClass);

      genFields(curClass);
      genConstructors(sName, curClass);
      genMethods(curClass);
      genFinalDeclarations();
    }

    out.close();
  }

  void generateRecursive(Class curClass) {

    // Get a list with all the classes referenced by the
    // class received as argument (directly or indirectly).
    ArrayList cls = new ArrayList();
    ArrayList newcls = new ArrayList();

    newcls.add((Object)curClass);

    while (newcls.size() > 0) {
      Object newcl = newcls.get(0);
      cls.add(newcl);
      newcls.remove(0);

      Iterator im = getImportations(getDeclElements((Class)newcl),
				    (Class)newcl);
      
      while (im.hasNext()) {
	Object type = im.next();
	if (!cls.contains(type) && !newcls.contains(type))
	  newcls.add(type);
      }

      Class sClass = ((Class)newcl).getSuperclass();
      if (sClass != null)
	if (!cls.contains(sClass) && !newcls.contains(sClass))
	  newcls.add(sClass);

      Class[] in = ((Class)newcl).getInterfaces();
      for (int i = 0 ; i < in.length ; i++)
	if (!cls.contains(in[i]) && !newcls.contains(in[i]))
	  newcls.add(in[i]);
 
    }
    
    for (int i = 0; i < cls.size(); i++)
      generate((Class)cls.get(i));
    
  }

  /**
   * genHeaderComments
   * Printing of class generic comments.
   *
   * Parameters:
   *  clName - java full class name.
   *  title - title to be printed on the file header comments.
   */
  private void genHeaderComments(String clName, String title) {

    out.println("%%");
    out.println("%% Class " + clName);
    out.println("%% NOTICE: Do not edit this file." +
		" This file has been generated from:");
    out.println("%% " + title);
    out.println("%%");

  }

  /**
   * genDeclarations
   * Printing of prolog class/module declaration.
   * The class/module declaration includes the constructors and
   * the public fields and methods of the corresponding java class.
   *
   * Parameters:
   *  curClass - Class object representing the java class.
   *
   */
  private void genDeclarations(Class curClass) {

    /*
     * Class/Module declaration printing.
     */
    out.println();
    printDeclaration(curClass, getDeclElementSet(curClass));

  }

  /**
   * genFields
   * Field information printing. In O'CIAO the java attributes (fields)
   * are declared as methods, so that the system can retrieve the actual
   * content of a field at run time.
   *
   * Parameters:
   *  curClass - class object representing the java class.
   *
   */
  private void genFields(Class curClass) {

    Field fl[] = curClass.getFields();

    // Fields must be sorted.
    Arrays.sort(fl, new ReflectComparator());

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Java fields.");
    out.println("%%--------------------------------------------------");
    out.println();
    for (int i=0; i<fl.length; i++)
      // Only public fields are included.
      if (Modifier.isPublic(fl[i].getModifiers())) {
	printField(fl[i]);
	out.println();
	if (!Modifier.isFinal(fl[i].getModifiers()))
	  printFieldAssertion(fl[i]);
	out.println();
      }

  }

  /**
   * genDeclaredClasses
   * Generates the declarations needed to access to all the classes used
   * by this class. These include the classes of the arguments of the
   * methods and constructors, and the type of the fields of the class.
   *
   * Parameters:
   * @param curClass class object representing the java class.
   *
   */
  private void genDeclaredClasses(Class curClass) {

    Iterator im = getImportations(getDeclElements((Class)curClass),
				  (Class)curClass);

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Declared classes.");
    out.println("%%--------------------------------------------------");
    out.println();
    while (im.hasNext()) { 
      Class cl = (Class)im.next();
      if (!cl.isInterface())
	printImportation(cl, curClass);
    }
    out.println();

  }

  /**
   * genConstructors
   * Constructor printing.
   *
   * Parameters:
   *  sName - prolog class name.
   *  curClass - Class object representing the java class.
   *
   */
  private void genConstructors(String sName, Class curClass) {

    Constructor cn[] = curClass.getDeclaredConstructors();

    // Sort the array to put together the constructors with the same arity.
    Arrays.sort(cn, new ReflectComparator());
    
    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Constructors.");
    out.println("%%--------------------------------------------------");
    out.println();
    
    for (int i = 0; i<cn.length; i++) 
      // Only public constructors are included.
      if (Modifier.isPublic(cn[i].getModifiers())) {
	printConstructor(cn[i], curClass);
	out.println();
      }

  }
  
  /**
   * genMethods
   * Public method printing.
   *
   * Parameters:
   *  curClass - Class object representing the java class.
   *
   */
  private void genMethods(Class curClass) {
    Method mt[] = curClass.getDeclaredMethods();

    // Sort the array to put together the methods with the same name and arity.
    Arrays.sort(mt, new ReflectComparator());
    
    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Methods.");
    out.println("%%--------------------------------------------------");
    out.println();
    
    for (int i = 0; i < mt.length; i++)
      // Only public methods are included.
      if (Modifier.isPublic(mt[i].getModifiers()))
	if (!Modifier.isAbstract(mt[i].getModifiers())) {
	  printMethod(mt[i]);
	  out.println();
	}

  }

  /**
   * genFinalDeclarations
   * Miscelanea about final declarations (toggle prolog flags).
   *
   */
  private void genFinalDeclarations() {

    out.println();
    out.println(":- set_prolog_flag(multi_arity_warnings,on).");
    out.println(":- set_prolog_flag(discontiguous_warnings,on).");
    out.println();

  }

  /**
   * varSequence
   * returns a sequence of n prolog variables enclosed by parentheses.
   */
  protected String varSequence(int n) {
    String s = "";

    if (n > 0) {
      s = "(" + getPrologVar(0);
      for (int j = 1; j<n; j++) {
        s += ", " + getPrologVar(j);
      }
      s += ")";
    }

    return s;
  }

  /**
   * varSequence
   * Prints a sequence of n prolog variables enclosed by parentheses,
   * plus a variable whose name is given as 2nd argument.
   */
  protected String varSequence(int n, String var) {
    String s = "(";

    if (n > 0)
      for (int j = 0; j<n; j++) {
        s += getPrologVar(j) + ", ";
      }
    s += var + ")";
    return s;
  }

  /**
   * varSequence
   * Prints a sequence of n prolog variables enclosed by parentheses,
   * plus an initial variable whose name is given as 1st argument.
   */
  protected String varSequence(String var, int n) {
    String s = "(";

    s += var;
    if (n > 0)
      for (int j = 0; j<n; j++) {
        s += ", " + getPrologVar(j);
      }
    s += ")";
    return s;
  }

  /**
   * varSequence
   * Prints a sequence of n prolog variables enclosed by parentheses,
   * plus an initial variable whose name is given as 1st argument.
   */
  protected String varSequence(String var1, int n, String var2) {
    String s = "(";

    s += var1;
    if (n > 0)
      for (int j = 0; j<n; j++) {
        s += ", " + getPrologVar(j);
      }

    if (s.length() > 1)
      s += ", " + var2 + ")";
    else
      s += var2 + ")";

    return s;
  }

  /**
   * printInclude
   * Prints on the out stream the include declaration of
   * the prolog file whose name is given as argument.
   */
  protected void printInclude(String file) {

    out.println(":- include(" + file + ").");

  }

  /**
   * printExport
   * Prints on the out stream the export declaration of
   * the prolog predicate or attribute whose name/arity is given as argument.
   */
  protected void printExport(String export) {

    out.println(":- export(" + export + ").");

  }

  /**
   * printTypeTesting
   * Printing of the piece of prolog code that tests the type of a term.
   * Returns the list of variables result of the test. This list of
   * variables may be different from the list of arguments of the prolog
   * clause because can be made a conversion on the prolog arguments
   * if these are java objects.
   * NOTE: This piece of prolog code must be followed by at least one
   * more goal.
   * 
   * Parameters:
   *  tName - String with the prolog name of the term.
   *  curClass - Class object representing the type of the prolog term.
   *
   * @return the list of variables that have been returned as result
   *         of the type testing. These variables will be the usual
   *         V0, ..., Vn if there are no java objects in the 
   *         class list received as argument; otherwise, the variables
   *         that are tested as java objects will be represented by
   *         variable names of the form VnOBJ.
   *
   */
  protected String printTypeTesting(Class[] curClass) {

    String varNames = "";

    for (int i = 0; i < curClass.length; i++) {
      if (varNames.length() != 0)
	varNames += ", ";

      out.print(TAB);
      if (curClass[i].isArray()) {
	out.print("list(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals(Integer.TYPE)) {
	out.print("java_integer(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals(Short.TYPE)) {
	out.print("java_short(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals(Long.TYPE)) {
	out.print("java_long(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals(Byte.TYPE)) {
	out.print("java_byte(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals(Character.TYPE)) {
	out.print("java_character(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals(Float.TYPE)) {
	out.print("java_float(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals(Double.TYPE)) {
	out.print("java_double(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals((new String()).getClass())) {
	out.print("string(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else if (curClass[i].equals(Boolean.TYPE)) {
	out.print("java_boolean(" + getPrologVar(i) + ")");
	varNames += getPrologVar(i);
      }
      else {
        // Is not a list nor a primitive type.
	varNames += printObjectTypeTesting(getPrologVar(i), curClass[i]);
      }

      if (curClass.length > 0)
	out.println(",");
    }

    return varNames;

  }

  /**
   * getPrologName
   * Get the prolog name of an identifier: if the identifier begins with 
   * uppercase, returns the identifier enclosed between quotes;
   * if the identifier represents a class object, returns the java
   * simple name of the class (with no package prefix) --enclosed between 
   * quotes if begins in uppercase--.
   * 
   * Parameters:
   *  name - java identifier.
   *
   * Returns:
   *  the name of the prolog associated element.
   */
  protected static String getPrologName(String name) {
    
    String sName = name.substring(name.lastIndexOf(PKG_SEP)+1);

    if (isPrologId(sName))
      return sName;
    else
      return "'" + sName + "'";

  }

  /**
   * isPrologId
   * Returns true if the string received can be used as a prolog id
   * (an atom or a functor name).
   */
  private static boolean isPrologId(String id) {

    if (id.length() > 0) {
      char initial = id.charAt(0);
    
      // First character must be in lowercase
      if (Character.isUpperCase(initial))
	return false;

      // First character must be a letter or underscore.
      if (!Character.isLetter(initial) && initial != UNDERSCORE)
	return false;

      // All characters must be letter, digit or underscore.
      for (int i = 1; i < id.length(); i++) {
	char c = id.charAt(i);
	if (!Character.isLetterOrDigit(c) && c != UNDERSCORE)
	  return false;
      }
    }
    return true;
  }

  /**
   * getFileName
   * Get the class file name: is the java name of the class (with
   * full package prefix) using the path separator instead of the
   * package separator ('.').
   * 
   * Parameters:
   *  clName - full name of the java class.
   *
   * Returns:
   *  the name of the class file name (initial in lowercase).
   */
  protected String getFileName(String clName) {

    String fName = clName.replace(PKG_SEP, File.separatorChar);
    //    return fName + ".pl"; // Por ahora no se pone extension.
    return fName;

  }

  /**
   * getRelFileName
   * Get the class file name, relative to the class being translated.
   * 
   * Parameters:
   *  clName - full name of the java class whose file name must be obtained.
   *  curClass - Class object representing the class being implemented.
   *
   * Returns:
   *  the name of the relative class file name.
   */
  protected String getRelFileName(String clName, Class curClass) {

    return getRelBasePath(curClass) + getFileName(clName);

  }

  /**
   * getRelBasePath
   * Get the base path relative to the current class
   * 
   * Parameters:
   *  curClass - Class object representing the class being implemented.
   *
   * Returns:
   *  the base path relative to the current class.
   */
  protected String getRelBasePath(Class curClass) {
    int idx = 0;
    String fName = "";
    String fCurClass = getFileName(curClass.getName());

    while ((idx = fCurClass.indexOf(File.separator, idx)) != -1) {
      fName = fName + ".." + File.separator;
      idx++;
    }
    return fName;
  }

  /**
   * getFilePath
   * Get the path of a file name given as argument.
   * 
   * Parameters:
   *  fileName - file name.
   *
   * Returns:
   *  the path of the file name
   */
  private String getFilePath(String fileName) {

    return fileName.substring(0,fileName.lastIndexOf(File.separatorChar));

  }

  /**
   * getPrologVar
   * Given an integer, generates a prolog variable name.
   *
   * Parameters:
   *  num - Variable number.
   *
   * Returns:
   *  prolog variable name.
   *
   */
  protected String getPrologVar(int num) {

    return VAR_NAME + String.valueOf(num);

  }

  /**
   * getPrologVar
   * Given a variable name and a string containing a variable name suffix, 
   * generates a prolog variable name.
   *
   * Parameters:
   * @param var Variable name.
   * @param sfx Variable suffix.
   *
   * @return    The resulting prolog variable name.
   *
   */
  protected String getPrologVar(String var, String sfx) {

    return var + sfx;

  }

  /*
   * getInterfaceSet
   * Private method to obtain recursively all the interfaces in the interface
   * hierarchy of a given class.
   *
   * Parameters:
   *  curClass - Class object representing the class.
   */
  protected TreeSet getInterfaceSet(Class curClass) {
    TreeSet ts = new TreeSet(new ReflectComparator());
    Class in[] = curClass.getInterfaces();

    for (int i = 0 ; i < in.length ; i++) {
      ts.add(in[i]);
      ts.addAll(getInterfaceSet(in[i]));
    }

    /*
     * Superclass interfaces.
     */
    Class sc = curClass.getSuperclass();
    if (sc != null)
      ts.addAll(getInterfaceSet(sc));

    return ts;
  }

  /**
   * getImportations
   * Returns an iterator over the classes/modules that must be
   * imported from this class/module. Those classes/modules are
   * the ones used by all the elements exported by this class/
   * module.
   *
   *  @param elements Iterator over the elements declared in
   *                  the current class.
   *  @param curClass Class object representing the class being
   *                  translated.
   */
  private Iterator getImportations(Iterator elements, Class curClass) {

    TreeSet types = new TreeSet(new ReflectComparator());

    while (elements.hasNext()) {
      Object el = elements.next();
      Iterator elTypes = ReflectComparator.getTypes(el);
      while (elTypes.hasNext()) {
	Object type = elTypes.next();
	if (!type.equals(curClass))
	  types.add(type);
      }
    }

    return types.iterator();
  }

  /**
   * toList
   * This method is used to avoid the bug in the linux version of jdk1.2.
   * (and replacing the Arrays.toList(Object[] o), existent in jdk1.2 for
   * solaris).
   */
  protected List toList(Object[] o) {
    ArrayList a = new ArrayList();

    for (int i = 0; i < o.length; i++)
      a.add(o[i]);

    return a;
  }

  /** **************************************************************
   *
   * reflectComparator 
   * Inner class used to sort a list of reflection objects.
   * These include Class, Method, Field, and Constructor objects.
   *
   ** **************************************************************/
  static class ReflectComparator implements Comparator {

    /*
     * compare
     * Comparison of reflection objects.
     */
    public int compare(Object r1, Object r2) {

      String s1 = toString(r1);
      String s2 = toString(r2);

      return s1.compareTo(s2);

    }

    public static String classRepresentation(Object r) {

      return getName(r) + "/" + getClassArity(r);

    }

    public static String moduleRepresentation(Object r) {

      return getName(r) + "/" + getModuleArity(r);

    }

    private static String getName(Object r) {
      String s = null;

      if (r instanceof Field)
        s = PrologFile.getPrologName(((Field)r).getName());

      if (r instanceof Class)
        s = PrologFile.getPrologName(((Class)r).getName());

      if (r instanceof Constructor)
        s = PrologFile.getPrologName(((Constructor)r).getName());

      if (r instanceof Method)
	s = PrologFile.getPrologName(((Method)r).getName());

      return s;
    }

    public static boolean isPublic(Object r) {
      boolean s = false;

      if (r instanceof Field)
	s = Modifier.isPublic(((Field)r).getModifiers());

      if (r instanceof Class)
	s = Modifier.isPublic(((Class)r).getModifiers());

      if (r instanceof Constructor)
	s = Modifier.isPublic(((Constructor)r).getModifiers());

      if (r instanceof Method)
	s = Modifier.isPublic(((Method)r).getModifiers());

      return s;
    }

    private static int getClassArity(Object r) {
      int n = 0;

      if (r instanceof Field)
	n = 1;

      if (r instanceof Class)
	n = 0;

      if (r instanceof Constructor)
	n = ((Constructor)r).getParameterTypes().length;

      if (r instanceof Method)
        if (((Method)r).getReturnType().equals(Void.TYPE))
	  n = ((Method)r).getParameterTypes().length;
        else
	  n = ((Method)r).getParameterTypes().length + 1;

      return n;
    }

    private static int getModuleArity(Object r) {
      int n = 0;

      if (r instanceof Field)
	n = 2;

      if (r instanceof Class)
	n = 0;

      if (r instanceof Constructor)
	n = ((Constructor)r).getParameterTypes().length + 1;

      if (r instanceof Method)
        if (((Method)r).getReturnType().equals(Void.TYPE))
	  n = ((Method)r).getParameterTypes().length + 1;
        else
	  n = ((Method)r).getParameterTypes().length + 2;

      return n;
    }

    /*
     * toString
     * Get the prolog class syntax for the reflection object.
     *
     */
    public static String toString(Object r) {

      return getName(r) + "/" + getClassArity(r);

    }

    /*
     * getTypes
     * Gets an iterator over the types needed by the reflection
     * object given as argument.
     * On a constructor or a method, these types are the types
     * of the parameters. On a field, returns an iterator with only
     * one type: itself. Geting types on a Class object is not
     * considered.
     *
     * The iterator returned is based on a TreeSet, in order to
     * avoid element repetition.
     */
    public static Iterator getTypes(Object r) {

      TreeSet ts = new TreeSet(new ReflectComparator());

      if (r instanceof Field) {
	Class type = ((Field)r).getType();
	while (type.isArray())
	  type = type.getComponentType();

        if (!type.isPrimitive())
          ts.add(type);
      }
      else if (r instanceof Constructor) {
        Class param[] = ((Constructor)r).getParameterTypes();
        for (int i = 0 ; i < param.length ; i++) {
	  Class type = param[i];
	  while (type.isArray())
	    type = type.getComponentType();
	  
          if (!type.isPrimitive())
            ts.add(type);
	}
      }
      else if (r instanceof Method) {
	Class param[] = ((Method)r).getParameterTypes();
        for (int i = 0 ; i < param.length ; i++) {
	  Class type = param[i];
	  while (type.isArray())
	    type = type.getComponentType();
	  
          if (!type.isPrimitive())
           ts.add(type);
	}
      }

      return ts.iterator();
    }      
  }
}

/**
 * *****************************************************************
 *                                                                 *
 * Class PrologClassFile                                           *
 * Implementation of a PrologFile to generate prolog class files   *
 * for java interface.                                             *
 *                                                                 *
 * *****************************************************************
 */
class PrologClassFile extends PrologFile {
  /*
   * Prolog constants.
   */
  // Methods declared on the prolog class java_obj.
  private static final String GET_FIELD_METHOD = "java_get_value";
  private static final String SET_FIELD_METHOD = "java_set_value";
  private static final String CONSTRUCTOR = "java_obj";
  private static final String JAVA_BASE_OBJECT = "java_obj";
  private static final String INVOKE_METHOD = "java_invoke_method";
  private static final String INTERNAL_JAVA_DESTRUCTOR = "java_delete_object";
  private static final String GET_JAVA_ID = "get_java_id";
  private static final String JAVA_ADD_LISTENER = "java_add_listener";
  private static final String JAVA_REMOVE_LISTENER = "java_remove_listener";


  private static final String INTERFACE = "interface";
  private static final String IMPLEMENTS = "implements";
  private static final String USE_CLASS = "use_class";

  public PrologClassFile(String title, String destdir) {
    super(title, destdir);
  }

  /**
   * genInheritance
   *
   * Printing of Inheritance information. If the java class inherits from
   * any other java class, then the prolog class inherits from the associated
   * prolog class too. Otherwise, the prolog class will inherit from 
   * 'java_obj' (only the 'Object' java class does not inherit from any other
   * class).
   * 
   * Parameters:
   *  curClass - class object representing the class being translated.
   */
  protected void genInheritance(Class curClass) {

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Inheritance information.");
    out.println("%%--------------------------------------------------");
    out.println();

    Class sClass = curClass.getSuperclass();
    if (sClass != null)
      out.println(":- inherit_class(library('javaobs/"
		  + getFileName(sClass.getName()) + "')).");
    else {
      out.println(":- inherit_class(library('javaobs/"
		  + JAVA_BASE_OBJECT + "')).");
    }
  }

  /**
   * genMiscelanea
   * Miscelanea. This method generates the prolog code that
   * cannot be included in any other generation method, and
   * that depends on the prolog syntax used.
   */
  protected void genMiscelanea(Class curClass) {

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Miscelanea.");
    out.println("%%--------------------------------------------------");
    out.println();

    out.println(":- redefining(_).");
    out.println(":- set_prolog_flag(multi_arity_warnings,off).");
    out.println(":- set_prolog_flag(discontiguous_warnings,off).");
    out.println();
    out.println(":- discontiguous java_assert/1.");
    out.println(":- use_module(library(lists)).");
    //jcf 10.02.00
    //    out.println();
    //    out.println(":- export(java_constructor/1).");
    //    out.println(":- export(" + INVOKE_METHOD + "/1).");
    //    out.println(":- export(" + GET_FIELD_METHOD + "/1).");
    //    out.println(":- export(" + SET_FIELD_METHOD + "/1).");
    //    out.println(":- export(" + JAVA_ADD_LISTENER + "/2).");
    //    out.println(":- export(" + JAVA_REMOVE_LISTENER + "/2).");
    //    out.println(":- export(" + INTERNAL_JAVA_DESTRUCTOR + "/0).");
    //    out.println(":- export(" + GET_JAVA_ID + "/1).");
    //fin jcf 10.02.00
//    out.println(":- export(java_assert/1).");

    out.println(":- use_class(library('javaobs/" + JAVA_BASE_OBJECT + "')).");
    out.println();

    // regular types for java basic types.
    //    out.println(":- regtype java_boolean(X).");
    out.println("java_boolean(yes).");
    out.println("java_boolean(no).");
    out.println();
    //    out.println(":- regtype java_integer(X).");
    out.println();
    out.println("java_integer(X) :-");
    out.println(TAB + "X > " + Integer.MIN_VALUE + ",");
    out.println(TAB + "X < " + Integer.MAX_VALUE + ".");
    out.println();
    //    out.println(":- regtype java_short(X).");
    out.println();
    out.println("java_short(X) :-");
    out.println(TAB + "X > " + Short.MIN_VALUE + ",");
    out.println(TAB + "X < " + Short.MAX_VALUE + ".");
    out.println();
    //    out.println(":- regtype java_long(X).");
    out.println();
    out.println("java_long(X) :-");
    out.println(TAB + "X > " + Long.MIN_VALUE + ",");
    out.println(TAB + "X < " + Long.MAX_VALUE + ".");
    out.println();
    //    out.println(":- regtype java_byte(X).");
    out.println();
    out.println("java_byte(X) :-");
    out.println(TAB + "X > " + Byte.MAX_VALUE + ",");
    out.println(TAB + "X < " + Byte.MAX_VALUE + ".");
    out.println();
    //    out.println(":- regtype java_character(X).");
    out.println();
    out.println("java_character(X) :-");
    out.println(TAB + "character_code(X).");
    out.println();
    //    out.println(":- regtype java_float(X).");
    out.println();
    out.println("java_float(X) :-");
    out.println(TAB + "X > " + Float.MAX_VALUE + ",");
    out.println(TAB + "X < " + Float.MAX_VALUE + ".");
    out.println();
    //    out.println(":- regtype java_double(X).");
    out.println(); 
    out.println("java_double(X) :-");
    out.println(TAB + "X > " + Double.MAX_VALUE + ",");
    out.println(TAB + "X < " + Double.MAX_VALUE + ".");

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Destructor.");
    out.println("%%--------------------------------------------------");
    out.println();

    printDispose(curClass);
  }

  /**
   * genInterfaces
   * Interface information printing. If the java class implements any
   * interface, the prolog class/module initializes the java_interface
   * attribute with the interfaces implemented.
   * 
   * Parameters:
   *  curClass - class object representing the class.
   */
  protected void genInterfaces(Class curClass) {

    Class in[] = curClass.getInterfaces();

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Interface Information.");
    out.println("%%--------------------------------------------------");
    out.println();

    for (int i = 0 ; i < in.length ; i++)
      printInterface(curClass, (Class)in[i]);
    
    out.println();
  }

  /**
   * getDeclElements
   * Gets the elements to be included in the prolog file 
   * as class/module declarations.
   * This method returns an iterator over all the elements
   * used by the class given as argument.
   */
  protected Iterator getDeclElements(Class curClass) {

    ArrayList el = new ArrayList();

    el.addAll(toList((Object[])curClass.getDeclaredConstructors()));
    el.addAll(toList((Object[])curClass.getFields()));
    el.addAll(toList((Object[])curClass.getDeclaredMethods()));

    int i = 0;
    while (i < el.size()) {
      if (!ReflectComparator.isPublic(el.get(i)))
	el.remove(i);
      else
	i++;
    }
    
    return el.iterator();
  }

  /**
   * getDeclElementSet
   * Gets the elements to be included in the prolog file 
   * as class/module declarations.
   * This method returns an ordered iterator over unique elements
   * in order to avoid prolog predicate repetition (elements with
   * the same name and arity).
   */
  protected Iterator getDeclElementSet(Class curClass) {

    Iterator it = getDeclElements(curClass);
    ReflectComparator rc = new ReflectComparator();
    TreeSet ts = new TreeSet(rc);

    while(it.hasNext())
      ts.add(it.next());

    return ts.iterator();
  }

  /**
   * printDeclaration
   * Prints the declaration information of the class whose name
   * is given as argument. The second argument is the set of
   * predicates exported by this class. Includes java constructor,
   * field and method information.
   */
  protected void printDeclaration(Class curClass, Iterator it) {

    String sName = getPrologName(curClass.getName());

    if (curClass.isInterface())
      out.println(":- interface(" + sName + ").");
    else {
      // NOTA: se sustituye use_package debido a un bug en
      //       OCIAO.
      out.println(":- class(" + sName
		  + ",[],[objects]).");
      /*
		  + ",[],[objects,assertions,regtypes,isomodes]).");
      */
      
      //      out.println(":- use_package(objects).");
    }

    out.println();

    while (it.hasNext()) {
      Object el = it.next();
      printExport(ReflectComparator.classRepresentation(el));
    }

  }

  /**
   * printImportation
   * Prints the use_class prolog declaration.
   */
  protected void printImportation(Class cl, Class curClass) {

    out.println(":- " + USE_CLASS + "(library('javaobs/"
		+ getFileName(cl.getName()) + "')).");

  }

  /**
   * printInterface
   * Prints one interface declaration of a given class.
   */
  protected void printInterface(Class curClass, Class intClass) {

    out.println(":- " + IMPLEMENTS + "(library('javaobs/"
		+ getFileName(intClass.getName()) + "')).");

  }

  /**
   * printDispose
   * Prints the destructor predicate, used to free the java object.
   */
  private void printDispose(Class curClass) {

    out.println("destructor :-");
    //    out.println(TAB + "java_instance(I),");
    //    out.println(TAB + "I:" + INTERNAL_JAVA_DESTRUCTOR + ".");
    out.println(TAB + INTERNAL_JAVA_DESTRUCTOR + ".");

  }

  /**
   * printField
   * Prints the predicate associated with a java field.
   */
  protected void printField(Field f) {

    out.println(getPrologName(f.getName()) + "(" + getPrologVar(0) + ") :-");
    Class[] types = {f.getType()};
    String varNames = printTypeTesting(types);
    //    out.println(TAB + "java_instance(I),");
    //    out.println(TAB + "I:" + GET_FIELD_METHOD
    out.println(TAB + GET_FIELD_METHOD
		 + "(" + getPrologName(f.getName())
		 + "(" + varNames + ")).");

  }

  /*
   * Printing of a java field (destructive) assertion.
   */
  protected void printFieldAssertion(Field f) {

    out.println("java_assert"
		+ varSequence(0, getPrologName(f.getName()) + "("
			      + getPrologVar(0) + ")")
		+ " :-");
    Class[] types = {f.getType()};
    String varNames = printTypeTesting(types);
    out.println(TAB + SET_FIELD_METHOD
                + varSequence(0, getPrologName(f.getName()) + "("
			      + varNames + ")")
                + ".");

  }

  /**
   * printConstructor
   * Prints the predicate associated with a java constructor.
   */
  protected void printConstructor(Constructor c, Class curClass) {
    Class param[] = c.getParameterTypes();

    out.println(getPrologName(c.getName()) + varSequence(param.length) + " :-");

    String varNames = printTypeTesting(param);

    out.print(TAB + "java_constructor('" + c.getName());
    if (varNames.length() > 0)
      out.println("'(" + varNames + ")).");
    else
      out.println("').");
		

    // Superclass constructor.
    //    out.print(TAB + "I new " + CONSTRUCTOR + "('" + c.getName());
    //    if (varNames.length() > 0)
    //      out.println("'(" + varNames + ")),");
    //    else
    //      out.println("'),");
      
    //    out.println(TAB + "set_fact(java_instance(I)).");
    out.println();
  }

  /**
   * printMethod
   * Prints the predicate associated with a java method.
   */
  protected void printMethod(Method m) {
    //@
    if (!Modifier.isAbstract(m.getModifiers())) {
      //@
      out.print(getPrologName(m.getName()));
      Class param[] = m.getParameterTypes();

      if (m.getReturnType().equals(Void.TYPE))
	out.println(varSequence(param.length) + " :-");
      else {
	out.println(varSequence(param.length, "Result") + " :-");
	out.println(TAB + "var(Result),");
      }

      // Type testing.
      String varNames = printTypeTesting(param);

      // Method invocation.
      //      out.println(TAB + "java_instance(I),");
      //      out.print(TAB + "I:" + INVOKE_METHOD + "(" + getPrologName(m.getName()));
      out.print(TAB + INVOKE_METHOD + "(" + getPrologName(m.getName()));
      if (m.getReturnType().equals(Void.TYPE))
	out.println(varSequence(varNames, 0, "_") + ").");
      else
	out.println(varSequence(varNames, 0, "Result") + ").");
      //@
    }
    //@

  }

  protected String printObjectTypeTesting(String var, Class cl) {

    String varName = getPrologVar(var,"OBJ");

    out.print(INTERFACE + "(" + var + ", "
	      + getPrologName(cl.getName()) + "), ");
    out.print(INTERFACE + "(" + var + ", "
	      + JAVA_BASE_OBJECT + "), ");
    out.print(var + ":" + GET_JAVA_ID + "(" + varName + ")");

    return varName;

  }
}

/**
 * *****************************************************************
 *                                                                 *
 * Class PrologModuleFile                                          *
 * Implementation of a PrologFile to generate prolog module files  *
 * for java interface.                                             *
 *                                                                 *
 * *****************************************************************
 */
class PrologModuleFile extends PrologFile {

  /*
   * Prolog constants.
   */
  // Methods declared on the prolog class java_obj.
  private static final String GET_FIELD_METHOD = "java_get_value";
  private static final String CONSTRUCTOR = "java_create_object";
  private static final String INVOKE_METHOD = "java_invoke_method";
  private static final String SET_FIELD_METHOD = "java_set_value";
  private static final String DESTRUCTOR = "java_delete_object";

  private static final String ADD_LISTENER = "java_add_listener";
  private static final String REMOVE_LISTENER = "java_remove_listener";

  private static final String JAVA_INSTANCE_OF = "java_instance_of";
  private static final String SET_JAVA_INSTANCE_OF = "set_java_instance_of";
  private static final String SET_JAVA_PARENTS = "set_java_parents";

  public PrologModuleFile(String title, String destdir) {
    super(title, destdir);
  }

  /**
   * genInheritance
   *
   * Printing of Inheritance information. If the java class inherits from
   * any other java class, then the prolog class inherits from the associated
   * prolog class/module too. Otherwise, the prolog class will inherit from 
   * 'java_obj' (only the 'Object' java class does not inherit from any other
   * class).
   * 
   * Parameters:
   *  curClass - class object representing the class being translated.
   */
  protected void genInheritance(Class curClass) {

    Class sClass = curClass.getSuperclass();

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Inheritance information.");
    out.println("%%--------------------------------------------------");
    out.println();
    if (sClass != null) {
      printImportation(sClass, curClass);
      out.println();
    }

  }

  /**
   * genMiscelanea
   * Miscelanea. This method generates the prolog code that
   * cannot be included in any other generation method, and
   * that depends on the prolog syntax used.
   */
  protected void genMiscelanea(Class curClass) {

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Miscelanea.");
    out.println("%%--------------------------------------------------");
    out.println();

    out.println(":- redefining(_).");
    out.println(":- set_prolog_flag(multi_arity_warnings,off).");
    out.println(":- set_prolog_flag(discontiguous_warnings,off).");
    out.println();
    out.println(":- data java_class_name/1.");
    out.println("java_class_name('" + curClass.getName() + "').");
    out.println();
    out.println(":- discontiguous java_assert/1.");
    out.println(":- use_module(library(lists)).");
    out.println();

    out.println(":- use_module('" + getRelBasePath(curClass) + JAVART + "').");
    out.println(":- use_module('" + getRelBasePath(curClass) + JAVA_INHERITANCE + "').");

    out.println();
    out.println("%%--------------------------------------------------");
    out.println("%% Destructor.");
    out.println("%%--------------------------------------------------");
    out.println();

    printDispose();
  }

  /**
   * genInterfaces
   * Prints one interface declaration of a given class.
   * Currently this is done at constructor time.
   */
  protected void genInterfaces(Class curClass) {

  }

  /**
   * getDeclElements
   * Gets the elements to be included in the prolog file 
   * as class/module declarations.
   * This method returns an iterator over all the elements
   * used by the class given as argument.
   */
  protected Iterator getDeclElements(Class curClass) {

    ArrayList el = new ArrayList();

    el.addAll(toList((Object[])curClass.getDeclaredConstructors()));
    el.addAll(toList((Object[])curClass.getFields()));
    el.addAll(toList((Object[])curClass.getDeclaredMethods()));

    for (int i = 0 ; i < el.size() ; i++)
      if (!ReflectComparator.isPublic(el.get(i)))
	el.remove(i);

    return el.iterator();
  }

  /**
   * getDeclElementSet
   * Gets the elements to be included in the prolog file 
   * as class/module declarations.
   * This method returns an ordered iterator over unique elements
   * in order to avoid prolog predicate repetition.
   */
  protected Iterator getDeclElementSet(Class curClass) {

    Iterator it = getDeclElements(curClass);
    ReflectComparator rc = new ReflectComparator();
    TreeSet ts = new TreeSet(rc);

    while (it.hasNext())
      ts.add(it.next());

    return ts.iterator();
  }

  /**
   * printDeclaration
   * Prints the prolog module declaration.
   */
  protected void printDeclaration(Class curClass, Iterator it) {

    String sName = getPrologName(curClass.getName());
    
    out.println(":- module(" + sName + ", []).");

    while (it.hasNext()) {
      printExport(ReflectComparator.moduleRepresentation(it.next()));
    }

  }

  /**
   * printImportation
   * Prints on the out stream the use_module declaration of
   * the prolog module whose name is given as argument.
   */
  protected void printImportation(Class module, Class curClass) {

    out.println(":- use_module('" + getRelFileName(module.getName(), curClass) + "').");

  }

  /**
   * printDispose
   * Prints the dispose predicate, used to free the java object.
   */
  private void printDispose() {

    out.println(":- export([java_dispose/1]).");
    out.println();
    out.println("java_dispose" + varSequence(1) + " :-");
    out.println(TAB + DESTRUCTOR + varSequence(1) + ".");

  }

  /**
   * printField
   * Prints the predicate associated with a java field.
   */
  protected void printField(Field f) {

    out.println(getPrologName(f.getName()) + varSequence(1,"Value") + " :-");
    Class[] types = {f.getType()};
    printTypeTesting(types);
    out.println(TAB + GET_FIELD_METHOD
                + varSequence(1, getPrologName(f.getName()) + "(Value)") + ".");

  }

  /*
   * Printing of a java field (destructive) assertion.
   */
  protected void printFieldAssertion(Field f) {

    out.println("java_assert"
              + varSequence(0, getPrologName(f.getName()) + "(Value)")
              + " :-");
    Class[] types = {f.getType()};
    printTypeTesting(types);
    out.println(TAB + SET_FIELD_METHOD
                + varSequence(1, getPrologName(f.getName()) + "(Value)")
                + ".");

  }

  /**
   * printConstructor
   * Prints the predicate associated with a java constructor.
   */
  protected void printConstructor(Constructor c, Class curClass) {

    out.print(getPrologName(c.getName()));
    Class param[] = c.getParameterTypes();
    out.println(varSequence(param.length, "Instance")
                + " :-");

    printTypeTesting(param);

    out.println(TAB + "var(Instance),");
    out.println(TAB + CONSTRUCTOR + "('" + c.getName() + "'"
		+ varSequence(param.length) + ", Instance),");

    // java instance_of setting.
    out.println(TAB + SET_JAVA_INSTANCE_OF + "("
		+ "Instance, "
		+ getPrologName(c.getName()) + "),");

    // java parents setting (superclass and interfaces).
    out.println(TAB + SET_JAVA_PARENTS + "("
		+ "Instance, [");

    Class sClass = curClass.getSuperclass();
    if (sClass != null)
      out.print(TAB + TAB + getPrologName(sClass.getName()));

    TreeSet intrs = getInterfaceSet(curClass);
    Iterator intri = intrs.iterator();
    while (intri.hasNext()) {
      out.println(",");
      out.print(TAB + TAB + getPrologName(((Class)intri.next()).getName()));
    }
    out.println();
    out.println(TAB + TAB + "]).");

  }

  /**
   * printMethod
   * Prints the predicate associated with a java method.
   */
  protected void printMethod(Method m) {
    out.print(getPrologName(m.getName()));
    Class param[] = m.getParameterTypes();

    if (m.getReturnType().equals(Void.TYPE))
      out.println(varSequence("Instance", param.length) + " :-");
    else {
      out.println(varSequence("Instance", param.length, "Result") + " :-");
      out.println(TAB + "nonvar(Instance),");
      out.println(TAB + "var(Result),");
    }

    // Type testing.
    printTypeTesting(param);

    // Method invocation.
    out.print(TAB + INVOKE_METHOD + "(Instance, " + getPrologName(m.getName()));
    if (m.getReturnType().equals(Void.TYPE))
      out.println(varSequence(param.length, "_") + ").");
    else
      out.println(varSequence(param.length, "Result") + ").");

  }

  protected String printObjectTypeTesting(String var, Class cl) {

    out.print(JAVA_INSTANCE_OF);
    out.print("(" + var + ", ");
    out.print(getPrologName(cl.getName()) + ")");

    return var;

  }

}






