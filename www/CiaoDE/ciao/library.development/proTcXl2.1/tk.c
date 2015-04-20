/* 
 * ProTcl - A Prolog Interface to the Tcl/Tk toolkit.
 *
 * tk.c - modified from the source file TkMain.c
 *
 *	Modifications Copyright (c) 1994,1995 Micha Meier
 *
 *******************************************************************/

/* 
 * tkMain.c --
 *
 *	This file contains a generic main program for Tk-based applications.
 *	It can be used as-is for many applications, just by supplying a
 *	different Tcl_AppInit procedure for each specific application.
 *	Or, it can be used as a template for creating new main programs
 *	for Tk applications.
 *
 * Copyright (c) 1990-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef lint
static char sccsid[] = "@(#) tkMain.c 1.109 95/01/04 09:34:58";
static char     *SccsId   = "@(#)tk.c	1.17        95/09/14";
#endif

#ifdef SICSTUS
/*#include "Runtime/runtime.h"*/ /* For SICStus 3 */
#include "sicstus.h"                          /* Definitions for Ciao 0.0 */
extern int tk_event_hook();
#endif

#define ARGV0		"protcl"

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>

/*
 * Declarations for various library procedures and variables (don't want
 * to include tkInt.h or tkPort.h here, because people might copy this
 * file out of the Tk source directory to make their own modified versions).
 * Note: don't declare "exit" here even though a declaration is really
 * needed, because it will conflict with a declaration elsewhere on
 * some systems.
 */

extern int	isatty _ANSI_ARGS_((int fd));
extern int	read _ANSI_ARGS_((int fd, char *buf, size_t size));
extern char *	strrchr _ANSI_ARGS_((CONST char *string, int c));
extern char *   malloc _ANSI_ARGS_((unsigned int numBytes));


/*
 * Global variables used by the main program:
 */

Tk_Window protcl_mainWindow;	/* The main window for the application.  If
				 * NULL then the application no longer
				 * exists. */
Tcl_Interp *protcl_interp;	/* Interpreter for this application. */
static int tty;			/* Non-zero means standard input is a
				 * terminal-like device.  Zero means it's
				 * a file. */

/*
 * Command-line options:
 */

static int synchronize = 0;
static char *name = NULL;
static char *display = NULL;
static char *geometry = NULL;
static char *user_argv;
static char *file;
int nodisplay;

static Tk_ArgvInfo argTable[] = {
  {"-display", TK_ARGV_STRING, (char *) NULL, (char *) &display,
     "Display to use"},
  {"-geometry", TK_ARGV_STRING, (char *) NULL, (char *) &geometry,
     "Initial geometry for window"},
  {"-name", TK_ARGV_STRING, (char *) NULL, (char *) &name,
     "Name to use for application"},
  {"-sync", TK_ARGV_CONSTANT, (char *) 1, (char *) &synchronize,
     "Use synchronous mode for display server"},
  {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
     (char *) NULL}
};


extern int Tk_PrologCmd _ANSI_ARGS_((ClientData clientData,
                                     Tcl_Interp *interp,
                                     int argc,
                                     char **argv));
extern int Tk_PrologEventCmd _ANSI_ARGS_((ClientData clientData,
                                          Tcl_Interp *interp,
                                          int argc,
                                          char **argv));
extern int Tk_ExitCmd _ANSI_ARGS_((ClientData clientData,
                                   Tcl_Interp *interp,
                                   int argc,
                                   char **argv));
int        Tcl_ExitTkCmd _ANSI_ARGS_((ClientData clientData,
                                      Tcl_Interp *interp,
                                      int argc,
                                      char **argv));
extern int Tk_ExitPrologCmd _ANSI_ARGS_((ClientData clientData,
                                         Tcl_Interp *interp,
                                         int argc,
                                         char **argv));

/*
 * Declarations copied from other files to avoid including tk sources
 */

typedef struct TkMainInfo {
  int d1;
  struct TkWindow *winPtr;                     /* Pointer to main window. */
  Tcl_Interp *interp;         /* Interpreter associated with application. */
  Tcl_HashTable d2;
  Tk_BindingTable d3;
  char *d4;
  int d5;
  int d6;
  char *d7;
  unsigned long d8;
  char *d9;
  char *d10;
  Tcl_HashTable d11;
  int strictMotif;
  struct TkMainInfo *nextPtr; /* Next in list of all main windows managed by
                               * this process. */
} TkMainInfo;


extern TkMainInfo  *tkMainWindowList;
extern char* event_data;

tk_init(file)
     char *file;
{
  char *p, *class;
  int code;
  char *fileName;

  protcl_interp = Tcl_CreateInterp();

  fileName = file;
  if (!strcmp(fileName, ""))
    fileName = NULL;
  
  if (name == NULL) {
    if (fileName != NULL) 
      p = fileName;
    else
      p = ARGV0;

    name = strrchr(p, '/');
    if (name != NULL) 
      name++;
    else
      name = p;
  }

    /*
     * Make command-line arguments available in the Tcl variables "argc"
     * and "argv".
     */

  Tcl_SetVar(protcl_interp, "argv", user_argv, TCL_GLOBAL_ONLY);

  Tcl_SetVar(protcl_interp, 
             "argv0",
             (fileName != NULL) ? fileName : ARGV0,
             TCL_GLOBAL_ONLY);


   /* Erase any remaining Prolog event. */

  event_data = NULL;

  /*
   * If a display was specified, put it into the DISPLAY
   * environment variable so that it will be available for
   * any sub-processes created by us.
   */
  
  if (display != NULL) {
	Tcl_SetVar2(protcl_interp, "env", "DISPLAY", display, TCL_GLOBAL_ONLY);
    }

  /*
   * Initialize the Tk application.  If a -name option was provided,
   * use it;  otherwise, if a file name was provided, use the last
   * element of its path as the name of the application; otherwise
   * use the last element of the program name.  For the application's
   * class, capitalize the first letter of the name.
   */

  if (name == NULL) {
    p = (fileName != NULL) ? fileName : ARGV0;
    name = strrchr(p, '/');
    if (name != NULL) 
      name++;
    else
      name = p;
  }
  
  if (!nodisplay) {
    class = (char *) ckalloc((unsigned) (strlen(name) + 1));
    strcpy(class, name);
    class[0] = toupper((unsigned char) class[0]);
    protcl_mainWindow =
      Tk_CreateMainWindow(protcl_interp, display, name, class);
    ckfree(class);
    if (protcl_mainWindow == NULL) {
      fprintf(stderr, "%s\n", protcl_interp->result);
      exit(1);
    }
    if (synchronize) {
      XSynchronize(Tk_Display(protcl_mainWindow), True);
    }
    
    /*
     * Set the geometry of the main window, if requested.  Put the
     * requested geometry into the "geometry" variable.
     */

    if (geometry != NULL) {
      Tcl_SetVar(protcl_interp, "geometry", geometry, TCL_GLOBAL_ONLY);
      code = Tcl_VarEval(protcl_interp, 
                         "wm geometry . ",
                         geometry, 
                         (char *) NULL);
      if (code != TCL_OK) fprintf(stderr, "%s\n", protcl_interp->result);
    }
  }
  
  /*
   * Set the "tcl_interactive" variable.
   */
  
  tty = isatty(0);
  Tcl_SetVar(protcl_interp, "tcl_interactive",
             ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);

  /*
   * Initialize ProTcl commands
   */
  Tcl_CreateCommand(protcl_interp,
                    "exit_prolog",
                    Tk_ExitPrologCmd,
                    (ClientData) NULL,
                    (void (*)()) NULL);
  Tcl_CreateCommand(protcl_interp,
                    "exit",
                    Tcl_ExitTkCmd,
                    (ClientData) NULL,
                    (void (*)()) NULL);
  
#if defined(CALL_PROLOG)
  Tcl_CreateCommand(protcl_interp,
                    "prolog",
                    Tk_PrologCmd,
                    (ClientData) NULL,
                    (void (*)()) NULL);
#endif                                                     /* CALL_PROLOG */
  
  Tcl_CreateCommand(protcl_interp,
                    "prolog_event",
                    Tk_PrologEventCmd,
                    (ClientData) NULL,
                    (void (*)()) NULL);

  /*
   * Invoke application-specific initialization.
     */
  
  if (Tcl_AppInit(protcl_interp) != TCL_OK)
	fprintf(stderr, "Tcl_AppInit failed: %s\n", protcl_interp->result);

  /*
   * If there is no file to execute, set a handler to wait
   * for input on the toplevel input stream.
   */
  
  if (fileName == NULL) {
    /*
     * 
     * Evaluate the .rc file, if one has been specified.
     */
    if (tcl_RcFileName != NULL) {
      Tcl_DString buffer;
      char *fullName;
      FILE *f;
    
      fullName = Tcl_TildeSubst(protcl_interp, tcl_RcFileName, &buffer);
      if (fullName == NULL) 
        fprintf(stderr, "%s\n", protcl_interp->result);
      else {
        f = fopen(fullName, "r");
        if (f != NULL) {
          code = Tcl_EvalFile(protcl_interp, fullName);
          if (code != TCL_OK)
            fprintf(stderr, "%s\n", protcl_interp->result);
          fclose(f);
        }
      }
      Tcl_DStringFree(&buffer);
    }
  }
  fflush(stdout);
  Tcl_ResetResult(protcl_interp);
  
#ifdef SICSTUS
  SP_read_hook = tk_event_hook;
#endif
  return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ExitTkCmd --
 *
 *      This procedure is invoked to process the "exit" Tcl command.
 *      Unlike the original Tcl command, this one only exits Tk, but
 *	does not return back to Unix.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      We have a problem here, because Tcl does not assume that
 *	exit may actually do something else than stop the process
 *	and so undefined things happen when it returns. For instance,
 *	when sourcing a file, then calling 'exit' will not prevent
 *	it from reading the next command.
 *
 *----------------------------------------------------------------------
 */

        /* ARGSUSED */
int Tcl_ExitTkCmd(dummy, intp, argc, argv)
     ClientData dummy;                                       /* Not used. */
     Tcl_Interp *intp;                            /* Current interpreter. */
     int argc;                                    /* Number of arguments. */
     char **argv;                                    /* Argument strings. */
{
  if ((argc != 1) && (argc != 2)) {
    Tcl_AppendResult(intp,
                     "wrong # args: should be \"",
                     argv[0],
                     " ?returnCode?\"",
                     (char *) NULL);
    return TCL_ERROR;
  }

  while (tkMainWindowList != NULL)
    Tk_DestroyWindow((Tk_Window) tkMainWindowList->winPtr);

  Tcl_DeleteInterp(intp);
  protcl_interp = NULL;
  return 0;
}


/*
 * Functions to collect command-line options. We use a simplified interface
 * so that quintus etc. can have them as well. They are in this file
 * to keep the variables static.
 */


void tk_clear_options()
{
  synchronize = 0;
  nodisplay = 0;
  name = NULL;
  display = NULL;
  geometry = NULL;
  user_argv = "";
  file = "";
}


void tk_option(atom, string)
     char *atom;
     char *string;
{
  if (!strcmp(atom, "geometry")) geometry = string;
  else if (!strcmp(atom, "nodisplay")) nodisplay = 1;
  else if (!strcmp(atom, "display")) display = string;
  else if (!strcmp(atom, "name")) name = string;
  else if (!strcmp(atom, "argv")) user_argv = string;
  else if (!strcmp(atom, "file")) file = string;
  else if (!strcmp(atom, "sync")) synchronize = 1;
}

