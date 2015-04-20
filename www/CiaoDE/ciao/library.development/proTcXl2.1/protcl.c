/* 
 * ProTcl - A Prolog Interface to the Tcl/Tk toolkit.
 *
 *
 *	Copyright (c) 1995 Micha Meier
 *
 *******************************************************************/

#ifndef lint
static char     *SccsId   = "@(#)protcl.c	1.10        96/03/25";
#endif

/*#include <stdio.h>*/

#ifdef ECLIPSE
#include "external.h"
#undef EXTERN
static word32	d_eclipse,
		d_on,
		d_off,
		d_string,
		d_file;
word32		d_protcl_xlib_id;
static pword	*event_data = 0;
void		string_to_prolog(),
		untrail_cut_tcl();
#define	DidLength(D)	DidString(D)->val.nint
#endif

#ifdef SICSTUS
#include "sicstus.h"
#include <stdio.h>
int tk_event_hook();
char *event_data;                       /* Initialized to NULL in tk_init */
#endif

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>

int	    Tk_ExitPrologCmd _ANSI_ARGS_((ClientData clientData,
                                          Tcl_Interp *interp,
                                          int argc,
                                          char **argv));
extern int  Tcl_ExitTkCmd _ANSI_ARGS_((ClientData clientData,
                                       Tcl_Interp *interp,
                                       int argc,
                                       char **argv));
static void _FileCmdProc _ANSI_ARGS_((ClientData clientData, 
                                      int mask));

/*
 * Global variables used by the main program:
 */

extern Tcl_Interp *protcl_interp;	/* Interpreter for this application. */


   /* Even if call_prolog does nothing, we have a dummy definition for it */

static int call_prolog();                     

#ifdef ECLIPSE

/*
 * tk_clear_options
 */

p_tk_clear_options()
{
    tk_clear_options();
    Succeed;
}


/*
 * tk_option(Option, Value)
 */

p_tk_option(vo, to, vv, tv)
     value vo, vv;
     type to, tv;
{
  char	*o, *v;
  
  Get_Name(vo, to, o);
  Get_Name(vv, tv, v);
  tk_option(o, v);
  Succeed;
}

/*
 *	tk_init(File)
 *		Initialize Tk so that we obtain the functionality of wish
 */

p_tk_init(vf, tf)
     value vf;
     type tf;
{
  char	*file;
  
  Get_Name(vf, tf, file);
  d_eclipse = Did("eclipse", 0);
  d_on = Did("on", 0);
  d_off = Did("off", 0);
  d_file = Did("file", 0);
  d_protcl_xlib_id = Did("xlib_id", 2);
  d_string = Did("string", 1);
  Succeed_If(tk_init(file) == 0);
}

p_tk_do_one_event(v, t, vl, tl)
     value v, vl;
     type t, tl;
{
  Check_Integer(t)
    event_data = NULL;
  if (Tk_DoOneEvent((int) v.nint) == 0) {
    Fail
  }
  if (event_data) {
    Return_Unify_List(vl, tl, event_data->val.ptr)
  }
  Return_Unify_Nil(vl, tl)
}


/*
 *	tcl_eval_string(String, Res)
 *		Send the string to Tcl
 */

p_tcl_eval_string(vs, ts, vr, tr)
     value vs, vr;
     type  ts, tr;
{
  char	*cmd, *res;
  pword	p;
  pword	*arg;
  
  Get_Name(vs, ts, cmd);
  if (tcl_eval_string(cmd, &res) != 0) {
    Fail;
  }
  if (IsStructure(tr) && vr.ptr->val.did == d_string) {
    arg = vr.ptr + 1;
    Dereference(arg);
    string_to_prolog(protcl_interp, res, &p, 0, 0);
    Return_Unify_Pw(arg->val, arg->tag, p.val, p.tag);
  } else {
    string_to_prolog(protcl_interp, res, &p, 1, 0);
    Return_Unify_Pw(vr, tr, p.val, p.tag);
  }
}


p_tk_num_main_windows(v, t)
     value v;
     type t;
{
  Check_Output_Integer(t);
  Return_Unify_Integer(v, t, tk_NumMainWindows)
}


p_tk_fd_handler(vfd, tfd, vs, ts, v, t)
     value vfd, vs, v;
     type tfd, ts, t;
{
  Check_Atom(t);
  Check_Integer(tfd);
  Check_Integer(ts);
  if (v.did == d_on) {
    Tk_CreateFileHandler(vs.nint,
                         TK_READABLE,
                         _FileCmdProc, 
                         (ClientData) vs.nint);
  } else
    if (v.did == d_off) {
      Tk_DeleteFileHandler(vs.nint);
    } else {
      Bip_Error(RANGE_ERROR)
    }
  Succeed_;
}


p_tcl_cut_fail(v, t)
     value v;
     type t;
{
  char	*cmd;
  extern int schedule_cut_fail_action();
  
  Get_Name(v, t, cmd);	                              /* to check the arg */
  schedule_cut_fail_action(untrail_cut_tcl, v, t);
  Succeed_;
}

/*
 * tcl_interp(-Interp)
 *	Returns the current Tcl interpreter
 */

p_tcl_interp(vin, tin)
     value vin;
     type tin;
{
  if (protcl_interp) {
    Return_Unify_Integer(vin, tin, (long) protcl_interp)
  } else {
    Fail
  }
}


p_tcl_command_complete(vs, ts)
     value vs;
     type ts;
{
  char *cmd;
  
  Get_Name(vs, ts, cmd);
  Succeed_If(Tcl_CommandComplete(cmd) == 1);
}


/*
 * Convert the Tcl representation into a Prolog one.
 * Currently only vars, numbers and strings are recognized.
 */

void string_to_prolog(interp, s, p, rlist, vars)
     Tcl_Interp *interp;
     char *s;
     pword *p;
     int rlist;		                               /* recognize lists */
     int vars;		                           /* recognize variables */
{
  pword	*r;
  char	**argvl;
  char	*pt = s;
  int	argcl;
  int	i;
  int	res = TCL_ERROR;
  int	list = 0;
  extern char *string_to_number();
  
  if (rlist) {
    /* First look if it is a list. This means either it starts with a '{'
       or it contains at least two elements separated by a space */

    while (isspace(*pt))
      pt++;

    while (*pt && !isspace(*pt))
      pt++;

    if (isspace(*pt)) {
      while (isspace(*pt))
        pt++;
      if (*pt)
        list = 1;
    }

    if (list) {
      /* This works for {{}}, but not for {{} {}}, so it is
         better not to make it at all
         if (!strcmp(s, "{}")) {
         p->tag.kernel = TNIL;
         return;
         }
         */
      res = Tcl_SplitList(interp, s, &argcl, &argvl);
      if (res == TCL_OK && argcl > 0) {
        r = p;
        for (i = 0; i < argcl; i++) {
          r->val.ptr = TG;
          r->tag.kernel = TLIST;
          r = TG;
          TG += 2;
          Check_Gc;
          string_to_prolog(interp, argvl[i], r, rlist, 0);
          r++;
        }
        r->tag.kernel = TNIL;
        free(argvl);
        return;
      }
    }
  }
  if (*string_to_number(s, p, (stream_id) 0, 0) ||
      IsTag(p->tag.kernel, TEND))
    {
      if (vars && s) {
        /* we must parse it to know if it is only a variable or more */
        pt = s;
        while (isspace(*pt))
          pt++;
        if (isupper(*pt) || *pt == '_') {
          pt++;
          while (*pt && (isalnum(*pt) || *pt == '_'))
            pt++;
          if (*pt == '\0') {
            p->tag.all = TREF;
            p->val.ptr = p;
            return;
          }
        }
      }
      Cstring_To_Prolog(s, p->val);
      p->tag.all = TSTRG;
    }
}


static long empty = 0;

#define MALLOC_MAGIC	0x98765432


/* Return the (volatile) string form of the prolog term. */

char * prolog_to_string(p, ds)
     pword	*p;
     Tcl_DString	*ds;
{
  char	buf[32];
  pword	*r;
  char	*s;
  Tcl_DString dsl;
  extern char *Tcl_Merge();
  
  switch (Ptag(p->tag.kernel)) {
  case TINT:
    (void) sprintf(buf, "%d", p->val.nint);
    Tcl_DStringAppend(ds, buf, strlen(buf));
    break;
    
  case TFLOAT:
  case TDBL:
    (void) float_to_string(p->val, p->tag, buf, 0);
    Tcl_DStringAppend(ds, buf, strlen(buf));
    break;
    
  case TDICT:
    Tcl_DStringAppend(ds, DidName(p->val.did), DidLength(p->val.did));
    break;
    
  case TNIL:
    Tcl_DStringAppend(ds, "{}", 2);
    break;
    
  case TSTRG:
    Tcl_DStringAppend(ds, StringStart(p->val), StringLength(p->val));
    break;
    
  case TLIST:
    Tcl_DStringInit(&dsl);
    while (IsList(p->tag)) {
      r = p->val.ptr;
      p = r + 1;
      Dereference_(r);
      s = prolog_to_string(r, &dsl);
      if (s == NULL)
        return NULL;
      Tcl_DStringAppendElement(ds, s);
      Tcl_DStringFree(&dsl);
      Dereference_(p);
    }
    if (!IsNil(p->tag))
      return NULL;
    break;
    
  default:
    return NULL;
  }
  return Tcl_DStringValue(ds);
}
#endif

tcl_eval_string(cmd, out)
     char *cmd;
     char **out;
{
  int	res;
    
  *out = "";                   /* exit command or interrupted application */

  if (!protcl_interp) return 1;

  res = Tcl_Eval(protcl_interp, cmd);
  if (res != TCL_OK) {
    cmd = Tcl_GetVar(protcl_interp, "errorInfo", TCL_GLOBAL_ONLY);
    if (cmd == NULL) cmd = protcl_interp->result;
    printf("%s\n", cmd);
    return 1;
  }
  
  if (protcl_interp)  *out = protcl_interp->result;
  /* else *out = ""; */ /* Already done. MCL. */

  return 0;
}


tk_num_main_windows()
{
  return tk_NumMainWindows;
}



#if defined(ECLIPSE)/* MCL. I guess _FileCmdProc is only called from Eclipse */

/*
 *----------------------------------------------------------------------
 *
 * _FileCmdProc --
 *
 *	This procedure is invoked by the event dispatcher whenever
 *	input becomes readable.
 *
 *----------------------------------------------------------------------
 */

/* ARGSUSED */
static void _FileCmdProc(clientData, mask)
     ClientData clientData;		                    /* stream no. */
     int mask;				                     /* Not used. */
{
    register pword *p = TG; /*  */

    TG += 5;
    Check_Gc;
    p[0].val.ptr = p + 1;
    p[0].tag.all = TLIST;
    p[1].val.ptr = DidString(d_file);
    p[1].tag.all = TSTRG;
    p[2].val.ptr = p + 3;
    p[2].tag.all = TLIST;
    p[3].val.nint = (long) clientData;
    p[3].tag.all = TINT;
    p[4].tag.all = TNIL;
    event_data = p;
  }


/*#ifdef ECLIPSE        Moved upwards. MCL. */

/*
 * This command makes an interface between Tk events and handlers
 * written in Prolog. When specified as an event handler, it will
 * save the required data and return. Prolog is then able
 * to retrieve the stored data and invoke the appropriate
 * Prolog predicate.
 */
int Tk_PrologEventCmd(data, interp, argc, argv)
     ClientData data;			                     /* Not used. */
     Tcl_Interp *interp;			  /* Current interpreter. */
     int argc;				          /* Number of arguments. */
     char **argv;			             /* Argument strings. */
{
  register pword *p;
  pword	result;
  int i;

  event_data = p = TG++;
  Check_Gc
    for (i = 1; i < argc; i++) {
      p->val.ptr = TG;
      p->tag.kernel = TLIST;
      p = TG;
      TG += 2;
      Check_Gc
	string_to_prolog(interp, argv[i], p, 1, 0);
	p++;
    }
  p->tag.kernel = TNIL;
  return TCL_OK;
}

#endif                                                       /* !ECLIPSE  */

#if defined(SICSTUS)


 /* Changed by MCL. Returning only atoms. */

static char *no_event = "$Tk_DoOneEvent_Failed";
static char *no_prolog_event = "";                   /* Tcl empty list  */

char *tk_do_one_event_atom(mask)
     int mask;
{
  char *saved_event;
/*  event_data = NULL;*/
  if (!Tk_DoOneEvent(mask)) return no_event;      /* No event to return */
  if (event_data) {
    saved_event = event_data;
    event_data = NULL;
    return saved_event;
  } else return no_prolog_event;
}


/* 
   Called from tcl, leaves in event_data a pointer to the string
   representing the event passed from Tcl.
*/

int Tk_PrologEventCmd(data, interp, argc, argv)
     ClientData data;			                     /* Not used. */
     Tcl_Interp *interp;			  /* Current interpreter. */
     int argc;				          /* Number of arguments. */
     char **argv;			             /* Argument strings. */
{
  int i;

 /* The ECLIPSE interface returns a proper Prolog list. We, by now at least,
    are going to return the Tcl list as an atom. */

  if (argc > 1 && argc < 3){
    event_data = argv[1];
    return TCL_OK;
  } else return TCL_ERROR;
}



/*
 * Convert the Tcl representation into a Prolog one.
 * Currently only vars, numbers and strings are recognized.
 */

#if defined(ZERO)
static tagged string_to_prolog(interp, s, p, rlist, vars)
     Tcl_Interp *interp;
     char *s;
     pword *p;
     int rlist;		                               /* recognize lists */
     int vars;		                           /* recognize variables */
{
  pword	*r;
  char	**argvl;
  char	*pt = s;
  int	argcl;
  int	i;
  int	res = TCL_ERROR;
  int	list = 0;
  extern char *string_to_number();
  
  if (rlist) {
    /* First look if it is a list. This means either it starts with a '{'
       or it contains at least two elements separated by a space */

    while (isspace(*pt)) pt++;

    while (*pt && !isspace(*pt)) pt++;

    if (isspace(*pt)) {
      while (isspace(*pt))
        pt++;
      if (*pt)
        list = 1;
    }

    if (list) {
      /* This works for {{}}, but not for {{} {}}, so it is
         better not to make it at all
         if (!strcmp(s, "{}")) {
         p->tag.kernel = TNIL;
         return;
         }
         */
      res = Tcl_SplitList(interp, s, &argcl, &argvl);
      if (res == TCL_OK && argcl > 0) {
        tagged this_list, running_list;

        MakeLST(this_list,
                string_to_prolog(interp, argvl[0], rlist, 0),
                running_list);
        for (i = 1; i < argcl - 1; i++)
          MakeLST(running_list, 
                  string_to_prolog(interp, argvl[i], rlist, 0),
                  running_list);
        MakeLST(running_list, 
                string_to_prolog(interp, argvl[i], rlist, 0),
                atom_nil);
        free(argvl);
        return this_list;
      }
    }
  }                                                         /* Not a list */
  if (*string_to_number(s, p, (stream_id) 0, 0) ||
      IsTag(p->tag.kernel, TEND))
    {
      if (vars && s) {
        /* we must parse it to know if it is only a variable or more */
        pt = s;
        while (isspace(*pt))
          pt++;
        if (isupper(*pt) || *pt == '_') {
          pt++;
          while (*pt && (isalnum(*pt) || *pt == '_'))
            pt++;
          if (*pt == '\0') {
            p->tag.all = TREF;
            p->val.ptr = p;
            return;
          }
        }
      }
      Cstring_To_Prolog(s, p->val);
      p->tag.all = TSTRG;
    }
}
#endif                                                            /* ZERO */
#endif                                                         /* SICSTUS */

#if !defined(ECLIPSE) && !defined(SICSTUS)        /* !SICSTUS && !ECLIPSE */
int Tk_PrologEventCmd(data, interp, argc, argv)
     ClientData data;			                     /* Not used. */
     Tcl_Interp *interp;			  /* Current interpreter. */
     int argc;				          /* Number of arguments. */
     char **argv;			             /* Argument strings. */
{
  Tcl_AppendResult(interp,
                   "prolog_event is not defined for this Prolog system",
                   (char *) NULL);
  return TCL_ERROR;
}
#endif


#if defined(CALL_PROLOG)
int Tk_PrologCmd(data, interp, argc, argv)
     ClientData data;			                     /* Not used. */
     Tcl_Interp *interp;			  /* Current interpreter. */
     int argc;				          /* Number of arguments. */
     char **argv;			             /* Argument strings. */
{
  int res, argCount;
  char **argArray;
  
  if (argc < 2 || argc > 3) {
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
                     " pred [module]\"", (char *) NULL);
    return TCL_ERROR;
  }

  res = Tcl_SplitList(interp, argv[1], &argCount, &argArray);
  if (res != TCL_OK) return TCL_ERROR;

   /* This just returns success -- does nothing */
  res = call_prolog(interp, argCount, argArray, argc, argv);

  free(argArray);
  switch (res) {
  case 0:		/* PSUCCEED */
    interp->result = "success";
    return TCL_OK;
    
  case 1:		/* PFAIL */
    interp->result = "fail";
    return TCL_OK;
    
  case 2:		/* PTHROW */
    interp->result = "error";
    return TCL_OK;
    
  case -1:		/* conversion error */
    Tcl_AppendResult(interp, "error when converting data from Prolog to Tcl",
                     (char *) NULL);
    return TCL_ERROR;
  }
}
#endif                                                     /* CALL_PROLOG */

/*
 * Exit Tcl/Tk and Prolog
 */
/* ARGSUSED */

int Tk_ExitPrologCmd(dummy, intp, argc, argv)
     ClientData dummy;                                       /* Not used. */
     Tcl_Interp *intp;                            /* Current interpreter. */
     int argc;                                    /* Number of arguments. */
     char **argv;                                    /* Argument strings. */
{
  int v;
  
  if (argc == 1) 
    v = 0;
  else {
    if (Tcl_GetInt(intp, argv[1], &v) != TCL_OK) {
      return TCL_ERROR;
    }
  }
  if (Tcl_ExitTkCmd(dummy, intp, argc, argv) == TCL_ERROR)
    return TCL_ERROR;

#ifdef ECLIPSE
    {
      extern void exit_session();
      exit_session(v);
    }
#else
  exit(v);
#endif
}


#if defined(ECLIPSE) && defined(CALL_PROLOG)
static int call_prolog(interp, argCount, argArray, argc, argv)
     Tcl_Interp	*interp;
     char **argArray, **argv;
     int  argCount, argc;
{
  value	vmod, vg;
  pword	*p = TG;
  pword	*v;
  extern type tdict, tcomp;
  pword	result;
  Tcl_DString	ds;
  int i, j, res, new;
  char	**argvl;
  int argcl;
  
  vmod.did = (argc == 3) ? in_dict(argv[2], 0) : d_eclipse;
  vg.did = in_dict(argArray[0], argCount - 1);
  if (argCount == 1) {
    return sub_emulc_noexit(vg, tdict, vmod, tdict);
  } else {
    TG += argCount;
    v = TG;
    v[0].tag.all = TNIL;
    TG += argCount;
    Check_Gc;
    p[0].val.did = vg.did;
    p[0].tag.all = TDICT;
    for (i = 1; i < argCount; i++) {
      string_to_prolog(interp, argArray[i], p + i, 1, 1);
      if (IsRef(p[i].tag)) {
        v[i].tag.all = TINT;
        v[i].val.nint= (long) argArray[i];
        /* check for duplicates, not too efficient... */
        for (j = 0; j <= i; j++)
          if (IsRef(p[j].tag) && !strcmp(argArray[i], argArray[j])) {
            p[i].val.ptr = p[j].val.ptr;
            break;
          }
      }
      else
        v[i].tag.all = TNIL;
    }
    vg.ptr = p;
    res = sub_emulc_noexit(vg, tcomp, vmod, tdict);
    
    /* insert the vars into the array */
    Tcl_DStringInit(&ds);
    for (i = 1; i < argCount; i++) {
      if (IsInteger(v[i].tag)) {
        pword	*pt = p + i;
        char	*s;
        
        Dereference_(pt);
        if (res == PSUCCEED) {
          s = prolog_to_string(pt, &ds);
          if (s != NULL) {
            Tcl_SetVar2(interp, "var", (char *) v[i].val.nint, 
                        s, TCL_GLOBAL_ONLY);
            Tcl_DStringFree(&ds);
          } else {
            Tcl_DStringFree(&ds);
            if (IsRef(pt->tag)) {
              Tcl_UnsetVar2(interp, "var", (char *) v[i].val.nint,
                            TCL_GLOBAL_ONLY);
            } else
              return -1;
          }
        }
        else
          Tcl_UnsetVar2(interp, "var", (char *) v[i].val.nint, 
			TCL_GLOBAL_ONLY);
      }
    }
  }
  return res;
}

/* The function to be called on cut & failure to reset changes in tk */
void untrail_cut_tcl(v, t)
     value v;
     type  t;
{
  char	*cmd;
  
  if (IsString(t))
    cmd = StringStart(v);
  else if (IsAtom(t))
    cmd = DidName(v.did);
  if (protcl_interp)
    (void) Tcl_Eval(protcl_interp, cmd);
}

#endif                                          /* ECLIPSE && CALL_PROLOG */
                                
#if defined(SICSTUS) && defined(CALL_PROLOG)
#define MAXARGS		5

static int call_prolog(interp, argCount, argArray, argc, argv)
     Tcl_Interp	*interp;
     char **argArray, **argv;
     int  argCount, argc;
{
  SP_pred_ref pred;
  SP_term args[MAXARGS];
  SP_qid q;
  char *mod;
  int i = 0;
  
  mod = (argc == 3) ? argv[2] : "user";
  pred = SP_predicate(argArray[0], argCount - 1, mod);
  if (!pred) return 0;

  for (i = 1; i < argCount; i++) {
    SP_put_string(args + i - 1, argArray[i]);
  }
  /*
     q = SP_open_query_array(pred, args);
     if (q)
     i = SP_close_query(q);
     */
  i = SP_query_cut_fail(pred, args, args+1, args+2, args+3, args+4);
  return (i == 0) ? 1 : 0;
}
#endif                                          /* SICSTUS && CALL_PROLOG */

#if defined(SICSTUS)
int tk_event_hook(fd)
     int fd;
{
  if (tk_NumMainWindows > 0) {
    (void) Tk_DoOneEvent(0);
    return input_ready(fd);
  } else
    return 1;
}
#endif                                                         /* SICSTUS */


/*
#if !defined(CALL_PROLOG)
static int call_prolog(interp, argCount, argArray, argc, argv)
     Tcl_Interp *interp;
     char **argArray, argv;
     int argCount, argc;
{
  return 1;
}
#endif
*/

/*************************************************************
 local stuff
*************************************************************/

int input_ready(fd)
     int fd;
{
  fd_set dread;
  struct timeval to;
  struct timeval *pto = &to;
  int max = fd;
  
  FD_ZERO(&dread);
  FD_SET(fd, &dread);
  to.tv_sec = 0;
  to.tv_usec = 0;
  
  if (select(max + 1, &dread, (fd_set *) 0, (fd_set *) 0, pto) < 0)
    return -1;
  if (FD_ISSET(fd, &dread))
    return 1;
  else
    return 0;
}
