#include "ciao_prolog.h"

#include <unistd.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/stat.h>
#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>

#if defined(__svr4__) || defined(DARWIN)             /* Solaris or DARWIN */
#include <unistd.h>                                            /* sbrk () */
#include <stdlib.h>                                           /* malloc() */
#else                                                            /* SunOS */
#include <sys/types.h>
#include <malloc.h>
#endif
#include <sys/param.h>

#include "datadefs.h"
#include "support.h"
#if defined(MALLOC_DEBUG)
#include "dmalloc.h"
#endif

#if !defined(X_OK)
# define X_OK 1
#endif

#include "debug.h"
#include "initial.h"
#include "threads.h"
#include "task_areas.h"

#include "wam_defs.h"
#include "tasks_defs.h"
#include "unix_utils_defs.h"
#include "initial_defs.h"
#include "inout_defs.h"
#include "start_defs.h"
#include "qread_defs.h"
#include "builtin_defs.h"
#include "own_malloc_defs.h"
#include "alloc_defs.h"
#include "support_defs.h"
#include "locks_defs.h"
#include "timing_defs.h"
#include "profile_defs.h"
#include "startgoal_defs.h"
#include "nondet_defs.h"
#include "prolog_tasks_defs.h"
#include "term_support_defs.h"
#include "bignum_defs.h"
#include "stacks_defs.h"
#if defined(DEBUG)
#include "locks_defs.h"
#endif
#include "wam.h"
#include "compat.h"
#include "instrdefs.h"

/*
#if defined(DEBUG)
int debug_c = 0;
#endif
*/

#if !defined(MAXPATHLEN)
# define MAXPATHLEN 1024
#endif

#if defined(DARWIN)
#include <stdlib.h>
#else
#include <malloc.h>
#endif
#include <string.h>

void ciao_at_exit(int result);

/*---------------------------------------------------------------------------*/

ciao_state ciao_implicit_state;

/*---------------------------------------------------------------------------*/

extern char source_path[];
extern int prolog_force_interactive;

extern char *library_directory;

/* Memory management routines --- now only interfaces to library, but they
   might evolve in access to a custom memory management library */

void *ciao_malloc(int size) {
  return malloc(size);
}

void ciao_free(void *pointer){
  free(pointer);
}


/* Low level term operations */

ciao_term ciao_ref(ciao_state state, TAGGED x);
TAGGED ciao_unref(ciao_state state, ciao_term term);

struct _ciao_query {
  ciao_state state;
  ciao_choice base_choice;
};

/* ---------------------------------------------------------------------------*/ 

void ciao_ensure_heap(ciao_state state, int cells) {
  struct worker *w;
  w = state->worker_registers;
  if (HeapDifference(w->global_top,Heap_End) < CONTPAD + cells)
    explicit_heap_overflow(w, CONTPAD + cells, 0);
}

/* --------------------------------------------------------------------------- */ 

ciao_bool ciao_is_char_code_list(ciao_state state, ciao_term term) {
  TAGGED cdr, car;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!TagIsSmall(car) || (car<TaggedZero) || (car>=MakeSmall(256))) break;
    DerefCdr(cdr,cdr);
  }
  return cdr == atom_nil;
}

int ciao_is_int_list(ciao_state state, ciao_term term) {
  TAGGED cdr, car;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!IsInteger(car)) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? 1 : 0;
}

int ciao_is_double_list(ciao_state state, ciao_term term) {
  TAGGED cdr, car;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!IsNumber(car)) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? 1 : 0;
}

int ciao_list_length(ciao_state state, ciao_term term) {
  TAGGED cdr;
  int len;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  for (len=0; cdr!=atom_nil; len++) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? len : (-1);
}

#define TEMPLATE(Name, X, XC) \
void Name(ciao_state state, ciao_term list, int length, X *array) { \
  int i; \
  TAGGED car, cdr; \
  cdr = ciao_unref(state, list); \
  DEREF(cdr, cdr); \
  for (i = 0; i < length; i++) { \
    DerefCar(car,cdr); \
    array[i] = XC(car); \
    DerefCdr(cdr,cdr); \
  } \
}
TEMPLATE(ciao_list_to_byte_array_l, char, GetSmall)
TEMPLATE(ciao_list_to_int_array_l, int, GetInteger)
TEMPLATE(ciao_list_to_double_array_l, double, GetFloat)
#undef TEMPLATE

#define TEMPLATE(Name, X, NameL) \
X *Name(ciao_state state, ciao_term list) { \
  X *array; \
  int length; \
  length = ciao_list_length(state, list); \
  if (length == 0) return NULL; /* sure? */ \
  array = (X *)ciao_malloc(sizeof(X) * length); \
  NameL(state, list, length, array); \
  return array; \
}
TEMPLATE(ciao_list_to_byte_array, char, ciao_list_to_byte_array_l)
TEMPLATE(ciao_list_to_int_array, int, ciao_list_to_int_array_l)
TEMPLATE(ciao_list_to_double_array, double, ciao_list_to_double_array_l)
#undef TEMPLATE

char *ciao_list_to_str(ciao_state state, ciao_term list) {
  char *string;
  int length;
  length = ciao_list_length(state, list);
  string = (char *)ciao_malloc(sizeof(char) * (length + 1));
  ciao_list_to_byte_array_l(state, list, length, string);
  string[length] = 0;
  return string;
}

#define TEMPLATE(Name, X, XC, XS) \
ciao_term Name(ciao_state state, X *s, int length) { \
  struct worker *w; \
  int i; \
  TAGGED cdr; \
  w = state->worker_registers; \
  ciao_ensure_heap(state, length * XS); \
  cdr = atom_nil; \
  s += length; \
  for (i = 0; i < length; i++) { \
    s--; \
    MakeLST(cdr, XC, cdr); \
  } \
  return ciao_ref(state, cdr); \
}
TEMPLATE(ciao_byte_listn, const char, MakeSmall(*s), 2)
TEMPLATE(ciao_int_listn, int, MakeInteger(w, *s), 4)
TEMPLATE(ciao_double_listn, double, MakeFloat(w, *s), 8)
#undef TEMPLATE

ciao_term ciao_str_to_list(ciao_state state, const char *string) {
  int length;
  length = strlen(string);
  return ciao_byte_listn(state, string, length);
}

ciao_term ciao_pointer_to_address(ciao_state state, void *pointer) {
  return ciao_structure_s(state, "$address", 1, ciao_integer_s(state, (int)pointer));
}

void *ciao_address_to_pointer(ciao_state state, ciao_term term) {
  return (void *)ciao_to_integer_s(state, ciao_structure_arg_s(state, term, 1));
}

ciao_bool ciao_is_address(ciao_state state, ciao_term term) {
  return (ciao_is_structure_s(state, term) && strcmp(ciao_structure_name_s(state, term), "$address") == 0 && ciao_structure_arity_s(state, term) == 1);
}

/* --------------------------------------------------------------------------- */
/* Initialization */

BOOL quiet_flag_bool;

/* Parse options before initialization */

int ciao_opts(const char *program_name, int programc, const char **programv, int optc, const char **optv, const char **p_raw_source_path) {
  BOOL quiet = FALSE;
  int i;
  char *lc_ctype;
  char *raw_source_path = NULL;

  init_locks();                                  /* global, first of all! */

#if defined(DEBUG)
  RESET_COUNTER;
#endif

  lc_ctype = getenv("LC_CTYPE");
  if (lc_ctype!=NULL &&
    (strcmp(lc_ctype,"ja_JP.EUC")==SAME || strcmp(lc_ctype,"ja_JP.euc")==SAME))
    init_kanji();
  else
    init_latin1();

  compute_cwd();

  prolog_argc = programc;
  prolog_argv = (char **)programv;

  for(i = 0; i < optc; i++) 
    if (strcmp(optv[i],"-b") == SAME)
      raw_source_path = (char *)optv[++i];
    else if (strcmp(optv[i],"-i") == SAME)
      prolog_force_interactive = 1;
    else if (strcmp(optv[i],"-q") == SAME)             /* To make quiet */
      quiet = TRUE;
    else if (strcmp(optv[i],"-v") == SAME)           /* To make verbose */
      quiet = FALSE;
    else
#if defined(PROFILE)
      if (strcmp(optv[i], "-prof") == SAME)        /* Simple profile */
	profile = TRUE;
      else if (strcmp(optv[i], "-proft") == SAME)         /* Include time */
	{profile = TRUE; prof_include_time = TRUE;}
      else
#endif
	if (strcmp(optv[i], "-tp") == SAME)        /* Trace predicates */
	  predtrace = TRUE;
#if defined(DBG) || defined(DEBUG)
      else if (strcmp(optv[i], "-dcp") == SAME)  /*debug regular choicepoints*/
        debug_choicepoints = TRUE;
      else if (strcmp(optv[i], "-dconccp") == SAME) /*conc. choicepoints*/
        debug_concchoicepoints = TRUE;
	else if (strcmp(optv[i], "-dt") == SAME)           /* debug threads */
	  debug_threads = TRUE;
	else if (strcmp(optv[i], "-dgc") == SAME)      /* debug garb. coll. */
	  debug_gc = TRUE;
	else if (strcmp(optv[i], "-dmem") == SAME)       /* debug mem. man. */
	  debug_mem = TRUE;
	else if (strcmp(optv[i], "-dconc") == SAME)    /* debug concurrency */
	  debug_conc = TRUE;
#endif
#if defined(DEBUG)                         /* Pack here debugging options */
	else if (strcmp(optv[i],"-d") == SAME)
	  debug_c = 1;
#endif
	else if (strcmp(optv[i], "-C") != SAME)  /* Ignore other "-C" */
	  fprintf(stderr,"Warning: %s ignored\n",optv[i]);

#if defined(PROFILE)
  if (profile||predtrace) stop_on_pred_calls = TRUE;
#else
  if (predtrace) stop_on_pred_calls = TRUE;
#endif

/* Find out the library_directory --- we need it before using '$' anywhere */

  if (!(library_directory = getenv("CIAOLIB")))
#if defined(Win32)
    {
      HKEY SOFTWAREKey, CiaoPrologKey;
      DWORD buffer_size = MAXPATHLEN;
 
      library_directory = (char *)checkalloc(MAXPATHLEN+1);
     
      if (( RegOpenKeyEx(HKEY_LOCAL_MACHINE, TEXT("SOFTWARE"), 0, KEY_READ,
			 &SOFTWAREKey) == ERROR_SUCCESS ) &&
	  ( RegOpenKeyEx(SOFTWAREKey, TEXT("Ciao Prolog"), 0, KEY_READ,
			 &CiaoPrologKey) == ERROR_SUCCESS ) &&
	  ( RegQueryValueEx(CiaoPrologKey, TEXT("ciao_dir"), NULL, NULL,
		           library_directory, &buffer_size) == ERROR_SUCCESS ))
	{
	  RegCloseKey(SOFTWAREKey);
	  RegCloseKey(CiaoPrologKey);
	} else {
	  fprintf(stderr,
                 "Registry key not found. Remember to install Ciao Prolog!\n");
	  ciao_at_exit(1); 
	}
    }
#else
    library_directory = installibdir;
#endif

#if defined(Win32)
/* #define SUBDIR_WINDOWS_BIN "/bin/Win32i86" */
    {
      extern char *emulator_os;
      extern char *emulator_architecture;

      /* Now, we adjust a couple of things to be used inside Windows;
         outstandingly, the PATH and the presence of the SHELL variable.
         The PATH should include the Win32/bin directory under the Windows
         distribution; inside it there is a sh.exe which we assign to the
         SHELL variable if it has not been already set by the user.  */
    
    /* Construct the Win32/bin directory */

      char *temp_path = (char *)checkalloc(MAXPATHLEN+1);
      char *current_path;
      char *current_path_local;
      
      strcpy(temp_path, library_directory);
      strcat(temp_path, "bin/");
      strcat(temp_path, emulator_os);
      strcat(temp_path, emulator_architecture);

       /* Is it already in the PATH? */
      if (!(current_path = getenv("PATH")) ||               /* No path or */
          !strstr(current_path, temp_path)) {      /* does not contain it */
         /* Add to $PATH at the end */
        if (current_path == NULL)
          current_path_local = temp_path;
        else {                           /* Do not alter the env. itself! */
          current_path_local = 
            (char *)checkalloc(strlen(current_path) + MAXPATHLEN + 2);
          strcpy(current_path_local, current_path);
          strcat(current_path_local, ":");
          strcat(current_path_local, temp_path);
        }
        setenv("PATH", current_path_local, 1);
      }

      /* Check now if the SHELL variable has been defined --- the
         shell/{0,3} call depends on it. */
      if (!getenv("SHELL")){
        strcat(temp_path, "/sh.exe");  /* CygWin shell */
        setenv("SHELL", temp_path, 1);
      }
    }
#endif

  quiet_flag_bool = quiet;
  if (p_raw_source_path != NULL) *p_raw_source_path = raw_source_path;

  return 0;
}

/* Initialization */

INSN *call_code;
INSN *default_code;
INSN *null_code;
struct try_node nullgoal_alt;
struct try_node defaultgoal_alt;
struct try_node startgoal_alt;

void ciao_initcode()
{
  INSN *b;

  call_code = (INSN *)checkalloc(32);
  null_code = (INSN *)checkalloc(32);
  default_code = (INSN *)checkalloc(32);

  b = call_code;
  *b++ = CALL;
  *(struct definition **)b = address_call;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(TAGGED);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;

  b = null_code;
  *b++ = EXIT_TOPLEVEL;

  b = default_code;
  *b++ = EXIT_TOPLEVEL;

  startgoal_alt.node_offset = ArityToOffset(1);
  startgoal_alt.number = 0;
  startgoal_alt.emul_p = call_code;
  startgoal_alt.emul_p2 = call_code;
  startgoal_alt.next = NULL;

  defaultgoal_alt.node_offset = ArityToOffset(0);
  defaultgoal_alt.number = 0;
  defaultgoal_alt.emul_p = default_code;
  defaultgoal_alt.emul_p2 = default_code;
  defaultgoal_alt.next = &nullgoal_alt; 

  nullgoal_alt.node_offset = ArityToOffset(0);
  nullgoal_alt.number = 0;
  nullgoal_alt.emul_p = null_code;
  nullgoal_alt.emul_p2 = null_code;
  nullgoal_alt.next = &nullgoal_alt; /* loop forever */
}

void ciao_init() 
{
  /* Global initializations */
  checkasserts();
#if defined(USE_OWN_MALLOC)
  init_own_malloc();
#endif
  /*init_wrb_state_list();*/
  init_goal_desc_list();
  init_once();
  init_alloc();
  current_quiet_flag = quiet_flag_bool ? atom_on : atom_off;
  /*mem_prog_count = 0;*/

  ciao_initcode();

  glb_init_each_time();
}

/* Reinitialization */

void ciao_reinit() 
{
  glb_init_each_time();
}

/* --------------------------------------------------------------------------- */
/* WAM creation */

ciao_state ciao_state_new() {
  static int first = 1;
  if (first) {
    return init_first_gd_entry();
  } else {
    return gimme_a_new_gd();
  }
}

void ciao_state_free(ciao_state state) {
  if ((state->state != PENDING_SOLS) &&
      (state->state != FAILED))
    return; /* Trying to release a worker either working or without assigned work */

  make_goal_desc_free(state);
}

/* --------------------------------------------------------------------------- */
/* WAM operations */

void ciao_load_ql_files(ciao_state state, FILE *qfile) {
  struct worker *w;
  int more_ql;

  w = state->worker_registers;
  
  push_qlinfo(NULL);

  more_ql = qread1(w,qfile,&X(0));	            /* ignore version no. */
  w->global_top = w->node->global_top;              /* Reset heap pointer */
  
  while (more_ql)
    while ((more_ql = qread1(w,qfile,&X(0))) && run_determ_c(w,X(0)))
      w->global_top = w->node->global_top;
  
  pop_qlinfo(NULL);
}

/* Load a qfile */

FILE *ciao_open_qfile(const char *raw_source_path);

void ciao_load_qfile_s(ciao_state state, const char *raw_source_path) {
  FILE *qfile;
  qfile = ciao_open_qfile(raw_source_path);
  ciao_load_ql_files(state, qfile);
  fclose(qfile);
}

void ciao_load_qfile(const char *raw_source_path) {
  ciao_load_qfile_s(ciao_implicit_state, raw_source_path);
}

FILE *ciao_open_qfile(const char *raw_source_path) {
  FILE *qfile = NULL;
#if defined(Win32)
  int i;
#endif

  expand_file_name((char *)raw_source_path,(char *)source_path);
#if defined(Win32)
  i = strlen(source_path)-4;
  if (i > 0 && strcmp(source_path+i,".bat") == SAME){
    source_path[i+1] = 'c';
    source_path[i+2] = 'p';
    source_path[i+3] = 'x';
  } else if (i > 0 && strcmp(source_path+i,".cpx") != SAME)
    strcat(source_path,".cpx");

  if (access(source_path,R_OK))
    source_path[strlen(source_path)-4] = '\0'; /* Take out .cpx */
#endif
  if (qfile == NULL) qfile = fopen(source_path,"r");
  if (qfile == NULL) {
    fprintf(stderr, "%s: boot file not found\n", source_path);
    ciao_at_exit(1);
    return NULL;
  } else { /* We have a bootfile we can read from */
    return qfile;
  }
}

/* Load a qfile embedded in an executable */

FILE *ciao_open_embedded_qfile(const char *program_name);
void ciao_open_emulator(const char *file, FILE **stream, const char *p);

void ciao_load_embedded_qfile_s(ciao_state state, const char *program_name) {
  FILE *qfile;
  qfile = ciao_open_embedded_qfile(program_name);
  ciao_load_ql_files(state, qfile);
  fclose(qfile);
}

void ciao_load_embedded_qfile(const char *program_name) {
  ciao_load_embedded_qfile_s(ciao_implicit_state, program_name);
}

FILE *ciao_open_embedded_qfile(const char *program_name) {
  FILE *qfile = NULL;

  const char *p;

  for (p=program_name; *p && *p != '/'; p++);
  if (*p != '/') ciao_open_emulator(program_name,&qfile,getenv("PATH"));
  if (qfile == NULL) ciao_open_emulator(program_name,&qfile,".");
  if (qfile == NULL) {
    fprintf(stderr,"%s: file not found\n", program_name);
    ciao_at_exit(1);
  }
  return qfile;
}

void ciao_open_emulator(const char *file, FILE **stream, const char *p) {}

/*

#define USAGE_STRING "Usage: %s [prolog_args] -C [-i] [-q] [-v] -b bootfile\n"

char ciao_emulatorlength[] = "This emulator executable has a size of        ";

void ciao_open_emulator(char *file, FILE **stream, char *p)
{
  char path[MAXPATHLEN+1];
  
  while (*p) {
    REGISTER char *next = path;
      
    while (*p && *p != ':') *next++ = *p++;
    *next++ = '/';
    *next++ = 0;
    if (*p) p++;
    if (!strcmp(path, "./")) path[0] = 0;
    strcat(path, file);

    if (!access(path, X_OK)) {
      struct stat data;
      stat(path,&data);
      if (data.st_size == atoi(&ciao_emulatorlength[38])) {
	fprintf(stderr, USAGE_STRING, file);
	ciao_at_exit(1);
      }
      if ((*stream = fopen(path,"r")) == NULL) {
	fprintf(stderr,"%s: unable to open for read\n", file);
	ciao_at_exit(1);
      }
      fseek(*stream,atoi(&ciao_emulatorlength[38]),SEEK_SET);
      return;
    }
  }
  *stream = NULL;
}
*/

/* --------------------------------------------------------------------------- */
/* Term creation */

ciao_term ciao_var_s(ciao_state state) {
  TAGGED *pt;
  TAGGED to;
  struct worker *w;
  ciao_ensure_heap(state, 1);
  w = state->worker_registers;
  pt = w->global_top;
  HeapPush(pt, to = TagHVA(pt));
  w->global_top = pt;  
  return ciao_ref(state, to);
}

ciao_term ciao_var() {
  return ciao_var_s(ciao_implicit_state);
}

ciao_term ciao_structure_a_s(ciao_state state, const char *name, int arity, ciao_term *args) {
  struct worker *w;
  w = state->worker_registers;
  if (arity == 0) {
    return ciao_ref(state, MakeString((char *)name));
  } else if (strcmp(name, ".") == 0 && arity == 2) {
    TAGGED list;
    ciao_ensure_heap(state, 3);
    MakeLST(list, ciao_unref(state, args[0]), ciao_unref(state, args[1]));
    return ciao_ref(state, list);
  } else {
    int i;
    TAGGED *pt;
    TAGGED functor;
    ciao_ensure_heap(state, 2 + arity);
    functor = SetArity(MakeString((char *)name), arity);
    pt = w->global_top;
    HeapPush(pt, functor);
    for (i = 0; i < arity; i++) {
      HeapPush(pt, ciao_unref(state, args[i]));
    }
    w->global_top = pt;  
    return ciao_ref(state, Tag(STR, HeapOffset(pt, -(arity+1))));
  }
}

ciao_term ciao_structure_a(const char *name, int arity, ciao_term *args) {
  return ciao_structure_a_s(ciao_implicit_state, name, arity, args);
}

ciao_term ciao_integer_s(ciao_state state, int i) {
  struct worker *w;
  ciao_ensure_heap(state, 4);
  w = state->worker_registers;
  return ciao_ref(state, MakeInteger(w, i));
}

ciao_term ciao_integer(int i) {
  return ciao_integer_s(ciao_implicit_state, i);
}

ciao_term ciao_float_s(ciao_state state, double f) {
  struct worker *w;
  ciao_ensure_heap(state, 4);
  w = state->worker_registers;
  return ciao_ref(state, MakeFloat(w, f));
}

ciao_term ciao_float(double f) {
  return ciao_float_s(ciao_implicit_state, f);
}

ciao_bool ciao_is_integer_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsInteger(t);
}

ciao_bool ciao_is_integer(ciao_term term) {
  return ciao_is_integer_s(ciao_implicit_state, term);
}

ciao_bool ciao_fits_in_int_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  /* This relies on lazy evaluation. Bignums are arrays of integers, and are
     always (explicitly) canonized after every evaluation.  If more than one
     word is needed, then it does not fit into an integer. */
  /*  if (IsFloat(t)) printf("IsFloat\n"); else printf("Not IsFloat\n"); */

  return TagIsSmall(t) || (IsInteger(t) && (bn_length(TagToSTR(t)) == 1));
}

ciao_bool ciao_fits_in_int(ciao_term term) {
  return ciao_fits_in_int_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_variable_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsVar(t);
}

ciao_bool ciao_is_variable(ciao_term term) {
  return ciao_is_variable_s(ciao_implicit_state, term);
}

int ciao_to_integer_s(ciao_state state, ciao_term term) {
  /* PRECONDITION: ciao_is_integer(state, term) */
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return GetInteger(t);
}

int ciao_to_integer(ciao_term term) {
  return ciao_to_integer_s(ciao_implicit_state, term);
}

ciao_bool ciao_to_integer_check_s(ciao_state state, ciao_term term, int *res) {
  if (ciao_fits_in_int_s(state, term)) {
    *res = ciao_to_integer_s(state, term);
    return TRUE;
  } else return FALSE;
}

ciao_bool ciao_to_integer_check(ciao_term term, int *res) {
  return ciao_to_integer_check_s(ciao_implicit_state, term, res);
}

ciao_bool ciao_is_number_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsNumber(t);
}

ciao_bool ciao_is_number(ciao_term term) {
  return ciao_is_number_s(ciao_implicit_state, term);
}

double ciao_to_float_s(ciao_state state, ciao_term term) {
  /* PRECONDITION: ciao_is_number(state, term) */
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return GetFloat(t);
}

double ciao_to_float(ciao_term term) {
  return ciao_to_float_s(ciao_implicit_state, term);
}

char *ciao_get_number_chars_s(ciao_state state, ciao_term term) {
  TAGGED number;
  char *number_result;
  Argdecl = state->worker_registers;

  number = ciao_unref(state, term);
  /* number_to_string() handles al kinds of numbers; it leaves the result in
     Atom_Buffer */
  number_to_string(Arg, number, GetSmall(current_radix));
  number_result = ciao_malloc(strlen(Atom_Buffer) + 1);
  strcpy(number_result, Atom_Buffer);
  return number_result;
}

char *ciao_get_number_chars(ciao_term term) {
  return ciao_get_number_chars_s(ciao_implicit_state, term);
}

/* PRECONDITION: number_result should really represent a number */
/* TO DO: raise a proper exception */
ciao_term ciao_put_number_chars_s(ciao_state state, char *number_string) {
  TAGGED result;
  (void)string_to_number( state->worker_registers, 
                          number_string,
			  GetSmall(current_radix),
                          &result,
			  0);
  return ciao_ref(state, result);
}

ciao_term ciao_put_number_chars(char *number_string) {
  return ciao_put_number_chars_s(ciao_implicit_state, number_string);
}


ciao_bool ciao_is_atom_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsAtom(t);
}

ciao_bool ciao_is_atom(ciao_term term) {
  return ciao_is_atom_s(ciao_implicit_state, term);
}

const char *ciao_atom_name_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!IsAtom(t)) {
    return (const char *)NULL;
  } else { 
    struct atom *atomptr;
    atomptr = TagToAtom(t);
    return atomptr->name;
  }
}

const char *ciao_atom_name(ciao_term term) {
  return ciao_atom_name_s(ciao_implicit_state, term);
}

char *ciao_atom_name_dup_s(ciao_state state, ciao_term term) {
  const char *s2;
  char *s;
  s2 = ciao_atom_name_s(state, term);
  s = (char *)ciao_malloc(sizeof(char *) * (strlen(s2) + 1));
  strcpy(s, s2);
  return s;
}

char *ciao_atom_name_dup(ciao_term term) {
  return ciao_atom_name_dup_s(ciao_implicit_state, term);
}

const char *ciao_structure_name_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TagIsSTR(t)) {
    return (const char *)NULL;
  } else {
    TAGGED f;
    struct atom *atomptr;
    f = TagToHeadfunctor(t);
    t = SetArity(f,0);
    atomptr = TagToAtom(t);
    return atomptr->name;
  }
}

const char *ciao_structure_name(ciao_term term) {
  return ciao_structure_name_s(ciao_implicit_state, term);
}

int ciao_structure_arity_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TagIsSTR(t)) {
    return 0;
  } else { 
    TAGGED f;
    f = TagToHeadfunctor(t);
    return Arity(f);
  }
}

int ciao_structure_arity(ciao_term term) {
  return ciao_structure_arity_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_list_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TagIsLST(t);
}

ciao_bool ciao_is_list(ciao_term term) {
  return ciao_is_list_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_empty_list_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsAtom(t) && t == MakeString("[]");
}

ciao_bool ciao_is_empty_list(ciao_term term) {
  return ciao_is_empty_list_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_structure_s(ciao_state state, ciao_term term) {
  TAGGED t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TagIsSTR(t);
}

ciao_bool ciao_is_structure(ciao_term term) {
  return ciao_is_structure_s(ciao_implicit_state, term);
}

ciao_term ciao_structure_arg_s(ciao_state state, ciao_term term, int i) {
  TAGGED t;
  TAGGED a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TagIsSTR(t)) return CIAO_ERROR;
  RefArg(a, t, i);
  return ciao_ref(state, a);
}

ciao_term ciao_structure_arg(ciao_term term, int i) {
  return ciao_structure_arg_s(ciao_implicit_state, term, i);
}

ciao_term ciao_list_head_s(ciao_state state, ciao_term term) {
  TAGGED t;
  TAGGED a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  RefCar(a, t);
  return ciao_ref(state, a);
}

ciao_term ciao_list_head(ciao_term term) {
  return ciao_list_head_s(ciao_implicit_state, term);
}

ciao_term ciao_list_tail_s(ciao_state state, ciao_term term) {
  TAGGED t;
  TAGGED a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  RefCdr(a, t);
  return ciao_ref(state, a);
}

ciao_term ciao_list_tail(ciao_term term) {
  return ciao_list_tail_s(ciao_implicit_state, term);
}

/* Helper functions */

ciao_term ciao_atom_s(ciao_state state, const char *name) {
  return ciao_structure_s(state, name, 0);
}

ciao_term ciao_atom(const char *name) {
  return ciao_atom_s(ciao_implicit_state, name);
}

ciao_term ciao_empty_list_s(ciao_state state) {
  return ciao_atom_s(state, "[]");
}

ciao_term ciao_empty_list() {
  return ciao_empty_list_s(ciao_implicit_state);
}

ciao_term ciao_list_s(ciao_state state, ciao_term head, ciao_term tail) {
  return ciao_structure_s(state, ".", 2, head, tail);
}

ciao_term ciao_list(ciao_term head, ciao_term tail) {
  return ciao_list_s(ciao_implicit_state, head, tail);
}

ciao_term ciao_dlist_a_s(ciao_state state, int len, ciao_term *args, ciao_term tail) {
  /* PRECONDITION: len >= 1 */ 
  int i;
  ciao_term list;
  
  list = tail;
  for (i = len - 1; i >= 0; i--) {
    list = ciao_list_s(state, args[i], list);
  }

  return list;
}

ciao_term ciao_dlist_a(int len, ciao_term *args, ciao_term tail) {
  return ciao_dlist_a_s(ciao_implicit_state, len, args, tail);
}

ciao_term ciao_listn_a_s(ciao_state state, int len, ciao_term *args) {
  return ciao_dlist_a_s(state, len, args, ciao_empty_list(state));
}

ciao_term ciao_listn_a(int len, ciao_term *args) {
  return ciao_listn_a_s(ciao_implicit_state, len, args);
}

#define GETARGS(LENGTH) \
  ciao_term *args; \
  int i; \
  va_list p; \
  args = alloca(sizeof(ciao_term) * LENGTH); \
  va_start(p, LENGTH); /* last argument before '...' */ \
  for (i = 0; i < LENGTH; i++) { \
    args[i] = va_arg(p, ciao_term); \
  } \
  va_end(p);

ciao_term ciao_structure_s(ciao_state state, const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_structure_a_s(state, name, arity, args);
}

ciao_term ciao_structure(const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_structure_a(name, arity, args);
}

ciao_term ciao_listn_s(ciao_state state, int length, ...) {
  GETARGS(length)
  return ciao_listn_a_s(state, length, args);
}

ciao_term ciao_listn(int length, ...) {
  GETARGS(length)
  return ciao_listn_a(length, args);
}

ciao_term ciao_dlist_s(ciao_state state, int length, ...) {
  GETARGS(length)
  return ciao_dlist_a_s(state, length - 1, args, args[length - 1]);
}

ciao_term ciao_dlist(int length, ...) {
  GETARGS(length)
  return ciao_dlist_a(length - 1, args, args[length - 1]);
}


ciao_term ciao_copy_term_s(ciao_state src_state, ciao_term src_term, ciao_state dst_state) {
  struct worker *w;
  w = dst_state->worker_registers;
  return ciao_ref(dst_state, cross_copy_term(w, ciao_unref(dst_state, src_term)));
}

ciao_term ciao_copy_term(ciao_term src_term) {
  return ciao_copy_term_s(ciao_implicit_state, src_term, ciao_implicit_state);
}

ciao_bool ciao_unify_s(ciao_state state, ciao_term x, ciao_term y) {
  struct worker *w;
  w = state->worker_registers;
  return cunify(w, ciao_unref(state, x), ciao_unref(state, y));
}

ciao_bool ciao_unify(ciao_term x, ciao_term y) {
  return ciao_unify_s(ciao_implicit_state, x, y);
}

ciao_bool ciao_equal_s(ciao_state state, ciao_term x, ciao_term y) {
  TAGGED a, b;
  a = ciao_unref(state, x);
  b = ciao_unref(state, y);
  DEREF(a, a);
  DEREF(b, b);
  return a == b;
}

ciao_bool ciao_equal(ciao_term x, ciao_term y) {
  return ciao_equal_s(ciao_implicit_state, x, y);
}

void ciao_at_exit(int result) {
/* #if defined(PROFILE) */
/*   if (profile) profile_dump(); */
/* #endif */
  fflush(NULL);
  exit(result);
}

/* Here with w->next_insn set up -- see local_init_each_time(). (MCL) */

JMP_BUF abort_env;                                              /* Shared */

int ciao_firstgoal(ciao_state state, ciao_term goal) {
  int i, exit_code;
  struct worker *w;

  w = state->worker_registers;
  w->node->term[0] = X(0) = ciao_unref(state, goal); /* WAS A init_atom_check!!! (and goal was goal_name) */
  w->next_insn = bootcode;

  while(TRUE) {
    i = SETJMP(abort_env);
    if (i == 0){                /* Just made longjmp */
      w->term[0] = w->node->term[0];
      wam_initialized = TRUE;
      exit_code = wam(w, state);
      flush_output(w);
      if (exit_code != WAM_ABORT) /* halting... */
        break;
    }
    else if (i == -1) {         /* SIGINT during I/O */
      REGISTER TAGGED *pt1;
      /* No need to patch "p" here, since we are not exiting wam() */
      REGISTER INSN *p = (INSN *)int_address;
      int_address = NULL;
      SETUP_PENDING_CALL(address_true);
      continue;
    }
#if defined(THREADS)
    (void)prolog_eng_killothers(w);
#endif
    reinitialize(w);                                      /* aborting... */
    init_each_time(w);                /* Sets X(0) to point to bootcode */
    *(struct definition **)(bootcode+2) = address_restart;
  }
  return exit_code;
}

int ciao_boot(ciao_state state) {
  int exit_code;
  exit_code = ciao_firstgoal(state, ciao_structure_s(state, "internals:boot", 0));
  return exit_code;
}

/* --------------------------------------------------------------------------- */

ciao_choice ciao_get_choice(ciao_state state) {
  struct worker *w;
  w = state->worker_registers;
  return ChoiceToInt(w->node);
}

ciao_bool ciao_more_solutions(ciao_state state, ciao_choice choice) {
  return ciao_get_choice(state) > choice;
}

void ciao_cut(ciao_state state, ciao_choice choice) {
  struct worker *w;
  w = state->worker_registers;
  if (!ciao_more_solutions(state, choice)) return;
  w->node = ChoiceFromInt(choice);
  SetShadowregs(w->node);
  PROFILE__HOOK_CIAOCUT;
}

void ciao_fail(ciao_state state) {
  struct worker *w;

  w = state->worker_registers;
  wam(w, state);
}

/* --------------------------------------------------------------------------- */

ciao_bool ciao_query_next(ciao_query *query) {
  if (!ciao_query_ok(query)) return FALSE;
  query->state->action = BACKTRACKING | KEEP_STACKS;
  ciao_fail(query->state);
  return ciao_query_ok(query);
}

ciao_bool ciao_query_ok(ciao_query *query) {
  struct try_node *next_alt;

  next_alt = query->state->worker_registers->next_alt;
  
  if (next_alt == &nullgoal_alt ||
      (next_alt == NULL && query->state->worker_registers->node->next_alt == &nullgoal_alt)) {
    return FALSE;
  } else {
    return TRUE;
  }
}

void ciao_query_end(ciao_query *query) {
  struct node *b;
  ciao_state state;
  struct worker *w;

  state = query->state;
  w = state->worker_registers;

  if (ciao_query_ok(query)) {
    ciao_cut(state, query->base_choice);
  }

  b = w->node;
  w->node = b = ChoiceCharOffset(b,-b->next_alt->node_offset);
  SetShadowregs(b);
  w->next_alt = NULL;

  ciao_free(query);
}

ciao_query *ciao_query_begin_term_s(ciao_state state, ciao_term goal) {
  struct worker *w;
  TAGGED *b0;
  struct node *b;
  ciao_query *query;

  goal = ciao_structure_s(state, "hiord_rt:call", 1, goal);

  w = state->worker_registers;
  DEREF(X(0), ciao_unref(state, goal));

  /* push null choice */

  w->next_insn = default_code;
  b0 = (TAGGED *)w->node;
  b = ChoiceCharOffset(b0, ArityToOffset(0));
  ComputeA(w->local_top,w->node);
  w->node = b;
  NewShadowregs(w->global_top);

  b->trail_top = w->trail_top;
  SaveGtop(b,w->global_top);
  b->next_alt = &defaultgoal_alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  SaveLtop(b);
    
  w->next_alt = NULL; 

  query = (ciao_query *)ciao_malloc(sizeof(ciao_query));
  query->state = state;
  query->base_choice = ciao_get_choice(state);
  
  /* push choice for starting goal */
  
  w->next_insn = call_code;
  b0 = (TAGGED *)w->node;
  b = ChoiceCharOffset(b0, ArityToOffset(1));
  ComputeA(w->local_top,w->node);
  w->node = b;
  NewShadowregs(w->global_top);

  b->trail_top = w->trail_top;
  SaveGtop(b,w->global_top);
  b->next_alt = &startgoal_alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  SaveLtop(b);
  b->term[0] = X(0);    /* Will be the arg. of a call/1 */
    
  w->next_alt = NULL; 

  state->action = BACKTRACKING | KEEP_STACKS;
  wam(w, state);

  return query;
}

ciao_query *ciao_query_begin_term(ciao_term goal) {
  return ciao_query_begin_term_s(ciao_implicit_state, goal);
}

ciao_query *ciao_query_begin_s(ciao_state state, const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_query_begin_term_s(state, ciao_structure_a_s(state, name, arity, args));
}

ciao_query *ciao_query_begin(const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_query_begin_term(ciao_structure_a(name, arity, args));
}

ciao_bool ciao_commit_call_term_s(ciao_state state, ciao_term goal) {
  ciao_bool ok;
  ciao_query *query;

  query = ciao_query_begin_term_s(state, goal);
  ok = ciao_query_ok(query);
  ciao_query_end(query);

  return ok;
}

ciao_bool ciao_commit_call_term(ciao_term goal) {
  return ciao_commit_call_term_s(ciao_implicit_state, goal);
}

ciao_bool ciao_commit_call_s(ciao_state state, const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_commit_call_term_s(state, ciao_structure_a_s(state, name, arity, args));
}

ciao_bool ciao_commit_call(const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_commit_call_term(ciao_structure_a(name, arity, args));
}

/* --------------------------------------------------------------------------- */

#include <setjmp.h>

jmp_buf ciao_gluecode_jmpbuf;

void ciao_raise_exception_s(ciao_state state, ciao_term exception) {
  struct worker *w;
  w = state->worker_registers;

  X(0) = ciao_unref(state, exception);
  longjmp(ciao_gluecode_jmpbuf, 1);
}

void ciao_raise_exception(ciao_term exception) {
  ciao_raise_exception_s(ciao_implicit_state, exception);
}

/* --------------------------------------------------------------------------- */

#define GARBAGE_PROTECTION

#ifdef GARBAGE_PROTECTION 

#define REF_TABLE_PAD 4
#define REF_TABLE_CHUNK_SIZE 32
#define REF_TABLE_CHUNKS 1

TAGGED create_ref_table(ciao_state state, int chunks) {
  struct worker *w;
  int i, j;
  TAGGED *pt, *pt0;
  TAGGED functor;

  ciao_ensure_heap(state, REF_TABLE_CHUNK_SIZE * chunks + 1);
  w = state->worker_registers;
  functor = SetArity(MakeString("$reftable"), (REF_TABLE_CHUNK_SIZE - 1));
  pt = w->global_top;
  pt0 = pt;
  for (j = 0; j < chunks - 1; j++) {
    HeapPush(pt, functor);
    for (i = 0; i < REF_TABLE_CHUNK_SIZE - 2; i++) {
      HeapPush(pt, TagHVA(pt));
    }
    HeapPush(pt, Tag(STR, pt + 1));
  }
  if (chunks > 0) {
    HeapPush(pt, functor);
    for (i = 0; i < REF_TABLE_CHUNK_SIZE - 1; i++) {
      HeapPush(pt, TagHVA(pt));
    }
  }
  w->global_top = pt;  
  return Tag(STR, pt0);
}

ciao_term ciao_ref(ciao_state state, TAGGED x) {
  struct worker *w;
  TAGGED *pt1;
  ciao_term term;
  int next, chunks;

  w = state->worker_registers;
  SetE(w->frame);

  next = GetSmall(Y(0));
  {
    TAGGED t0, t1, ta;
    ta = *TagToArg(Y(2), next);
    CUNIFY(ta, x);
    goto ok;
  }
 fail:
    /* fatal error */
    SERIOUS_FAULT("Error registering term");
 ok:
  term = next;
  next++;
  if ((next & (REF_TABLE_CHUNK_SIZE - 1)) == (REF_TABLE_CHUNK_SIZE - 1)) 
    next++; /* skip functor */
  if ((next & (REF_TABLE_CHUNK_SIZE - 1)) == 0) 
    next++; /* skip str tag */
  Y(0) = MakeSmall(next);

  chunks = GetSmall(Y(1));
  if (chunks * REF_TABLE_CHUNK_SIZE - next < REF_TABLE_PAD) { /* chunk overflow! */
    TAGGED *x, *y;
    TAGGED new_table;
    int i, j, new_chunks, k;

    new_chunks = chunks * 2;
    /* old table is in Y(2) so don't care about gc here */  
    new_table = create_ref_table(state, new_chunks); 

    x = TagToArg(Y(2), 0);
    y = TagToArg(new_table, 0);
    k = 0;
    for (j = 0; j < chunks - 1; j++) {
      k++;
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 2; i++) {
	y[k] = x[k];
	if (k == term) goto end;
	k++;
      }
      k++;
    }
    if (chunks > 0) {
      k++;
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 1; i++) {
	y[k] = x[k];
	if (k == term) goto end;
	k++;
      }
    }
  end:
    Y(2) = new_table;
    Y(1) = MakeSmall(new_chunks);
  }

  return term;
}

TAGGED ciao_unref(ciao_state state, ciao_term term) {
  struct worker *w;
  TAGGED *pt1;
  TAGGED x;

  w = state->worker_registers;
  SetE(w->frame);

  x = *TagToArg(Y(2), term);
  return x;
}

void ciao_frame_begin_s(ciao_state state) {
  TAGGED *pt1;
  struct worker *w = state->worker_registers;
  int arity;

  arity = 3;

  ComputeE;		
  E->next_insn = w->next_insn;
  E->frame = w->frame;
  w->frame = E;
  w->next_insn = &contcode[(1+LOffset)*arity];
  w->local_top = (struct frame *)Offset(E,EToY0+arity);
  Y(0) = MakeSmall(1); /* next free ref */
  Y(1) = MakeSmall(REF_TABLE_CHUNKS); /* chunks */
  Y(2) = create_ref_table(state, REF_TABLE_CHUNKS);
}

void ciao_frame_begin() {
  ciao_frame_begin_s(ciao_implicit_state);
}

void ciao_frame_end_s(ciao_state state) {
  struct worker *w;
  TAGGED *pt1;
  w = state->worker_registers;

  SetE(w->frame); 

  w->local_top = E;
  w->frame = E->frame;
  w->next_insn = E->next_insn;
}

void ciao_frame_end() {
  ciao_frame_end_s(ciao_implicit_state);
}

#else

ciao_term ciao_ref(ciao_state state, TAGGED x) {
  return (ciao_term)x;
}

TAGGED ciao_unref(ciao_state state, ciao_term term) {
  return (TAGGED)term;
}

void ciao_frame_begin_s(ciao_state state) {
}
  
void ciao_frame_end_s(ciao_state state) {
}

void ciao_frame_begin() {
}
  
void ciao_frame_end() {
}

#endif


