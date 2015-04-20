/* Copyright (C) 1996,1997,1998, UPM-CLIP */

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>

#include "main.h"
#include "debug.h"
#include "initial.h"
#include "threads.h"
#include "task_areas.h"

/* declarations for global functions accessed here */

#include "wam_defs.h"
#include "tasks_defs.h"
#include "unix_utils_defs.h"
#include "initial_defs.h"
#include "main_defs.h"
#include "qread_defs.h"
#include "builtin_defs.h"
#include "own_malloc_defs.h"
#include "alloc_defs.h"
#include "support_defs.h"
#include "locks_defs.h"
#include "profile_defs.h"
#include "startgoal_defs.h"
#include "prolog_tasks_defs.h"

/* local declarations */


#define USAGE_STRING "Usage: %s [prolog_args] -C [-i] [-q] [-v] -b bootfile\n"

BOOL interactive = FALSE;

/*
extern void init_kanji(), init_latin1();
extern void compute_cwd();
extern void checkasserts(), mem_start_level();
extern void firstgoal();
extern void init_wrb_state_list(), init_once(), init_alloc();
extern void init_each_time();
*/

/* Not required now (DCG) */
/*
static void find_emulator(file)
     char *file;
{
  REGISTER char *p;
  char path[MAXPATHLEN+1];

  for (p=file; *p && *p != '/'; p++)
    ;
  if (*p == '/')
    p = ".";
  else
    p = getenv("PATH");	
  
  while (*p) {
      REGISTER char *next = path;
      
      while (*p && *p != ':')
	*next++ = *p++;
      *next++ = '/';
      *next++ = 0;
      if (*p) p++;
      if (!strcmp(path, "./")) path[0] = 0;
      strcat(path, file);
      
      if (!access(path, X_OK)){
        int emul_path_length = 1+strlen(path);
        emulator_path = (char *)checkalloc(emul_path_length);
        strcpy(emulator_path, path);
        if (expand_file_name(emulator_path, path)) {
          emulator_path = (char *)checkrealloc((TAGGED *)emulator_path, 
                                               emul_path_length,
                                               1+strlen(path));
          strcpy(emulator_path, path);
          return;
        }
        else checkdealloc((TAGGED *)emulator_path, emul_path_length);
      }
  }
  fprintf(stderr, "%s: emulator not found\n", file);
  at_exit(1);
}
*/

void load_ql_files(Arg, qfile)
     Argdecl;
     FILE *qfile;
{
  int more_ql;

  push_qlinfo(NULL);
  if (is_a_script(qfile)) 
    skip_to_ctrl_l(qfile);
  more_ql = qread1(w,qfile,&X(0));	            /* ignore version no. */
  w->global_top = w->node->global_top;              /* Reset heap pointer */
  
  while (more_ql)
    while ((more_ql = qread1(w,qfile,&X(0))) && run_determ_c(w,X(0)))
      w->global_top = w->node->global_top;
  
  pop_qlinfo(NULL);
}

 /* Creates the wam structure, allocates its areas and initializes them.
    This returns an empty, fresh wam.  We do not add it here to the task
    state list; it needs its own thread, which we have after startwam() */


extern ENG_INT mem_prog_count;

struct worker *create_and_init_wam()
{
  Argdecl;
  /*ENG_INT saved_program_count = mem_prog_count;  */

  Arg = create_wam_storage();                         /* Just create *Arg */
  create_wam_areas(Arg);                      /* Make room for the stacks */
  numstack_init(Arg);                                     /* bignum areas */
  local_init_each_time(Arg);                               /* Local areas */

  /*mem_prog_count = saved_program_count;                          */
  
  return Arg;
}



extern char cwd[];


int main(argc, argv)
     int argc;
     char *argv[];
{
  /*Argdecl;*/
  wrb_state_p first_worker;
  BOOL quiet = FALSE;
  int i;
  char *lc_ctype;
  char *raw_source_path = NULL;
  FILE *qfile;
  /*init_mm();*/
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
  /*  find_emulator(argv[0]); */ /* Not useful now (DCG) */

  prolog_argc = argc;
  prolog_argv = argv;

  for(i=1; i < argc; i++)                        /* Detect the first "-C" */
    if (strcmp(argv[i], "-C") == SAME){
      prolog_argc = i;
      break;
    }

  if (prolog_argc != argc){                      /* There are C arguments */
    for(i = prolog_argc + 1; i < argc; i++)
      if (strcmp(argv[i],"-b") == SAME)
        raw_source_path = argv[++i];
      else if (strcmp(argv[i],"-i") == SAME)
        prolog_force_interactive = 1;
      else if (strcmp(argv[i],"-q") == SAME)             /* To make quiet */
        quiet = TRUE;
      else if (strcmp(argv[i],"-v") == SAME)           /* To make verbose */
        quiet = FALSE;
    /*
      else if (strcmp(argv[i],"-L") == SAME){    
#if defined(Win32)
	library_directory = (char *)checkalloc(MAXPATHLEN+1);
	expand_file_name(argv[++i],library_directory);
#else
	library_directory = argv[++i];
#endif
      }
    */
      else
#if defined(PROFILE)
        if (strcmp(argv[i], "-prof") == SAME)        /* Simple profile */
          profile = TRUE;
        else if (strcmp(argv[i], "-proft") == SAME)         /* Include time */
          {profile = TRUE; prof_include_time = TRUE;}
        else
#endif
          if (strcmp(argv[i], "-tp") == SAME)        /* Trace predicates */
            predtrace = TRUE;
#if defined(DBG) || defined(DEBUG)
      else if (strcmp(argv[i], "-dt") == SAME)           /* debug threads */
        debug_threads = TRUE;
      else if (strcmp(argv[i], "-dgc") == SAME)      /* debug garb. coll. */
        debug_gc = TRUE;
      else if (strcmp(argv[i], "-dmem") == SAME)       /* debug mem. man. */
        debug_mem = TRUE;
      else if (strcmp(argv[i], "-dconc") == SAME)    /* debug concurrency */
        debug_conc = TRUE;
#endif
#if defined(DEBUG)                         /* Pack here debugging options */
      else if (strcmp(argv[i],"-d") == SAME)
        debug_c = 1;
#endif
      else if (strcmp(argv[i], "-C") != SAME)  /* Ignore other "-C" */
        fprintf(stderr,"Warning: %s ignored\n",argv[i]);
  }

  if (profile||predtrace) stop_on_pred_calls = TRUE;

  if (raw_source_path == NULL) {   /* issue an error if no boot file given */
    fprintf(stderr, USAGE_STRING, argv[0]);
    at_exit(1);
  }

#if defined(Win32)
  /* This assumes ciaoengine is run as relative/absolute path */
  {
    char *p;
    int slashcount = 0;

    p = argv[0];
    if (*p == '"') p++ ;

    expand_file_name(p, library_directory);

    /* skip 3 '/' because engine is in directory "$/Win32/bin/" */
    p = library_directory+strlen(library_directory);

    while(p >= library_directory && slashcount<1)
      if (*p-- == '/') slashcount++;

    *(p+1) = (char)0;

    while(p >= library_directory && slashcount<3)
      if (*p-- == '/') slashcount++;

    if (strcasecmp(++p,"/Win32/bin") == SAME)
      *p = (char)0;
    else {
      fprintf(stderr,
              "%s: should be in the standard location, under Win32/bin/",
              argv[0]);
      at_exit(1); 
    }
  }
#else
  if (!(library_directory = getenv("CIAOLIB")))
    library_directory = installibdir;
#endif

  /* This is here in order to be already defined library_directory */
  expand_file_name(raw_source_path,source_path);

#if defined(Win32)
  i = strlen(source_path)-4;
  if (i > 0 && strcmp(source_path+i,".bat") == SAME){
    source_path[i+1] = 'c';
    source_path[i+2] = 'p';
    source_path[i+3] = 'x';
  } else
    if (i > 0 && strcmp(source_path+i,".cpx") != SAME)
      strcat(source_path,".cpx");
#endif

  if((qfile=fopen(source_path,"r"))==NULL) {
    fprintf(stderr, "%s: boot file not found\n", source_path);
    at_exit(1);
  } else {                         /* We have a bootfile we can read from */

    /* Global initializations */
    checkasserts();
    init_wrb_state_list();
    init_once();
    init_alloc();
    current_quiet_flag = quiet ? atom_on : atom_off;
    /*mem_prog_count = 0;*/

    glb_init_each_time();

 /* Make the first wam.  We need it to load the ql's. Main thread is always
    goal # 0 */

    first_worker = init_first_worker_entry();
    load_ql_files(first_worker->worker_registers, qfile);
    fclose(qfile);
    /* wam->next_insn set to boot code in local_init_each_time */
    /*w->node->global_top = w->global_top;*/     /* Isn't this unnecessary? */
    /* w->node->term[0] = X(0) = init_atom_check("boot");*/
    firstgoal(first_worker, "boot");              /*  Fills in worker_entry */
  }
  
  return 0;

}


void at_exit(result)
     int result;
{
#if defined(PROFILE)
  if (profile) dump_profile();
#endif
  exit(result);
}
