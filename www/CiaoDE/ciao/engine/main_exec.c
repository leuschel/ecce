/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include "main.h"

extern TAGGED init_atom_check PROTO((char *));

void ENG_start_user_ql();
void ENG_init();
void ENG_load_internal_ql();
void ENG_kick_start();


/*#define X_OK 1*/

/* Global initializations. Called once at the beginning of the execution. */

BOOL interactive = FALSE;

void ENG_init(argc, argv)
     int argc;
     char *argv[];
{
  char *lc_ctype;
  BOOL loadql = FALSE;
  BOOL boot_from_ql = FALSE;
  int i, lastflag=0;
  char *pathname=NULL;
  FILE *qfile;

				                  /* internationalization */
  lc_ctype = getenv("LC_CTYPE");
  if (lc_ctype!=NULL &&
      (strcmp(lc_ctype,"ja_JP.EUC")==SAME || strcmp(lc_ctype,"ja_JP.euc")==SAME))
    init_kanji();
  else
    init_latin1();

  compute_cwd();
  checkasserts();
#if defined(USE_OWN_MALLOC)
  init_own_malloc();
#endif
  init_once();
  init_alloc(Arg);
  init_each_time(Arg);
  /* mem_prog_reset(); */

  prolog_argc = argc;
  prolog_argv = argv;

  /*  ENG_start_user_ql(); */

  for(i=1; i < argc; i++)                        /* Detect the first "-C" */
    if (strcmp(argv[i], "-C") == SAME){
      prolog_argc = i;
      break;
    }

  if (prolog_argc != argc){                      /* There are C arguments */
    for(i = prolog_argc + 1; i < argc; i++)
      if (strcmp(argv[i],"-b") == SAME) {              /* Load a .ql file */
        i++;
        pathname = argv[i];
        loadql = TRUE;
      } else
      if (strcmp(argv[i], "-B") == SAME) {      /* Load .ql file and boot */
        i++;                               /* from main/0 predicate in it */
        pathname = argv[i];
        loadql = TRUE;
        boot_from_ql = TRUE;
      }
      else 
      if (strcmp(argv[i], "-tp") == SAME){        /* Trace predicates */
        i++;
        predtrace = TRUE;
      }
#if defined(DEBUG)                         /* Pack here debugging options */
      else if (strcmp(argv[i],"-d") == SAME)
        debug_c = 1;
#endif
      else if (strcmp(argv[i], "-C") != SAME)  /* Ignore other "-C" */
        fprintf(stderr,"Warning: %s ignored\n",argv[i]);
  }




  if (loadql){
    if (pathname==NULL) {
      fprintf(stderr,"Usage: %s {-b qlfile | -B startup_qlfile} arg...\n", 
              argv[0]);
      at_exit(1);
    } 
    if((qfile=fopen(pathname,"r"))==NULL) {
      fprintf(stderr, "%s: qlfile not found\n", pathname);
      at_exit(1);
    }
    {
      WAMENV;

      int more_ql;
      
      push_qlinfo(w);
      qread1(w,qfile,&X(0));	                    /* ignore version no. */
      w->global_top = w->node->global_top;

      while (more_ql)
        while ((more_ql = qread1(w,qfile,&X(0))) && run_determ_c(w,X(0)))
          w->global_top = w->node->global_top;

      pop_qlinfo(w);
      fclose(qfile);
      w->node->global_top = w->global_top;
      if (boot_from_ql){
        w->node->term[0] = X(0) = init_atom_check("internals:boot");
        startwam(w);
      }
    }
  }
}


void ENG_load_internal_ql(Arg, qlcodestream, qlcodesize)
     Argdecl;
     char qlcodestream[];
     int qlcodesize;
{
  int i;
  struct qlstream qlcode;

  qlcode.qlpointer = qlcodestream; 
  qlcode.qlremains = qlcodesize; 

  push_qlinfo(w);
  qlinsert(w,&qlcode,&X(0));
  w->global_top = w->node->global_top;
  while (qlinsert(w,&qlcode,&X(0)) && run_determ_c(w,X(0)))
    w->global_top = w->node->global_top;
  pop_qlinfo(w);
  w->node->global_top = w->global_top;
}


void ENG_start_user_ql(Arg)
     Argdecl;
{
  context_switch();
}


void ENG_kick_start(Arg)
     Argdecl;
{

  w->node->term[0] = X(0) = init_atom_check("internals:boot");
  startwam(w);
  at_exit(0);
}


