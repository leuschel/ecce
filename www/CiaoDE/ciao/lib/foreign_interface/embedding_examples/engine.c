#include "ciao_prolog.h"
#include <stdio.h>
#include <string.h>

int ciao_start(int argc, char *argv[]) 
{
  ciao_desc desc;
  int i;
  int optc; char **optv; 
  int programc; char **programv;
  char *program_name;
  char *raw_source_path = NULL;
  int exit_code;

  /* Split arguments in C arguments and Ciao arguments
     ( program [args] -C [ciao-args] )*/

  programc = argc;
  for(i=1; i < argc; i++)                        /* Detect the first "-C" */
    if (strcmp(argv[i], "-C") == SAME){
      programc = i;
      break;
    }
  programv = argv;

  optv = &argv[programc + 1];
  optc = argc - (programc + 1);

  program_name = argv[0];

  ciao_opts(program_name, programc, programv, optc, optv, &raw_source_path);
  ciao_init();

  desc = ciao_desc_new();
  if (raw_source_path != NULL) {
    ciao_load_qfile(desc, raw_source_path);
  } else {
    ciao_load_embedded_qfile(desc, raw_source_path);
  }

  exit_code = ciao_boot(desc);

  fflush(NULL);
  return exit_code;
}

int main(int argc, char *argv[])
{
  printf("Ciao Loader...\n");
  return ciao_start(argc, argv);
}
