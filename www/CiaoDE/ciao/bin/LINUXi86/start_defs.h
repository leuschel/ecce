void load_ql_files(Argdecl, FILE *qfile);
struct worker *create_and_init_wam(void);
void create_source_path(char *pathname);
int start_prolog_program(char * caller_name,
                         char * program_name);
int start(int argc, char **argv);
void at_exit(int exit_code);
