
BOOL prolog_bootversion(register struct worker *w);
BOOL prolog_sourcepath(register struct worker *w);
BOOL prolog_open(register struct worker *w);
BOOL prolog_close(register struct worker *w);
BOOL prolog_unix_popen(register struct worker *w);
void ENG_perror(char *s);
BOOL prolog_current_input(register struct worker *w);
BOOL prolog_set_input(register struct worker *w);
BOOL prolog_current_output(register struct worker *w);
BOOL prolog_set_output(register struct worker *w);
BOOL prolog_get_stream(register struct worker *w);
BOOL prolog_replace_stream(register struct worker *w);
BOOL prolog_stream_code(register struct worker *w);
BOOL character_count(register struct worker *w);
BOOL line_position(register struct worker *w);
BOOL line_count(register struct worker *w);
