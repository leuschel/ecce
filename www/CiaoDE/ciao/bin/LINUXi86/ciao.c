
/* Access to CIAO from C (or, at least, skeletons) */

#include <stdio.h>
#include "ciao.h"

#if defined(__svr4__)
# define _POSIX_SOURCE 1
#endif

/* declarations for global functions accessed here */

#include "ENG_defs.h"

/* local declarations */



ENG_pred_ref ENG_predicate(char_a, long_number, char_b)
     char *char_a;
     long long_number;
     char *char_b;
{
  fprintf(stderr, "Calling ENG_predicate in ciao.c, not yet implemented\n");
  return NULL;
}


int ENG_put_string(my_term, my_char)
     ENG_term_ref my_term;
     char * my_char;
{
  fprintf(stderr, "Calling ENG_put_string in ciao.c, not yet implemented\n");
  return 0;
}


int ENG_query_cut_fail(predicate)
     ENG_pred_ref predicate;
{
  fprintf(stderr,
          "Calling ENG_query_cut_fail in ciao.c, not yet implemented\n");
  return 0;
}
  
