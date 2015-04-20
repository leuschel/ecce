#include "ciao_prolog.h"
#include <stdio.h>
#include <stdlib.h>

void mydisplay(ciao_term term) {
  if (ciao_is_atom(term)) {
    printf("<atom name=\"%s\"/>", ciao_atom_name(term));
  } else if (ciao_is_structure(term)) {
    int i;
    int a;
    a = ciao_structure_arity(term);
    printf("<structure name=\"%s\" arity=\"%d\">", ciao_structure_name(term), a);
    for (i = 1; i <= a; i++) {
      printf("<argument number=\"%d\">", i);
      mydisplay(ciao_structure_arg(term, i));
      printf("</argument>");
    }
    printf("</structure>");
  } else if (ciao_is_list(term)) {
    printf("<list>");
    printf("<head>");
    mydisplay(ciao_list_head(term));
    printf("</head>");
    printf("<tail>");
    mydisplay(ciao_list_tail(term));
    printf("</tail>");
    printf("</list>");
  } else if (ciao_is_empty_list(term)) {
    printf("<empty_list/>");
  } else if (ciao_is_integer(term)) {
    printf("<integer value=\"%d\"/>", ciao_to_integer(term));
  } else if (ciao_is_number(term)) {
    printf("<float value=\"%f\"/>", ciao_to_float(term));
  } else {
    printf("<unknown/>");
  }
}

int main()
{
  ciao_term var, list, complex_term;
  ciao_query *query;

  printf("Initializing Ciao...\n");

  ciao_opts("program_name", 0, NULL, 0, NULL, NULL);
  ciao_init();

  printf("Creating WAM...\n");

  ciao_implicit_state = ciao_state_new();

  printf("Loading test.po...\n");

  ciao_load_qfile("test.po");

  printf("Testing...\n");

  ciao_frame_begin();

  complex_term = ciao_var();

  ciao_commit_call("test:mydisplay", 1, complex_term);

  mydisplay(complex_term);
  printf("\n");
  ciao_commit_call("test:complex_term", 1, complex_term);
  mydisplay(complex_term);
  printf("\n");

  ciao_commit_call("test:mydisplay", 1, complex_term);
  mydisplay(complex_term);
  printf("\n");
  
  var = ciao_var();

  list = ciao_listn(7,
		       ciao_integer(2),
		       ciao_integer(3),
		       ciao_integer(5),
		       ciao_integer(7),
		       ciao_integer(11),
		       ciao_integer(13),
		       ciao_integer(17)
		       );
  printf("Created var %x\n", list);

  ciao_commit_call("test:mydisplay", 1, list);

  printf("Calling member/2\n");
  query = ciao_query_begin("test:member", 2, var, list);
  printf("Done\n");
  while (ciao_query_ok(query)) {
    printf("There is a solution\n");
    if (ciao_is_integer(var)) {
      printf("Number %d\n", ciao_to_integer(var));
    } else {
      printf("Not a number!\n");
    }
    printf("Searching next\n");
    ciao_query_next(query);
  }
  printf("End\n");
  ciao_query_end(query);

  query = ciao_query_begin("test:numbers", 1, var);
  while (ciao_query_ok(query)) {
    if (ciao_is_integer(var)) {
      printf("Number %d\n", ciao_to_integer(var));
    } else {
      printf("Not a number!\n");
    }
    ciao_commit_call("test:mydisplay", 1, var);
    if (ciao_commit_call("test:member", 2, var, list)) {
      printf("Is a selected number\n");
    }
    ciao_query_next(query);
  }
  ciao_query_end(query);
  
  ciao_frame_end();
  printf("End\n");

  return 0;
}
