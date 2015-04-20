/*

*/

void choice_overflow(Argdecl, int pad);
void stack_overflow(Argdecl);
BOOL gc_start(Argdecl);
void heap_overflow(Argdecl, int pad);
void collect_goals_from_trail(Argdecl, int wake_count);
void trail_gc(Argdecl);
BOOL stack_shift_usage(Argdecl);
BOOL termheap_usage(Argdecl);
BOOL envstack_usage(Argdecl);
BOOL choice_usage(Argdecl);
BOOL trail_usage(Argdecl);
void explicit_heap_overflow(Argdecl, int pad, int arity);
