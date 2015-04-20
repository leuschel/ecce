/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */


/*          INITIALIZATION INFO 
    definitions, structures, and global variables needed for
    initializations */

/* These should be local to a thread */

/* extern TAGGED heap_start, *heap_end, heap_warn, *heap_warn_soft,
   *stack_start, *stack_end, *stack_warn, tagged_choice_start, choice_start,
   *choice_end, trail_start, *trail_end; */

/* extern char *atom_buffer; */  /* Non-shared --- used by each worker */


/* I believe that atom_buffer_length can change dynamically; should be
   private to each worker, then.  Will that pose problems problems with the
   hashing functions? */

/* extern int atom_buffer_length; */  /* Non-shared --- used by each worker */


/* Streams pointing to "user" -- should be shared */

extern struct stream_node *stream_user_input;                   /* Shared */
extern struct stream_node *stream_user_output;                  /* Shared */
extern struct stream_node *stream_user_error;                   /* Shared */


/* root of the stream pointers -- shared */

extern struct stream_node *root_stream_ptr;            /* Shared & locked */

extern struct sw_on_key *prolog_predicates;    /* Shared -- never changes */
/*extern struct sw_on_key *user_predicates;*/
extern struct sw_on_key **predicates_location;                  /* Shared */

/* Database locks */
extern SLOCK    prolog_predicates_l;                      /* Pointer to it */

/* Wait until new worker Id is generated */
extern SLOCK    worker_id_pool_l;
extern SLOCK    atom_id_l;
extern SLOCK    wam_list_l;


#if defined(DEBUG)
extern SLOCK    ops_counter_l;
#endif


extern BOOL wam_initialized;/* Non-shared? --- set by each worker to decide whether re-initialize or exit after a failure */

extern char symbolchar[];               /* Shared --- type of each symbol */

extern char *installibdir;

 /* All atom & functor definitions are shared */

#if defined(MARKERS)
 TAGGED atom_success;
 TAGGED atom_failure;
#endif

extern TAGGED atom_share;
extern TAGGED atom_noshare;
extern TAGGED atom_nil;
extern TAGGED atom_list;
extern TAGGED atom_user_input;
extern TAGGED atom_user_output;
extern TAGGED atom_user_error;
extern TAGGED atom_read;
extern TAGGED atom_write;
extern TAGGED atom_append;
extern TAGGED atom_socket;
extern TAGGED atom_symlink;
extern TAGGED atom_regular;
extern TAGGED atom_directory;
extern TAGGED atom_fifo;
extern TAGGED atom_unknown;
extern TAGGED atom_prolog;
extern TAGGED atom_user;
extern TAGGED atom_lessthan;
extern TAGGED atom_greaterthan;
extern TAGGED atom_equal;
extern TAGGED atom_off;
extern TAGGED atom_on;
extern TAGGED atom_error;
extern TAGGED atom_trace;
extern TAGGED atom_debug;
extern TAGGED atom_fail;
extern TAGGED atom_all;
extern TAGGED atom_terse;
extern TAGGED atom_verbose;
extern TAGGED atom_compiled;
extern TAGGED atom_interpreted;
extern TAGGED atom_builtin;
extern TAGGED atom_true;
extern TAGGED atom_false;
extern TAGGED atom_retry_hook;
extern TAGGED atom_unprofiled;
extern TAGGED atom_profiled;
/*extern TAGGED atom_public;*/
extern TAGGED atom_wait;
extern TAGGED atom_dynamic;
extern TAGGED atom_concurrent;
extern TAGGED atom_multifile;

extern TAGGED atom_self;
extern TAGGED atom_create;

extern TAGGED atom_installibdir;

extern TAGGED atom_block;
extern TAGGED atom_no_block;

#if defined(GAUGE)
extern TAGGED atom_counter;
#endif

extern TAGGED functor_neck;
extern TAGGED functor_list;
extern TAGGED functor_cut;
extern TAGGED functor_minus;
extern TAGGED functor_slash;
extern TAGGED functor_and;
extern TAGGED functor_functor;
extern TAGGED functor_tagged;
extern TAGGED functor_emul_entry;
extern TAGGED functor_builtin;
extern TAGGED functor_Dref;
extern TAGGED functor_Dstream;
extern TAGGED functor_Dsetarg;
extern TAGGED functor_large;
extern TAGGED functor_long;

extern TAGGED functor_active;
extern TAGGED functor_pending;
extern TAGGED functor_failed;
extern TAGGED functor_available;

extern TAGGED current_prompt;
extern TAGGED current_unknown;
/* extern TAGGED current_leash_mode; */
/* extern TAGGED current_maxdepth; */
/* extern TAGGED current_printdepth; */
/* extern TAGGED current_breaklevel; */
extern TAGGED current_compiling;
/* extern TAGGED current_character_escapes_flag; */
extern TAGGED current_ferror_flag;
/* extern TAGGED current_single_var_flag; */
/* extern TAGGED current_discontiguous_flag; */
/* extern TAGGED current_redefine_flag; */
extern TAGGED current_quiet_flag;
extern TAGGED current_gcmode;
extern TAGGED current_gctrace;
extern TAGGED current_gcmargin;
/*extern TAGGED current_debugger_state;*/  /* Now private */
/*extern TAGGED current_debugger_mode;*/   /* Now private */
extern TAGGED current_radix;

extern struct try_node *address_nd_repeat;
extern struct try_node *address_nd_current_instance;
extern struct try_node *address_nd_current_atom;
extern struct try_node *address_nd_current_predicate;
extern struct try_node *address_nd_predicate_property;
extern struct try_node *address_nd_current_stream;
extern struct try_node *address_nd_atom_concat;

extern struct definition *address_true;
extern struct definition *address_fail;

/*
#if !defined(ATTRVARS)
extern struct definition *address_fast_apply;
extern struct definition *address_slow_apply;
extern struct definition *address_apply;
#endif
*/

extern struct definition *address_call;
#if defined(INTERNAL_CALLING)
extern struct definition *address_internal_call;
#endif
extern struct definition *address_interpret_goal;
extern struct definition *address_call_with_cont;
extern struct definition *address_interpret_compiled_goal;
extern struct definition *address_interpret_c_goal;
extern struct definition *address_undefined_goal;
extern struct definition *address_help; 
extern struct definition *address_restart; 
extern struct definition *address_trace;
extern struct definition *address_getct;
extern struct definition *address_getct1;
extern struct definition *address_get;
extern struct definition *address_get2;
extern struct definition *address_get1;
extern struct definition *address_get12;
extern struct definition *address_peek;
extern struct definition *address_peek2;
extern struct definition *address_skip;
extern struct definition *address_skip2;
extern struct definition *address_skip_line;
extern struct definition *address_skip_line1;
extern struct definition *address_error;

extern BOOL nd_repeat PROTO((struct worker *w));
extern BOOL nd_current_atom PROTO((struct worker *w));
extern BOOL nd_current_clauses PROTO((struct worker *w));
extern BOOL nd_current_predicate PROTO((struct worker *w));
extern BOOL nd_predicate_property PROTO((struct worker *w));
extern BOOL nd_current_stream PROTO((struct worker *w));
extern BOOL nd_atom_concat PROTO((struct worker *w));

struct definition *define_c_mod_predicate(char *module, char  *pname, int arity, BOOL (*procedure)());
void undefine_c_mod_predicate(char *module, char *pname, int arity); 
