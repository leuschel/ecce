
#if defined(THREADS) && defined(USE_POSIX_THREADS)
extern pthread_attr_t detached_thread;
extern pthread_attr_t joinable_thread;
#endif

BOOL prolog_eng_kill(Argdecl);
BOOL prolog_eng_killothers(Argdecl);
BOOL prolog_eng_wait(Argdecl);
BOOL prolog_eng_self(Argdecl);
BOOL prolog_eng_status(Argdecl);
BOOL prolog_eng_status1(Argdecl);
BOOL prolog_eng_release(Argdecl);
BOOL prolog_eng_call(Argdecl);
BOOL prolog_eng_backtrack(Argdecl);
BOOL prolog_eng_cut(Argdecl);

