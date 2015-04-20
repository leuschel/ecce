
#if defined(i86) && defined(LINUX)

#define get_timestamp() \
({ \
  REGISTER ENG_LINT __res; \
  __asm__ __volatile__("rdtsc" : "=A"(__res)); \
  __res; \
})

#endif

ENG_FLT usertime(void);
ENG_FLT walltime(void);
void init_statistics(void);
void reset_statistics(void);

extern ENG_LINT (*userclick)(void);
extern ENG_LINT (*systemclick)(void);
extern ENG_LINT (*wallclick)(void);

BOOL prolog_runtime(Argdecl);
BOOL prolog_usertime(Argdecl);
BOOL prolog_systemtime(Argdecl);
BOOL prolog_walltime(Argdecl);

BOOL prolog_time(Argdecl);
BOOL prolog_datime(Argdecl);

BOOL prolog_wallclick(Argdecl);
BOOL prolog_userclick(Argdecl);
BOOL prolog_systemclick(Argdecl);
BOOL prolog_runclick(Argdecl);

BOOL prolog_userclockfreq(Argdecl);
BOOL prolog_systemclockfreq(Argdecl);
BOOL prolog_wallclockfreq(Argdecl);
