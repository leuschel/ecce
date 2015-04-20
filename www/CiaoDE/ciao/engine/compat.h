
 /* Parts of this, from MUSE source code */

 /*  Supply __svr4__ symbol if not given by the compiler */

#if defined(Solaris) && !defined(__svr4__)
# define __svr4__
#endif

#if defined(linux) && !defined(LINUX)
#define LINUX
#endif

#if defined(sun)
# define HOST_sun 1
# if sparc
#  define HOST_sun4 1
# endif
#endif

#if defined(sequent) && defined(i386)
# define HOST_symmetry 1
#endif

#if defined(sgi) && defined(mips)
# define HOST_sgi 1
#endif

#if defined(SYMM)
# define LONGJMP(Env, Val)   longjmp(Env, Val)
# define SETJMP(Env)         setjmp(Env)
# define JMP_BUF             jmp_buf
#else                                                         /* Not SYMM */
# if defined(Solaris)
#   define JMP_BUF           jmp_buf
#   define SETJMP(Env)       setjmp(Env)
#   define LONGJMP(Env, Val) longjmp(Env, Val)
# else 
#  if defined(crossWin32i86)            
#  define JMP_BUF           jmp_buf
#  define SETJMP(Env)       setjmp(Env)
#  define LONGJMP(Env, Val) longjmp(Env, Val)
#  else
#  define JMP_BUF           sigjmp_buf
#  define SETJMP(Env)       sigsetjmp(Env, 1)
#  define LONGJMP(Env, Val) siglongjmp(Env, Val)
#  endif
# endif
#endif

#if !defined(__svr4__)
#define SIGNAL signal
#define SIGBLOCK(SIG) sigblock(sigmask(SIG))
#define SIGPAUSE_0() sigpause(0)
#else                                                         /* __svr4__ */
#define SIGNAL(SIG,HDL)				\
do {						\
  struct sigaction act;				\
						\
  sigemptyset(&act.sa_mask);			\
  act.sa_flags = 0 | SA_NODEFER;		\
  act.sa_handler = HDL;				\
						\
  sigaction(SIG,&act,(struct sigaction *)NULL);	\
} while (0)

#define SIGBLOCK(SIG)					\
do {							\
  sigset_t block_set;					\
							\
  sigemptyset(&block_set);				\
  sigaddset(&block_set, SIG);				\
  sigprocmask(SIG_BLOCK, &block_set, (sigset_t *)NULL);	\
} while (0)

#define SIGPAUSE_0()	\
do {			\
  sigset_t set;		\
			\
  sigemptyset(&set);	\
  sigsuspend(&set);	\
} while (0)

#endif /* __svr4__ */

#if defined(__svr4__) || defined(LINUX)
#define UNBLOCK_SIG(Sig)                 \
{ sigset_t mask;                         \
  sigemptyset(&mask);                    \
  sigaddset(&mask, (Sig));               \
  sigprocmask(SIG_UNBLOCK, &mask, NULL); \
}
#else
#define UNBLOCK_SIG(Sig)
#endif

