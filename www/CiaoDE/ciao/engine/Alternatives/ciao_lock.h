/* Copyright (C) 1995, Swedish Institute of Computer Science. */

/* M_lock.h - Lock implementation for different hosttypes */

#if NO_LOCKS                                                /* Fake locks */

typedef int _lock_t;

# define init_lock(dummy)
# define _lock(dummy)
# define un_lock(dummy)
# define test_lock(dummy) 1
# define try_lock(dummy) 1

#else                                       /* We want to have real locks */


/* Machine dependent macros */

# if HOST_sun4 && !lint
typedef VOLATILE int _lock_t;
#  if __GNUC__
#   define asm_swap(adr,reg)					\
  ({ int _ret;							\
     asm VOLATILE ("swap %1,%0"					\
	: "=r" (_ret), "=m" (*(adr))	/* Output %0,%1 */	\
	: "m"  (*(adr)), "0" (reg));	/* Input (%2),%0 */	\
     _ret;							\
  })
#  endif
#  define try_lock(p)	(asm_swap((p),1)==0)
# endif /* sun4 */


# if HOST_symmetry && !lint
typedef VOLATILE char _lock_t;
#  if !__GNUC__
#include <parallel/parallel.h>
#   define try_lock(p) S_CLOCK(p)
#  else /* __GNUC__ */
#   define asm_swap(adr,reg)					\
  ({ char _ret;							\
     asm VOLATILE ("xchgb %0,%1"				\
	: "=q" (_ret), "=m" (*(adr))	/* Output %0,%1 */	\
	: "m"  (*(adr)), "0"  (reg));	/* Input (%2),%0 */	\
     _ret;							\
  })
#   define try_lock(p)	(asm_swap((p),1)==0)
#  endif /* __GNUC__ */
# endif /* symmetry */

# if HOST_sgi
typedef VOLATILE int _lock_t;
#  if __GNUC__
#   define asm_ll(adr)						\
  ({ int _ret;							\
     asm VOLATILE ("ll %0,%1"					\
	: "=r" (_ret), "=m" (*(adr))	/* Output %0,%1 */	\
	: "m"  (*(adr)));		/* Input (%2) */	\
     _ret;							\
  })
#   define asm_sc(adr,reg)					\
  ({ int _ret;							\
     asm VOLATILE ("sc %0,%1"					\
	: "=r" (_ret), "=m" (*(adr))	/* Output %0,%1 */	\
	: "m"  (*(adr)), "0" (reg));	/* Input (%2),%0 */	\
     _ret;							\
  })
#  endif
#  define asm_swap(p,v)						\
  ({ int _ret;							\
     do { _ret = asm_ll(p); } while(! asm_sc(p, v));		\
     _ret;							\
  })
#  define try_lock(p)	(asm_swap((p),1)==0)
# endif


#if lint
#define try_lock(p) (*(p)=1)
#endif


/* Default lock routines if nothing else is defined.  The host
   specific parts above must provide at least the macro try_lock.  This
   macro shall try to take the lock and return true at success. */

/* Test if a lock is free.  */
#ifndef test_lock
# define test_lock(p)	(*(p)==0)
#endif

/* Initialize a lock. */
#ifndef init_lock
# define init_lock(p)	(*(p)=0)
#endif


/* Take a lock. */
#ifndef _lock
# define _lock(p)							\
do { if (try_lock(p)) break;						\
     {{ t_lock_wait(); }}						\
     {{ t_debug(__FILE__,__LINE__); }}					\
     while(*(p)==1) continue;						\
     {{ t_lock_ready(); }}						\
   } while(1)
#endif

/* Untake a lock. */
#ifndef un_lock
# define un_lock(p)	(*(p)=0)
#endif

#endif /* LOCKS */
