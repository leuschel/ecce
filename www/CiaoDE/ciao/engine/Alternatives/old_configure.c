/* Copyright (C) 1996,1997,1998, UPM-CLIP */

/* First, if we are a BSD system, a line is printed:
 *      #define SP_BSD 1
 * Then, we test for endian-ness and print a line
 *      #define BIGENDIAN <bool>
 * Then, we test whether the top 4 bits matter in memory accesses.
 * We also test what part of the address space that malloc() places
 * its objects in: if the top 4 bits matter, then a line
 *	#define MallocBase <base>
 * is printed, <base> being the top 4 bits for malloc() pointers.
 * CIAO assumes that those 4 bits are always the same.
 * Then, two lines
 *      #define SP_FLOAT_SIGNIF <int>
 *      #define SP_FLOAT_ROUND <float>
 * denoting respectively #significant digits in a float and a number to
 * be used for rounding purposes when floats are printed.
 * Then, a line is printed telling whether the system is being built with
 * native code support.
 */

#if defined(__STDC__)
#define VOLATILE volatile
#define PROTO(argl) argl
#else
#define VOLATILE
#define PROTO(ignore) ()
#endif

#include <unistd.h>
#include <stdio.h>
#include <setjmp.h>
#include <signal.h>

#include "compat.h"

#if defined(__svr4__)                                          /* Solaris */
# include <unistd.h>                                            /* sbrk () */
# if !defined(MALLOC_DEBUG)
#  include <stdlib.h>                                           /* malloc() */
# endif
#else                                                            /* SunOS */
# include <sys/types.h>
# if !defined(MALLOC_DEBUG)
#  include <malloc.h>
# endif
#endif

#if defined(MALLOC_DEBUG)
#include "dmalloc.h"
#endif

int lots = 16384;

jmp_buf buf;

void handler(rc)
     int rc;
{
  longjmp(buf,rc);
}


int main(argc, argv)
     int argc;
     char **argv;
{
  char *ptr;
  register int i, j;
  unsigned long mbase = 1;

  SIGNAL(SIGSEGV, handler);
  SIGNAL(SIGBUS, handler);

  if
#if defined(multimax) || defined(AIX)
    (1)
#else
#if defined(hpux) || defined(m88k) || defined(_SEQUENT_) || defined(__svr4__)
    (0)
#else
    (!access("/usr/include/sys/time.h",0) &&
     !access("/usr/include/sys/resource.h",0))
#endif
#endif
      printf("#define SP_BSD 1\n");
  printf("#define BIGENDIAN %d\n", ((unsigned short *)(&mbase))[1]);

#if defined(sun)
  mbase = 0x0;			/* some models are inconsistent */
#else
  ptr = malloc(lots*sizeof(long));
  mbase = (unsigned long)ptr & 0xf0000000;
  for (j=lots-1; j; --j)
    ((unsigned long *)ptr)[j] = 0x12345678;
  if (setjmp(buf))
#endif
    {
      printf("#define MallocBase 0x%lx\n", mbase);
      goto out;
    }
  for (i=0; i<16; i++)
    {
      ptr += 0x10000000;
      for (j=lots-1; j; --j)
	{
				/* sloppy reads? */
	  if (((unsigned long *)ptr)[j] != 0x12345678)
	    longjmp(buf,1);
				/* sloppy writes? */
	  ((unsigned long *)ptr)[j] = 0x12345678;
	}
    }
 out:
  {
    double f = 1.0;
    
    for (i=1; f+1.0>1.0; i++)
      f/=2.0;
    i = (i*0.3010299956639812)+1; /* #significant digits */
    f = 0.5e-9;			/* rounding factor if above=18 */
    for (j=18; j>i; j--)
      f*=10.0;
    printf("#define SP_FLOAT_SIGNIF %d\n#define SP_FLOAT_ROUND %.16g\n",
	   i, f);
  }
  
  printf("/*kernel=def*/\n");

  exit(0);
}
