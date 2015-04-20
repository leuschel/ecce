/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* Obtain the value of system and architecture parameters that do not
   change from run to run, so that those values can be static data
   during native code compilation */

#if defined(__svr4__) || defined(Solaris) || defined(DARWIN)
# include <unistd.h>                                            /* sbrk () */
# include <stdlib.h>                                         /* malloc() */
#else                                                            /* SunOS */
# include <sys/types.h>
# include <malloc.h>
#endif
#include <stdio.h>
#include <unistd.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <math.h>

/* Import basic engine definitions */
#include "compat.h"
#include "termdefs.h"                          /* because of TAGGED (MCL) */
#include "own_malloc_defs.h"

/* ------------------------------------------------------------------------- */
/* configure__fpbits:
 *
 * ENG_FLT_SIGNIF <int>: number of significant digits in a float
 * ENG_FLT_ROUND <float>: a number to be used for rounding purposes
 *   when floats are printed
 *  
 * (so that display(5.347) exactly shows 5.347)
 */

/*
  safe_addition is intended to force A and B to be stored prior to doing the
  addition of A and B , for use in situations where optimizers might hold
  one of these in a register.

  There is a problem with gcc (3.1, at least): when using -O3, which turns
  inlining on, the number of digits in the (decimal) mantissa returned is
  20, due to the inlining of the safe_addition function, and the further
  optimization this brings about.  In order to avoid this in general, I have
  put the volatile keyword which tells the optimizer not to throw away
  references to the ret_val variable (since it may be read/written to by
  other threads :-), even if safe_addition is inlined.
*/

ENG_FLT safe_addition(volatile ENG_FLT *a, volatile ENG_FLT *b) {
  volatile ENG_FLT ret_val;
  ret_val = *a + *b;
  return ret_val;
} 

/* Computing the accuracy of floats - Changed so that display(5.347) = 5.347 */

void find_fp_bits(ENG_INT *t) {
  volatile static ENG_FLT one = 1.0;
  static ENG_INT base = 2; 

  /* 'base' was originally found out from the implementation of the FPU/FP
     routines; it is tipically 2 in mos FP implementations.  I suppose,
     then, that we can choose whatever base is appropriate for us and use
     the loop below to determine the number of significant digits in
     (pseudo-) base 10. */
    
  volatile ENG_FLT a, c, tmp1;
  volatile ENG_INT lt;

  lt = 0;
  a = c = one;
  while (c == one) {
    ++lt;
    a *= base;
    c = safe_addition(&a, &one);
    tmp1 = -a;
    c = safe_addition(&c, &tmp1);
  }
  *t = lt;
} 

void get_mask_descr(int size,
		    unsigned long *lx,
		    unsigned long *ly,
		    unsigned long *mask,
		    unsigned int *indx,
		    unsigned int *shft) {
  for(*indx=0; *indx<size; (*indx)++) {
    *mask = lx[*indx] ^ ly[*indx];
    if(*mask) {
      *shft = 0;
      while(((*mask >> *shft) & (unsigned long)1)==0) {
	(*shft)++;
      }
      return;
    }
  }
  *indx=0;
  *shft=0;
  *mask=0;
}

/* This function will calculate some values related to the internal
   representation of the double numbers according with the ieee754
   standard */

void configure_ieee754() {
  double x, y, z;
  unsigned long *lx, *ly, *lz;
  unsigned long mask;
  unsigned int index;
  unsigned int shift;
  int i;
  int size = sizeof(x) / sizeof(mask);

  lx = (unsigned long *)&x;
  ly = (unsigned long *)&y;
  lz = (unsigned long *)&z;

  x = 1;
  y = -x;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE754_MASK_NEGATIVE  0x%08lX\n", mask);
  printf("#define IEEE754_INDEX_NEGATIVE %d\n", index);
  printf("#define IEEE754_SHIFT_NEGATIVE %d\n", shift);

  x = 1;
  y = 2;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE754_MASK_EXPONENT  0x%08lX\n", mask);
  printf("#define IEEE754_INDEX_EXPONENT %d\n", index);
  printf("#define IEEE754_SHIFT_EXPONENT %d\n", shift);

  x = 1;
  y = 1;
  z = 1;
  /* This have 20 bits */
  for (i=1; i<=20; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE754_MASK_MANTISSA0  0x%08lX\n", mask);
  printf("#define IEEE754_INDEX_MANTISSA0 %d\n", index);
  printf("#define IEEE754_SHIFT_MANTISSA0 %d\n", shift);

  /* This have 32 bits */
  z = x;
  for (i=1; i<=32; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE754_MASK_MANTISSA1  0x%08lX\n", mask);
  printf("#define IEEE754_INDEX_MANTISSA1 %d\n", index);
  printf("#define IEEE754_SHIFT_MANTISSA1 %d\n", shift);
  printf("#define IEEE754_MANTISSA_LENGTH 52\n\n");
}

void configure_ieee854() {
  long double x, y, z;
  unsigned long *lx, *ly, *lz;
  unsigned long mask;
  unsigned int index;
  unsigned int shift;
  unsigned int split;
  unsigned int mantissa0_length;
  int i;
  int size = sizeof(x) / sizeof(mask);

  lx = (unsigned long *)&x;
  ly = (unsigned long *)&y;
  lz = (unsigned long *)&z;

  if (size==2) {
    mantissa0_length=20;
  } else {
    printf("#define USE_LONG_DOUBLE\n");
    mantissa0_length=31;
  }

  /* It is necessary to init to 0 to avoid garbage in the unused bits: */

  for (i = 0; i < size; i++) {
    lx[i] = 0;
    ly[i] = 0;
    lz[i] = 0;
  }

  /* printf("/\* x=0x%08lX:0x%08lX:0x%08lX *\/\n", lx[0],lx[1],lx[2]); */
  /* printf("/\* y=0x%08lX:0x%08lX:0x%08lX *\/\n", ly[0],ly[1],ly[2]); */
  /* printf("/\* z=0x%08lX:0x%08lX:0x%08lX *\/\n", lz[0],lz[1],lz[2]); */

  x = 1;
  y = -x;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE854_MASK_NEGATIVE   0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_NEGATIVE  %d\n", index);
  printf("#define IEEE854_SHIFT_NEGATIVE  %d\n", shift);

  x = 1;
  y = 2;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE854_MASK_EXPONENT   0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_EXPONENT  %d\n", index);
  printf("#define IEEE854_SHIFT_EXPONENT  %d\n", shift);

  x = 1;
  y = 1;
  z = 1;

  /* This have mantissa0_length bits (plus a fixed bit if 31) */

  y /= 2;
  x += y;
  get_mask_descr(size, lx, lz, &mask, &index, &split);
  split++;
  for (i=1; i < split; i++) {
    y /=2;
    x +=y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA0_0  0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA0_0 %d\n", index);
  printf("#define IEEE854_SPLIT_MANTISSA0_0 %d\n", mantissa0_length - split);

  z = x;
  for (i=split; i<mantissa0_length; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA0_1  0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA0_1 %d\n", index);
  printf("#define IEEE854_SHIFT_MANTISSA0_1 %d\n", shift);

  lx[index] = lx[index] | (mask << shift);

  /* This have 32 bits */
  z = x;
  y /= 2;
  x += y;
  get_mask_descr(size, lx, lz, &mask, &index, &split);
  split++;
  for (i=1; i < split; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA1_0  0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA1_0 %d\n", index);
  printf("#define IEEE854_SPLIT_MANTISSA1_0 %d\n", 32 - split);

  z = x;
  for(i=split; i < 32; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA1_1  0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA1_1 %d\n", index);
  printf("#define IEEE854_SHIFT_MANTISSA1_1 %d\n", shift);

  printf("#define IEEE854_MANTISSA_LENGTH %d\n\n", mantissa0_length + 32);
}

void configure__fpbits() {
  ENG_INT bits;
  double f;    
  int i, j;

  find_fp_bits(&bits);

  i = (bits*0.301029995663981); /* #significant digits, bits*log_10(2) */

  f = 0.5e-9;			/* rounding factor if above 18 */
  for (j=18; j>i; j--)
    f*=10.0;

  printf("#define ENG_FLT_SIGNIF %d\n", i);
  printf("#define ENG_FLT_ROUND %.*g\n\n", i, f);

  configure_ieee754();
  configure_ieee854();
}

/* ------------------------------------------------------------------------- */
/* configure__endianness: Obtain system endianness
 * 
 * BIGENDIAN: 1 if the system is big-endian, 0 if the system is little-endian
 */

void configure__endianness() {
  union {
    unsigned short as_short[2];
    TAGGED as_tagged;
  } u;
  u.as_tagged = 1;
  printf("#define BIGENDIAN %d\n", (int)u.as_short[1]);
}

/* ------------------------------------------------------------------------- */
/* configure__alloc: Memory management configuration
 *
 * MallocBase: 0xM0000000 where M are the top 4 bits for pointers
 *   returned by malloc()
 * MIN_MEM_ALLOC: minimum amount of memory that makes malloc return
 *   pointers in that region
 *
 * Some systems (namely, LINUX) allocate memory in different parts of
 * the memory depending on how much we ask.  The result is that blocks
 * can be scattered so that the "unmutable four top bits" assumption
 * is broken (i.e., the mapped memory can't be fit into the pointer
 * part of a tagged word) and MallocBase is not useful at all.  We try
 * to find out dynamically if this ever happens, and at which point.
 * This will serve to calculate the minimum amount of memory to be
 * requested at a time from the system, and a (hopefully) correct
 * MallocBase.
 *  
 * If we, anyway, choose to build a system without snooping for a
 * good MallocBase, just use the first pointer the system returns.
 */

#if defined(LINUX)
#  define USE_OWN_MALLOC 1
//#  define USE_MMAP 1
#endif

#if defined(USE_OWN_MALLOC) && defined(USE_MMAP)
#include <unistd.h>
#include <sys/mman.h>
#endif

#define MIN_MEM_BLOCK_CHARS 16384

#define BIGTAGMASK 0xf0000000
#define ALIGN sizeof(TAGGED)                        /* Minimum block size */
#define CHARS_TO_TW(Chars) ((Chars)%ALIGN==0 ? (Chars)/ALIGN : (Chars)/ALIGN+1)
#define MIN_MEM_BLOCK (unsigned int)(CHARS_TO_TW(MIN_MEM_BLOCK_CHARS))

void configure__alloc() {
#if defined(USE_OWN_MALLOC)
#if defined(USE_MMAP)
  /* Use mmap to allocate blocks of memory */
  /* todo: this is not complete */
  //int pagesize = getpagesize();
  //printf("#define MMAP_PAGESIZE %d\n", pagesize);
  printf("#define USE_MMAP 1\n");
  printf("#define MallocBase 0x40000000\n");
  printf("#define MIN_MEM_ALLOC %d\n", MIN_MEM_BLOCK);
#else
  /* Use system malloc to allocate blocks of memory */
  TAGGED malloc_base;
  int *chunk;
  int min_mem_alloc;
  int size;
  /* Obtain turn point */ 
  size = ALIGN;
  while(1) {
    chunk = (int *)malloc(size);
    if (chunk == NULL) {
      /* BIGTAGMASK is never 0, use MIN_MEM_BLOCK */
      malloc_base = (TAGGED)0;
      min_mem_alloc = MIN_MEM_BLOCK;
      break;
    } else {
      if (((TAGGED)chunk & BIGTAGMASK) == 0) {
	/* not yet non-zero, continue */
        size *= 2;
        free(chunk);
      } else {
	/* Use that one, assume that there will be no more changes in
	   the upper bits */
        malloc_base = (TAGGED)chunk & BIGTAGMASK;
        free(chunk);
	int tagged_lots = CHARS_TO_TW(size);
	min_mem_alloc = (tagged_lots > MIN_MEM_BLOCK ?
			 tagged_lots : MIN_MEM_BLOCK);
        break;
      }
    }
  }
  printf("#define MallocBase 0x%lx\n", malloc_base);
  printf("#define MIN_MEM_ALLOC %d\n", min_mem_alloc);
#endif
  printf("#define USE_OWN_MALLOC 1\n");
#else /* !defined(USE_OWN_MALLOC) */
  /* Trust that the malloc implementation gives pointers in the
     0xMmmmmmmm region, where 0xM0000000 is the base */
  TAGGED malloc_base;
  int *chunk;
  chunk = (int *)malloc(ALIGN);
  malloc_base = (TAGGED)chunk & BIGTAGMASK;
  free(chunk);
  printf("#define MallocBase 0x%lx\n", malloc_base);
#endif
}

/* ------------------------------------------------------------------------- */

/* SunOs does not include strsep  */
#if defined(SunOS4) || defined(Solaris) || defined(IRIX)
/*      $NetBSD: strsep.c,v 1.8 1998/10/13 20:32:09 kleink Exp $        */
/*-
 * Copyright (c) 1990, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Get next token from string *stringp, where tokens are possibly-empty
 * strings separated by characters from delim.  
 *
 * Writes NULs into the string at *stringp to end tokens.
 * delim need not remain constant from call to call.
 * On return, *stringp points past the last NUL written (if there might
 * be further tokens), or is NULL (if there are definitely no more tokens).
 *
 * If *stringp is NULL, strsep returns NULL.
 */
char *strsep(stringp, delim)
     char **stringp;
     const char *delim;
{
  char *s;
  const char *spanp;
  int c, sc;
  char *tok;
  
  if ((s = *stringp) == NULL) return (NULL);
  for (tok = s;;) {
    c = *s++;
    spanp = delim;
    do {
      if ((sc = *spanp++) == c) {
        if (c == 0) s = NULL;
        else s[-1] = 0;
        *stringp = s;
        return (tok);
      }
    } while (sc != 0);
  }
}
#endif

void generate_defines(char *cflags) {
  char *Dpointer;
  char *definition;
  char *macroname = NULL;
  char *definition_value;

  Dpointer = cflags;
  while (Dpointer && (Dpointer = strstr(Dpointer, "-D"))) {
    Dpointer += 2;
    if ((definition = strsep(&Dpointer, " "))) {
      definition_value = definition;
      macroname = strsep(&definition_value, "=");
    }
    if (definition_value)
      printf("#if !defined(%s)\n#define %s %s\n#endif\n\n", 
             macroname, macroname, definition_value);
    else
      printf("#if !defined(%s)\n#define %s\n#endif\n\n", 
             macroname, macroname);
  }
}

/* ------------------------------------------------------------------------- */
/* Call all configuration parts */

int main(int argc, char **argv) {
  if (argc > 0) generate_defines(argv[1]);
  configure__endianness();
  configure__alloc();
  configure__fpbits();
  return 0;
}


