/* Copyright (C) 1995, Swedish Institute of Computer Science. */

#if MUSE

#if HOST_symmetry
#include <usclkc.h>
#define microtimeinit()	usclk_init()
#define microtime(t)	do {*t = GETUSCLK();} while (0)
#endif

#if HOST_sun4 && __svr4__
#include <sys/time.h>
#define nanotime(t)	do {*t = gethrtime();} while (0)
#define microtime(t)	do {*t = (unsigned long)(gethrtime()/1000);} while (0)
#endif

#else /* !MUSE */

#define microtime(t)

#endif /* !MUSE */

#ifndef microtimeinit
#define microtimeinit()
#endif
