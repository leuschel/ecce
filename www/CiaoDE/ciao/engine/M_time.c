/* Copyright (C) 1995, Swedish Institute of Computer Science. */

/* This is the default microtime() call for all host types that
   does not support micro second resolution clocks. */

#include "S_config.h"

static char ignore;

#if MUSE

#include "M_types.h"
#include "M_time.h"


#ifndef microtime

#include <sys/time.h>

void microtime(usec)
     unsigned int *usec;
{
  struct timeval tp;
  struct timezone tzp;

  gettimeofday(&tp,&tzp);

  *usec = tp.tv_sec*1000000+tp.tv_usec;
}

#endif /* !microtime */

#endif /* MUSE */
