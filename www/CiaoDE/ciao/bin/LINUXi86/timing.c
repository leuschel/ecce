/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include <string.h>

#include <sys/types.h>

#include "datadefs.h"
#include "support.h"

/* declarations for global functions accessed here */

#include "timing_defs.h"

/* local declarations */

#if (defined(Solaris) || defined(SunOS4) || defined(LINUX) || defined(DARWIN) || defined(IRIX) || defined(Win32)) && !defined(crossWin32i86)

#include <sys/time.h>
#include <sys/resource.h>

ENG_LINT internal_userclick_std(void)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((ENG_LINT)rusage.ru_utime.tv_sec) * 1000000 + rusage.ru_utime.tv_usec;
}

ENG_LINT internal_systemclick_std(void)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((ENG_LINT)rusage.ru_stime.tv_sec) * 1000000 + rusage.ru_stime.tv_usec;
}

static void init_frequency_info(void)
{
  stats.userclockfreq = 1000000;
  stats.systemclockfreq = 1000000;
}

#else

#include <sys/times.h>
#include <sys/param.h>

ENG_LINT internal_userclick_std(void)
{
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_utime;
}

ENG_LINT internal_systemclick_std(void)
{
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_stime;
}

static void init_frequency_info(void)
{
  stats.userclockfreq = HZ;
  stats.systemclockfreq = HZ;
}
#endif

/* userclick is defined as a pointer to a function to let the
   redefinition on the fly for a better timing measurement function */

ENG_LINT (*userclick)(void) = internal_userclick_std;
ENG_LINT (*systemclick)(void) = internal_systemclick_std;

ENG_FLT usertime(void)
{
  return ((ENG_FLT)userclick()) / stats.userclockfreq;
}

extern time_t time PROTO((time_t *));

BOOL prolog_time(Arg)
     Argdecl;
{
  
  time_t timeofday = time(NULL);

  return cunify(Arg,MakeInteger(Arg,timeofday),X(0));
}


/* walltime(?Time): unifies Time with the time in milliseconds elapsed since
  the last call to walltime/1 . The first call returns walltime since the
  start of the execution.  */

/* Shared but locked?  Initialized in init_once() */

ENG_LINT internal_wallclick_std(void)
{
  struct timeval tp;

  gettimeofday(&tp, 0L);
  return (ENG_LINT)tp.tv_sec*1000000 + ((ENG_LINT)tp.tv_usec);

}

#if defined(i86) && defined(LINUX)
/* more precise timing functions available in x86 plattform: click
  represent the more precise time unit.  In Pentium II and better, it
  is equal to 1 cpu - cycle.
*/

/* Returns the clock speed of the system's CPU in Hz, as reported by
  /proc/cpuinfo. On a multiprocessor machine, returns the speed of the
  first CPU. On error returns zero.

  Reference:

  http://www.informit.com/isapi/product_id~%7B480DF8FB-19B8-4C4E-88B8-FC2BF352887D%7D/content/index.asp

*/

static ENG_LINT cpuspeed (void)
{
  FILE* fp;
  char buffer[4096];
  size_t bytes_read;
  char* match;
  double clock_speed;

  /* Read the entire contents of /proc/cpuinfo into the buffer. */
  fp = fopen ("/proc/cpuinfo", "r");
  bytes_read = fread (buffer, 1, sizeof (buffer), fp);
  fclose (fp);
  /* Fail if read failed or if buffer isn't big enough. */
  if (bytes_read == 0 || bytes_read == sizeof (buffer))
    return 0;
  /* NUL-terminate the text. */
  buffer[bytes_read] = '\0';
  /* Locate the line that starts with "cpu MHz". */
  match = strstr (buffer, "cpu MHz");
  if (match == NULL)
    return 0;
  /* Parse the line to extract the clock speed. */
  sscanf (match, "cpu MHz : %lf", &clock_speed);
  return (ENG_LINT)(clock_speed * 1000000);
}

/* Note that "cpuid" could be necessary to force the processor not do the */
/* "speculative execution". */

ENG_LINT internal_wallclick_tsc(void)
{
/*   volatile unsigned long long k; */
/*   volatile unsigned long *parts = (volatile unsigned long *) &k; */
/* /\*   __asm__ __volatile__("cpuid"); *\/ */
/*   __asm__ __volatile__("rdtsc" : "=a" ( parts[0] ), "=d" (  parts[1] )) ; */
/*   return k; */
  return get_timestamp();
}

ENG_LINT (*wallclick)(void) = internal_wallclick_tsc;

void init_statistics(void) {
  init_frequency_info();
  stats.wallclockfreq = cpuspeed();
  if(stats.wallclockfreq==0) {
    // by now, if this method fail, use the standar method
    stats.wallclockfreq = 1000000;
    wallclick = internal_wallclick_std;
  }
}

#else

ENG_LINT (*wallclick)(void) = internal_wallclick_std;

void init_statistics(void) {
  init_frequency_info();
  stats.wallclockfreq = 1000000;
}
#endif

/*
  This function returns the walltime in milliseconds
*/

ENG_FLT walltime(void)
{
  return ((ENG_FLT)wallclick() * 1000) / stats.wallclockfreq;
}

/*
  This function has been modified by Edison Mera to prevents the
  truncation of the microseconds.  Very important in modern
  plattforms where the speed is given in GHz.
 */
static BOOL generic_time(
  Argdecl,
  ENG_LINT (*time_function)(void),
  ENG_LINT startclick,
  ENG_LINT *lastclick,
  ENG_LINT clockfreq)
{
  /*int st,lt; */
  ENG_LINT st,lt;
  ENG_LINT t;
  TAGGED x;
  
  t = time_function();
  st = t - startclick;
  lt = t - *lastclick;
  *lastclick = t;
  /* while ciao not support ENG_LINT, lt and st must be cast to
    ENG_FLT */
  MakeLST(x,MakeFloat(Arg,(((ENG_FLT)lt)*1000)/clockfreq),atom_nil);
  MakeLST(x,MakeFloat(Arg,(((ENG_FLT)st)*1000)/clockfreq),x);
  return cunify(Arg,x,X(0));  
}

/* runtime returns a list of two floats
 * giving time in milliseconds. The first number gives time from the system 
 * start_up and the second since the last call to runtime */
BOOL prolog_runtime(Arg)
     Argdecl;
{
  return generic_time(Arg, userclick, stats.startclick, &stats.lastclick, stats.userclockfreq);
}

BOOL prolog_usertime(Arg)
     Argdecl;
{
  return generic_time(Arg, userclick, stats.startuserclick, &stats.lastuserclick, stats.userclockfreq);
}

BOOL prolog_systemtime(Arg)
     Argdecl;
{
  return generic_time(Arg, systemclick, stats.startsystemclick, &stats.lastsystemclick, stats.systemclockfreq);
}

BOOL prolog_walltime(Arg)
     Argdecl;
{
  return generic_time(Arg, wallclick, stats.startwallclick, &stats.lastwallclick, stats.wallclockfreq);
}

/* New time medition functions */
static BOOL generic_click(
  Argdecl,
  ENG_LINT (*time_function)(void),
  ENG_LINT startclick,
  ENG_LINT *lastclick)
{
  /*int st,lt; */
  ENG_LINT st,lt;
  ENG_LINT t;
  TAGGED x;
  
  t = time_function();
  st = t - startclick;
  lt = t - *lastclick;
  *lastclick = t;
  /* while ciao not support ENG_LINT, lt and st must be cast to
    ENG_FLT */
  MakeLST(x,MakeFloat(Arg,(ENG_FLT)lt),atom_nil);
  MakeLST(x,MakeFloat(Arg,(ENG_FLT)st),x);
  return cunify(Arg,x,X(0));  
}

BOOL prolog_wallclick(Arg)
     Argdecl;
{
  return generic_click(Arg, wallclick, stats.startwallclick, &stats.lastwallclick);
}

BOOL prolog_userclick(Arg)
     Argdecl;
{
  return generic_click(Arg, userclick, stats.startuserclick, &stats.lastuserclick);
}

BOOL prolog_systemclick(Arg)
     Argdecl;
{
  return generic_click(Arg, systemclick, stats.startsystemclick, &stats.lastsystemclick);    
}

BOOL prolog_runclick(Arg)
     Argdecl;
{
  return generic_click(Arg, userclick, stats.startclick, &stats.lastclick);
}

/* New time medition functions */
inline static BOOL generic_clockfreq(
  Argdecl,
  ENG_LINT clockfreq)
{
  /* while ciao not support ENG_LINT, return value must be cast to
    ENG_FLT */
  return cunify(Arg,MakeFloat(Arg,(ENG_FLT)clockfreq),X(0));
}

BOOL prolog_userclockfreq(Arg)
     Argdecl;
{
  return generic_clockfreq(Arg, stats.userclockfreq);
}

BOOL prolog_systemclockfreq(Arg)
     Argdecl;
{
  return generic_clockfreq(Arg, stats.systemclockfreq);
}

BOOL prolog_wallclockfreq(Arg)
     Argdecl;
{
  return generic_clockfreq(Arg, stats.wallclockfreq);
}

void reset_statistics(void)
{
  stats.lastclick = stats.startclick = userclick();
  stats.startwallclick = wallclick();
  stats.lastwallclick = stats.startwallclick;
  stats.startuserclick = stats.startclick; /*userclick();*/
  stats.lastuserclick = stats.startuserclick;
  stats.startsystemclick = systemclick();
  stats.lastsystemclick = stats.startsystemclick;
}

/* datime(+Time,-Year,-Month,-Day,-Hour,-Min,-Sec,-WeekDay,-YearDay) */

BOOL prolog_datime(Arg)
     Argdecl;
{
  struct tm *datime;
  time_t inputtime[1];

  DEREF(X(0),X(0));

  if (IsVar(X(0))) {
    inputtime[0] = time(NULL);
    cunify(Arg,MakeInteger(Arg,inputtime[0]),X(0));
  } else if (IsInteger(X(0))) {
    inputtime[0] = GetInteger(X(0));
  } else
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);

  datime = localtime(inputtime);

  return(cunify(Arg,MakeSmall((datime->tm_year)+1900),X(1))
      && cunify(Arg,MakeSmall((datime->tm_mon)+1), X(2))
      && cunify(Arg,MakeSmall(datime->tm_mday),X(3))
      && cunify(Arg,MakeSmall(datime->tm_hour),X(4))
      && cunify(Arg,MakeSmall(datime->tm_min), X(5))
      && cunify(Arg,MakeSmall(datime->tm_sec), X(6))
      && cunify(Arg,MakeSmall(datime->tm_wday),X(7))
      && cunify(Arg,MakeSmall(datime->tm_yday),X(8)));

}
