:- module(hrtimer,[],[foreign_interface]).

:- use_module(library('hrtimer/hrtimea')).

:- comment(title,"Redefiner for statistics using hrtimer").

:- comment(author,"Edison Mera").

:- comment(module,"

@section{Implementing correct time benchmarking for profiling tools.}

The aim of this work is to show some techniques that improves the
results of benchmarks, and fit them to the task of profiling.

By now, when I need to proof some computer process, the time
measurement used is the millisecond.  However, the problem of that
approach is that most of the systems are doted with low resolution
calendar-clocks, some C functions such as time, or clock, returns the
time with a resolution of milliseconds, but the right is that the
resolution is about 0.01 seconds, and the number of milliseconds
returned have the last digit unaccurate.  A correct benchmarking for
profiling systems, must fit some requirements, such as:

@begin{enumerate}

@item Be independent of the plattform speed.  That is, the objective
   of the benchmark is to measure the time of execution of any piece
   of software independently of the plattform used to do the proof.

@item Do use of the high capacity and speed of modern computers, by
   this way, at most power computing, one could espect better results,
   and for the same benckmark, the improvement of the hardware
   platform must be reflected in a improvement of the performance of
   the profiling tool.

@end{enumerate}

For the first requirement, the measurement unit suggested is one CPU
clock cycle.  These measure is relatively independent to the hardware,
and for estimate the seconds used in execute some task, we must divide
the number of CPU cycles by the velocity in Hertzios of the computer.
For example, if some task takes 1.000.000.000 of cpu cycles, then the
taken time in a 2GHz modern computer is 0.5 seconds.

The second requirement, is solved fortunately with the same approach.
A improvement in the hardware will be reflected in a better
performance of the profiling tool.  In the example above, if we use a
4GHz computer, then the task will take 0.25 seconds.

But there are some issues: modern computers could execute more than 1
instruction by clock cycle, some laptops could slow down your cpu
clock to improve power managment, and there are a few of optimizations
such as the unsorted excecution, that are the benchmarks imprecise,
and for that, make completely hardware independent test is a challenge
task.

Now, the pentium II and highter processors have an instruction RDTSC,
that returns the number of cpu clock since the last power on of the
computer.  These information could be used to do the time measurement
in the benchmarks.

The next module, let us to redefine the traditional benchmark time
measurement with this new focus, and proof that this focus could
impact positively the development of any profiling tool for our
ciao/ciaopp system, and by extension, all systems that requires
profiling.

At this moment, this library has been implemented using a third part
software written in C, you can check these softare at:

@uref{http://www.cs.wisc.edu/paradyn/libhrtime/}

").

:- initialization(init_hrtimer(true)).

:- foreign_inline(
"#include \"hrtime.h\"
#include \"datadefs.h\"
#include \"timing_defs.h\"

ENG_LINT internal_userclick_hrtime(void)
{
  hrtime_t r;
  get_hrutime_self(&r);
  return r;
}

ENG_LINT internal_systemclick_hrtime(void)
{
  hrtime_t r;
  get_hrstime_self(&r);
  return r;
}

").

:- true pred init_hrtimer(go(Error)) :: int + (foreign, returns(Error)).
:- foreign_inline(init_hrtimer/1,
"long init_hrtimer(void) {
  long r;
  if(hrtime_is_present()) {
    r = hrtime_init();
    userclick = internal_userclick_hrtime;
    systemclick = internal_systemclick_hrtime;
    stats.userclockfreq = stats.systemclockfreq = stats.wallclockfreq;
    reset_statistics();
    return r;
  }
  else {
    printf(\"{WARNING: The kernel don't support hrtime, nothing has been done}\\n\");
    return 0;
  }
}
").
