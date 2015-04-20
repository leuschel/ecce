:- module(hrtimea,[
%	main/0,
	hrtime_is_present/1,
	get_hrtimef/2,
	get_hrvtimef/2,
	get_hrutimef/2,
	get_hrstimef/2,
	get_current_hrtimef/1,

	get_hrtime_selff/1,
	get_hrvtime_selff/1,
	get_hrutime_selff/1,
	get_hrstime_selff/1,

	hrtime_initl/1,
	get_hrtime_structl/3,
	free_hrtime_structl/2],[foreign_interface]).

% ===========================================================================
:- comment(title, "High Resolution Time Adapter for Prolog.").
:- comment(author, "Edison Mera").
% ===========================================================================
% 2004-02-25

:- comment(module,

"This module is an adatper for use in prolog the functions
availables in the library hrtime

This information has been written by Edison Mera, and complements the
official documentation found in the involved software.

@subsection{Installation Instructions for patch the Kernel with the
hrtime library}

To install this patch in the Kernel:

@begin{enumerate}

@item @bf{Uncompress the kernel}.  A recommended place to do that is
      /usr/src.

@item Copy the patch hrtime-0.6.1-2.4.25.patch in the directory
      /usr/src.

@item Now will We suppose that the kernel are in the directory
      /usr/src/linux-2.4.25.  enter into that directory and execute
      the following:

      patch -p1 < ../hrtime-0.6.1-2.4.25.patch

@item Your kernel must be patched now.

@item @bf{Configuring and compiling the Kernel}. More detailed
      information is available in the README file of the kernel, Is
      hightly recommended that you read it before begin.  But in
      addition to that, is hightly recommended that you use the
      original configuration file that comes with your operating
      system.  Normally (RedHat, Mandrake, etc...), It can be found in
      the directory /boot.  And also, you must enable the options
      related with the hrtime library.  In addition to that, to avoid
      problems with the crypto library, you can deactivate It in the
      kernel configuration menu.

      Resuming, in an ideal case, you must simply apply the next
      commands:

      a) Configuring Kernel:     'make xconfig'.

      b) Making dependencies:    'make dep'.

      c) Compiling the kernel:   'make bzImage'.

      d) Compiling the modules:  'make modules'.

      d) A work around:          'mkdir /lib/modules/2.4.25'.

      e) Installing the kernel:  'make install'.

      f) Installing the modules: 'make modules_install'.

@item Due to some undocumented problem in the kernel, It is possible
      that the file /boot/initrd-2.4.25.img have been created
      incorrectly.  You must recreate that executing:

      mv /boot/initrd-2.4.25.img /boot/initrd-2.4.25.img.old
      mkinitrd /boot/initrd-2.4.25.img 2.4.25

@item First Verify that the file /boot/grub/menu.lst have been
      generated correctly.  Be sure that if something leaves bad, you
      can always back to the previous kernel and solve the problem.

@item Restart the system and cross your fingers.

@item The patch is now ready to be used at Kernel Level.

@end{enumerate}

@subsection{Installation Instructions to install the library that let
us to acces the timming functions.}

Now is necessary to install the library that the access to the
timming functions.

@begin{enumerate}

@item Install the library rpm package:

      rpm -i libhrtime-0.6.1-1.i386.rpm

@item It is possible that the binaries files recently installed needs to
      be recompiled.  To do that, follow the next steps:

      a) Decompress the file libhrtime-0.6.1.tar.gz.

      b) In the subdir src, copy the file Makefile that are in this
         directory.

      c) do make all
     
      d) as root, do make install

@item If everything is Ok, The system must work now.

@end{enumerate}

").

:- use_foreign_library(c).
:- use_foreign_source([library('hrtimer/hrtime')]).

%:- use_foreign_library(hrtime).

:- foreign_inline("#include \"hrtime.h\"\n\n").

:- true pred hrtime_is_present(go(Value)) :: int +
   (foreign,returns(Value)).

% the f means that the result is double
:- true pred get_hrtimef(in(Hr), go(Dest)) :: address * num + (foreign).
:- foreign_inline(get_hrtimef/2,
"void get_hrtimef(struct hrtime_struct *hr, double *dest) {
  hrtime_t r;
  get_hrtime(hr, &r);
  *dest = (double)r; 
}
").

:- true pred get_hrvtimef(in(Hr), go(Dest)) :: address * num +
   (foreign).

:- foreign_inline(get_hrvtimef/2,
"void get_hrvtimef(struct hrtime_struct *hr, double *dest) {
  hrtime_t r;
  get_hrvtime(hr, &r);
  *dest = (double)r; 
}

").

:- true pred get_hrutimef(in(Hr), go(Dest)) :: address * num +
   (foreign).

:- foreign_inline(get_hrutimef/2,
"void get_hrutimef(struct hrtime_struct *hr, double *dest) {
  hrtime_t r;
  get_hrutime(hr, &r);
  *dest = (double)r; 
}

").

:- true pred get_hrstimef(in(Hr), go(Dest)) :: address * num +
   (foreign).

:- foreign_inline(get_hrstimef/2,
"void get_hrstimef(struct hrtime_struct *hr, double *dest) {
  hrtime_t r;
  get_hrstime(hr, &r);
  *dest = (double)r; 
}

").

:- true pred get_current_hrtimef(go(Dest)) :: num + (foreign).

:- foreign_inline(get_current_hrtimef/1,
"void get_current_hrtimef(double *dest) {
  hrtime_t r;
  get_current_hrtime(&r);
  *dest = (double)r; 
}

").

:- true pred get_hrtime_selff(go(Dest)) :: num + (foreign).

:- foreign_inline(get_hrtime_selff/1,
"void get_hrtime_selff(double *dest) {
  hrtime_t r;
  get_hrtime_self(&r);
  *dest = (double)r;
}

").

:- true pred get_hrvtime_selff(go(Dest)) :: num + (foreign).

:- foreign_inline(get_hrvtime_selff/1,
"void get_hrvtime_selff(double *dest) {
  hrtime_t r;
  get_hrvtime_self(&r);
  *dest = (double)r;
}

").

:- true pred get_hrutime_selff(go(Dest)) :: num + (foreign).

:- foreign_inline(get_hrutime_selff/1,
"void get_hrutime_selff(double *dest) {
  hrtime_t r;
  get_hrutime_self(&r);
  *dest = (double)r;
}

").

:- true pred get_hrstime_selff(go(Dest)) :: num + (foreign).

:- foreign_inline(get_hrstime_selff/1,
"void get_hrstime_selff(double *dest) {
  hrtime_t r;
  get_hrstime_self(&r);
  *dest = (double)r;
}

").

% the l means that the result is long integer

:- true pred hrtime_initl(go(Error)) :: int +
(foreign,returns(Error)).

:- foreign_inline(hrtime_initl/0,
"long hrtime_initl() {
  return hrtime_init();
}

").

:- true pred get_hrtime_structl(in(Pid), go(Dest), go(Error)) :: int *
   address * int + (foreign,returns(Error)).

:- foreign_inline(get_hrtime_struct/3,
"long get_hrtime_structl(pid_t pid, struct hrtime_struct **dest) {
  return get_hrtime_struct(pid, dest);
}

").

:- true pred free_hrtime_structl(in(Hr), go(Error)) :: address * int +
   (foreign, returns(Error)).

:- foreign_inline(free_hrtime_structl/2,
"long free_hrtime_structl(struct hrtime_struct *hr) {
  return free_hrtime_struct(hr);
}

").

% main :-
% 	hrtime_initl(_E1),
% 	get_hrtime_structl(0,Ts,_E2),
% 	display(Ts),nl,
% 	get_hrstimef(Ts,A),
% 	display(A),nl,
% 	get_hrstimef(Ts,B),
% 	display(B),nl,
% 	free_hrtime_structl(Ts,_E3).
