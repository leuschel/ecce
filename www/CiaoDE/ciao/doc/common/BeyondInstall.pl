:- use_package(assertions).

:- comment(title,"Beyond installation").

:- comment(author,"Manuel Carro").
:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(module,"

@section{Architecture-specific notes and limitations} 

@cindex{limitations, architecture-specific}
Ciao makes use of advanced characteristics of modern architectures and
operating systems such as multithreading, shared memory, sockets,
locks, dynamic load libraries, etc., some of which are sometimes
not present in a given system and others may be implemented in very
different ways across the different systems.  As a result, currently
not all Ciao features are available in all supported operating
systems. Sometimes this is because not all the required features are
present in all the OS flavors supported and sometimes because we
simply have not had the time to port them yet.

The current state of matters is as follows:

@begin{description}
@item{LINUX:}     multithreading, shared DB access, and locking working.
@item{Solaris:}   multithreading, shared DB access, and locking working.
@item{IRIX:}  multithreading, shared DB access, and locking working.
@item{SunOS 4:}   multithreading, shared DB access, and locking NOT working.
@item{Win 95/98/NT/2000/XP:} multithreading, shared DB access, and locking working. Dynamic linking of object code (C) libraries NOT working.   
@item{Mac OS X (Darwin):}  multithreading, shared DB access, and locking working. 
@end{description}

The features that do not work are disabled at compile time.  

@include{MailWWWBugs.lpdoc}

"). 

main.
