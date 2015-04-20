%% CIAO syntax
:- use_package(assertions).  
:- comment(nodoc,assertions).  

:- comment(title,"Find out which architecture we are running on").

:- comment(author,"Manuel Carro").
:- comment(author,"Robert Manchek").

:- comment(module,"The architecure and operating system the engine is
compiled for determines whether we can use or not certain libraries.
This script, taken from a PVM distribution, uses a heuristic (which
may need to be tuned from time to time) to find out the platform.  It
returns a string which is used throughout the engine (in #ifdefs) to
enable/disable certain characteristics.

   @section{Usage (ciao_get_arch)}

   @begin{verbatim}
   Usage: ciao_get_arch
   @end{verbatim}

   @section{More details (ciao_get_arch)}

   Look at the script itself...

").


main.
