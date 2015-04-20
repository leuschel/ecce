%% CIAO syntax
:- use_package([assertions]).  

:- comment(title,"Compile Ciao in an arbitrary subdirectory").

:- comment(author,"M. Carro").

:- comment(module,"A simple bash script for
   @concept{compile the whole Ciao distribution},
   in order to test automatically whether something
   is broken.

   @section{Usage (auto_compile_ciao)}

   @begin{verbatim}
   Usage: auto_compile_ciao ciaodir_name destdir_name
   @end{verbatim}

   @section{More details (auto_compile_ciao)}

The script copies the ciao distribution passed on as first argument to
a subdirectory of the directory name passed as second argument.  The
subdirectory is always the same, in order to speed up tests made
repeteadly in a distribution.  It also deletes from the destination
files which do not appear any longer in the sources, and tries not to
copy files which will eventually be generated or deleted prior to
compilation.

After that, the @tt{SETTINGS} file is updated to reflect the new
location, the sources are cleaned up, and a regular compilation is
started.

").


main.
