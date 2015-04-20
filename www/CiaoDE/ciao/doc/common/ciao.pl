
:- use_package(assertions).

:- comment(title,"The Ciao Prolog System").

:- comment(subtitle,
	"@em{A New Generation Multi-Paradigm Programming Environment}").
:- comment(subtitle,"REFERENCE MANUAL").
:- comment(subtitle,"@bf{The Ciao System Documentation Series}").
:- comment(subtitle, "@uref{http://www.ciaohome.org/}").
:- comment(subtitle,"@em{Generated/Printed on:} @today{}").
%% :- comment(subtitle,"@em{Preliminary version printed on:} @today{}").
:- comment(subtitle,"Technical Report CLIP 3/97-@em{<version below>}").

:- comment(author, "@em{Edited by:}").
:- comment(author, "F. Bueno").
:- comment(author, "D. Cabeza").
:- comment(author, "M. Carro").
:- comment(author, "M. Hermenegildo").
:- comment(author, "P. L@'{o}pez").
:- comment(author, "G. Puebla").

:- include(library('ClipAddress')).

:- include(library('Copyright')).

%% :- comment(bug,"Although the documentation is acceptable at this
%%    point, we are still really in beta mode in this regard.").

:- comment(summary,"

      @include{CiaoDesc.lpdoc}

   ").

:- comment(module,"

   @section{About this manual}
   @cindex{status, this manual}

   This is the @em{Reference Manual} for the Ciao Prolog development
   system. It contains basic information on how to install Ciao Prolog
   and how to write, debug, and run Ciao Prolog programs from the
   command line, from inside GNU @apl{emacs}, or from a windowing
   desktop. It also documents all the libraries available in the
   standard distribution.

      @include{AboutLPdoc.lpdoc}

   @section{About the Ciao Prolog development system}
   @cindex{ciao, global description}

      @include{AboutCiao.lpdoc}

   @section{ISO-Prolog compliance versus extensibility}
   @cindex{iso-prolog, compliance}
   @cindex{extensibility}

      @include{Compliance.lpdoc}

   @section{About the name of the System}
   @cindex{why the name Ciao}
   @cindex{Ciao, why this name}

      @include{AboutName.lpdoc}

   @section{Referring to Ciao}
   @cindex{referring to Ciao}
   @cindex{references, to Ciao}

   If you find Ciao or any of its components useful, we would
   appreciate very much if you added a reference to this manual (i.e.,
   the Ciao reference manual @cite{ciao-reference-manual-tr}) in your
   work. The following is an appropriate BiBTeX entry with the
   relevant data:

@noindent
@begin{verbatim}
@includeverbatim{CiaoReference.lpdoc}
@end{verbatim}


   @section{Syntax terminology and notational conventions}
   @cindex{notation}

      @include{Conventions.lpdoc}

   @section{A tour of the manual}
   @cindex{manual, tour}
   @cindex{tour, of the manual}

   The rest of the introductory chapters after this one provide a
   first ``getting started'' introduction for newcomers to the Ciao
   system. The rest of the chapters in the manual are organized into a
   sequence of major parts as follows:

      @subsection{PART I - The program development environment}

         @include{DevEnv.lpdoc}

      @subsection{PART II - The Ciao basic language (engine)}

         @include{Builtins.lpdoc}

      @subsection{PART III - ISO-Prolog library (iso)}

         @include{IsoProlog.lpdoc}

      @subsection{PART IV - Classic Prolog library (classic)}

         @include{ClassicProlog.lpdoc}

      @subsection{PART V - Annotated Prolog library (assertions)}

         @include{AnnotatedProlog.lpdoc}

      @subsection{PART VI - Ciao Prolog library miscellanea}

         @include{MiscProlog.lpdoc}

      @subsection{PART VII - Ciao Prolog extensions}

         @include{ExtendProlog.lpdoc}

      @subsection{PART VIII - Interfaces to other languages and systems}

         @include{Interfaces.lpdoc}

      @subsection{PART IX - Abstract data types}

         @include{ADTs.lpdoc}

      @subsection{PART X - Miscellaneous standalone utilities}

         @include{ciao-utilities.lpdoc}

      @subsection{PART XI - Contributed libraries}

         @include{Contrib.lpdoc}


      @subsection{PART XII - Appendices}

         @include{Append.lpdoc}


   @section{Acknowledgments} 
   @cindex{acknowledgments}

      @include{Acknowledgments.lpdoc}

").

main.

%% --------------------------------------------------------------------------- 

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*13+0,2005/07/03,19:05*53+'CEST'), "Development
   version after 1.12 (Jose Morales)").

:- comment(version(1*12+0,2005/07/03,18:50*50+'CEST'), "Temporary
   version before transition to SVN log comments (Jose Morales)").

:- comment(version(1*11+247,2004/07/02,13:27*33+'CEST'), "Improved
   front cover (old authors are now listed as editors, mention UNM,
   new TR number including system version, pointer to
   @tt{www.ciaohome.org}, mention multi-paradigm, etc.). Also changed
   mention of GPL in summary to LGPL.  (Manuel Hermenegildo)").

:- comment(version(1*11+1,2003/04/04,18:30*31+'CEST'), "New
   development version to begin the builtin modularization (Jose
   Morales)").

:- comment(version(1*10+1,2003/04/04,18:29*07+'CEST'), "Version
   skipped (Jose Morales)").

:- comment(version(1*10+0,2004/07/29,16:12*03+'CEST'), "
   @begin{itemize}
   @item Classical prolog mode as default behavior.
   @item Emacs-based environment improved.
      @begin{itemize}
      @item Improved emacs inferior (interaction) mode for Ciao and CiaoPP.
      @item Xemacs compatibility improved (thanks to A. Rigo).
      @item New icons and modifications in the environment for the 
            preprocessor.
      @item Icons now installed in a separate dir.          
      @item Compatibility with newer versions of @apl{Cygwin}.
      @item Changes to programming environment:
            @begin{itemize}
            @item Double-click startup of programming environment. 
            @item Reorganized menus: help and customization grouped in 
                  separate menus.
            @item Error location extended.
            @item Automatic/Manual location of errors produced when 
                  running Ciao tools now customizable.
            @item Presentation of CiaoPP preprocessor output improved.
            @end{itemize}
      @item Faces and coloring improved:
            @begin{itemize}
            @item Faces for syntax-based highlighting more customizable.
            @item Syntax-based coloring greatly
                  improved. Literal-level assertions also correctly
                  colored now.
            @item Syntax-based coloring now also working on ASCII
                  terminals (for newer versions of emacs).
            @item Listing user-defined directives allowed to be colored in
                  special face.
            @item Syntax errors now colored also in inferior buffers.
            @item Customizable faces now appear in the documentation.
            @item Added new tool bar button (and binding) to refontify
                  block/buffer.
            @item Error marks now cleared automatically also when 
                  generating docs.
            @item Added some fixes to hooks in lpdoc buffer.
            @end{itemize}  
      @end{itemize}
   @item Bug fixes in compiler.
      @begin{itemize}
      @item Replication of clauses in some cases (thanks to S. Craig).
      @end{itemize} 

   @item Improvements related to supported platforms
      @begin{itemize}
      @item Compilation and installation in different palatforms have been 
            improved.
      @item New Mac OS X kernels supported.
      @end{itemize}

   @item Improvement and bugs fixes in the engine:
      @begin{itemize}
      @item Got rid of several segmentation violation problems.
      @item Number of significant decimal digits to be printed now computed 
            accurately.
      @item Added support to test conversion of a Ciao integer into a machine 
            int.
      @item Unbound length atoms now always working.
      @item C interface .h files reachable through a more standard location 
            (thanks to R. Bagnara).
      @item Compatibility with newer versions of gcc.
      @end{itemize}

   @item New libraries and utilities added to the system:
      @begin{itemize}
      @item Factsdb: facts defined in external files can now be automatically 
            cached on-demand.
      @item Symfnames: File aliasing to internal streams added.
      @end{itemize}

   @item New libraries added (in beta state):
      @begin{itemize}
      @item fd: clp(FD)
      @item xml_path: XML querying and transformation to Prolog.
      @item xdr_handle: XDR schema to HTML forms utility.
      @item ddlist: Two-way traversal list library.
      @item gnuplot: Interface to GnuPlot.
      @item time_analyzer: Execution time profiling.
      @end{itemize}

   @item Some libraries greatly improved:
      @begin{itemize}
      @item Interface to Tcl/Tk very improved. 
          @begin{itemize}
          @item Corrected many bugs in both interaction Prolog to
                Tcl/Tk and viceversa.
          @item Execution of Prolog goals from TclTk revamped.
          @item Treatment of Tcl events corrected.
          @item Predicate  @pred{tcl_eval/3} now allows the execution of Tcl 
                procedures running multiple Prolog goals.
          @item Documentation heavily reworked.
          @item Fixed unification of prolog goals run from the Tcl side.
          @end{itemize}
      @item Pillow library improved in many senses.
          @begin{itemize}
          @item HTTP media type parameter values returned are always strings 
                now, not atoms. 
          @item Changed verbatim() pillow term so that newlines are translated 
                to <br>.
          @item Changed management of cookies so that special characters in 
                values are correctly handled. 
          @item Added predicate @pred{url_query_values/2}, reversible. 
                Predicate @pred{url_query/2} now obsolete.
          @item Now attribute values in tags are escaped to handle values 
                which have double quotes.
          @item Improved @pred{get_form_input/1} and @pred{url_query/2} so 
                that names of parameters having unusual characters are always 
                correctly handled.
          @end{itemize}
      @item Fixed bug in tokenizer regarding non-terminated single or 
            multiple-line comments.  When the last line of a file has a 
            single-line comment and does not end in a newline, it is accepted 
            as correct.  When an open-comment /* sequence is not terminated in 
            a file, a syntax error exception is thrown.
      @end{itemize}

   @item Other libraries improved:
      @begin{itemize}
      @item Added native_props to assertions package and included
            @pred{nonground/1}.
      @item In atom2terms, changed interpretation of double quoted strings so 
            that they are not parsed to terms.
      @item Control on exceptions improved.
      @item Added @pred{native/1,2} to basic_props.
      @item Davinci error processing improved.
      @item Foreign predicates are now automatically declared as 
            implementation-defined.
      @item In lists, added @pred{cross_product/2} to compute the cartesian 
            product of a list of lists. Also added 
            @pred{delete_non_ground/3}, enabling deletion of nonground terms 
            from a list. 
      @item In llists added @pred{transpose/2} and changed @pred{append/2} 
            implementation with a much more efficient code. 
      @item The make library has been improved.
      @item In persdb, added @pred{pretractall_fact/1} and 
            @pred{retractall_fact/1} as persdb native capabilities. 
      @item Improved behavior with user environment from persdb.
      @item In persdb, added support for @pred{persistent_dir/4},
            which includes arguments to specify permission modes for
            persistent directory and files.
      @item Some minor updates in persdb_sql.
      @item Added treatment of operators and module:pred calls to
            pretty-printer.
      @item Updated report of read of syntax errors.
      @item File locking capabilities included in @pred{open/3}.
      @item Several improvements in library system.
      @item New input/output facilities added to sockets.
      @item Added @pred{most_specific_generalization/3} and 
            @pred{most_general_instance/3} to terms_check.
      @item Added @pred{sort_dict/2} to library vndict.
      @item The xref library now treats also empty references.
      @end{itemize}

   @item Miscellaneous updates:
      @begin{itemize}
      @item Extended documentation in libraries actmods, arrays, 
            foreign_interface, javall, persdb_mysql, prolog_sys, old_database, 
            and terms_vars.
      @end{itemize}
   @end{itemize}").

:- comment(version(1*9+355,2004/07/02,13:28*02+'CEST'), "Improved
   front cover (old authors are now listed as editors, mention UNM,
   new TR number including system version, pointer to
   @tt{www.ciaohome.org}, mention multi-paradigm, etc.). Also changed
   mention of GPL in summary to LGPL.  (Manuel Hermenegildo)").

:- comment(version(1*9+38,2002/12/12,20:06*26+'CET'), "Manual now
   posted in pdf format (since lpdoc now generates much better pdf).
   (Manuel Hermenegildo)").

:- comment(version(1*9+34,2002/11/30,14:42*45+'CET'), "Installation
   can now be done in Test distribution directory (for testing
   purposes).  (Manuel Hermenegildo)").

:- comment(version(1*9+33,2002/11/30,14:37*10+'CET'), "Modified
   installation site text to make more explicit the fact that we
   support Mac OS X and XP.  (Manuel Hermenegildo)").

:- comment(version(1*9+0,2002/05/16,23:17*34+'CEST'), " New
   development version after stable 1.8p0 (MCL, DCG)").

:- comment(version(1*8+0,2002/05/16,21:20*27+'CEST'), "
   @begin{itemize}
   @item Improvements related to supported platforms:
       @begin{itemize}
       @item Support for Mac OS X 10.1, based on the Darwin kernel.
       @item Initial support for compilation on Linux for Power PC
             (contributed by @index{Paulo Moura}).
       @item Workaround for incorrect C compilation while using newer
             (> 2.95) gcc compilers.
       @item .bat files generated in Windows.
       @end{itemize}
   
   @item Changes in compiler behavior and user interface:
       @begin{itemize}
       @item Corrected a bug which caused wrong code generation in some cases.
       @item Changed execution of initialization directives.  Now the
             initialization of a module/file never runs before the
             initializations of the modules from which the module/file
             imports (excluding circular dependences).
       @item The engine is more intelligent when looking for an engine
             to execute bytecode; this caters for a variety of
             situations when setting explicitly the CIAOLIB
             environment variable.
       @item Fixed bugs in the toplevel: behaviour of @tt{module:main}
             calls and initialization of a module (now happens after
             related modules are loaded).
       @item Layout char not needed any more to end Prolog files.
       @item Syntax errors now disable .itf creation, so that they
             show next time the code is used without change.
       @item Redefinition warnings now issued only when an unqualified call
             is seen. 
       @item Context menu in Windows can now load a file into the toplevel.
       @item Updated Windows installation in order to run CGI
             executables under Windows: a new information item is
             added to the registry.
       @item Added new directories found in recent Linux distributions to
             INFOPATH. 
       @item Emacs-based environment and debugger improved:
           @begin{itemize}
           @item Errors located immediataly after code loading.
           @item Improved ciao-check-types-modes (preprocessor progress
                 now visible). 
           @item Fixed loading regions repeatedly (no more predicate
                 redefinition warnings).
           @item Added entries in @apl{ciaopp} menu to set verbosity of output.
           @item Fixed some additional xemacs compatibility issues
                 (related to searches). 
           @item Errors reported by inferior processes are now
                 explored in forward order (i.e., the first error
                 rewported is the first one highlighted). Improved
                 tracking of errors.
           @item Specific tool bar now available, with icons for main
                 fuctions (works from emacs 21.1 on). Also, other
                 minor adaptations for working with emacs 21.1 and
                 later.
           @item Debugger faces are now locally defined (and better
                 customization). This also improves comtability with xemacs
                 (which has different faces).
           @item Direct access to a common use of the preprocessor
                 (checking modes/types and locating errors) from toolbar.
           @item Inferior modes for Ciao and CiaoPP improved: contextual
                 help turned on by default.
           @item Fixes to set-query. Also, previous query now appears
                 in prompt.
           @item Improved behaviour of stored query.
           @item Improved behaviour of recentering, finding errors, etc.
           @item Wait for prompt has better termination characteristics.
           @item Added new interactive entry points (M-x): ciao,
                 prolog, ciaopp.
           @item Better tracking of last inferior buffer used.
           @item Miscellanous bugs removed; some colors changed to
                 adapt to different Emacs versions.
           @item Fixed some remaining incompatibilities with xemacs.
           @item @tt{:- doc} now also supported and highlighted.
           @item Eliminated need for calendar.el
           @item Added some missing library directives to fontlock
                 list, organized this better.
           @end{itemize}
       @end{itemize}
   
   @item New libraries added to the system:
       @begin{itemize}
       @item hiord: new library which needs to be loaded in order to use
               higher-order call/N and P(X) syntax. Improved model for predicate
               abstractions. 
       @item fuzzy: allows representing fuzzy information in the form or
               Prolog rules.
       @item use_url: allows loading a module remotely by using a WWW
               address of the module source code
       @item andorra: alternative search method where goals which become
               deterministic at run time are executed before others.
       @item iterative deepening (id): alternative search method which makes a
               depth-first search until a predetermined depth is reached.
               Complete but in general cheaper than breadth first.
       @item det_hook: allows making actions when a deterministic
               situation is reached.
       @item ProVRML: read VRML code and translate it into Prolog terms,
               and the other way around.
       @item io_alias_redirection: change where stdin/stdout/stderr point to
               from within Ciao Prolog programs.
       @item tcl_tk: an interface to Tcl/Tk programs.
       @item tcl_tk_obj: object-based interface to Tcl/Tk graphical
       objects.
       @item CiaoPP: options to interface with the CiaoPP Prolog preprocessor.
       @end{itemize}
   
   @item Some libraries greatly improved:
       @begin{itemize}
       @item WebDB: utilities to create WWW-based database interfaces.
       @item Improved java interface implementation (this forced
             renaming some interface primitives). 
       @item User-transparent persistent predicate database revamped:
           @begin{itemize}
           @item Implemented passerta_fact/1 (asserta_fact/1).
           @item Now it is never necessary to explicitly call
                 init_persdb, a call to initialize_db is only needed
                 after dynamically defining facts of persistent_dir/2.
                 Thus, pcurrent_fact/1 predicate eliminated.
           @item Facts of persistent predicates included in the
                 program code are now included in the persistent
                 database when it is created.  They are ignored in
                 successive executions.
           @item Files where persistent predicates reside are now
                 created inside a directory named as the module where
                 the persistent predicates are defined, and are named
                 as F_A* for predicate F/A.
           @item Now there are two packages: persdb and 'persdb/ll'
                 (for low level).  In the first, the standard builtins
                 asserta_fact/1, assertz_fact/1, and retract_fact/1
                 are replaced by new versions which handle persistent
                 data predicates, behaving as usual for normal data
                 predicates.  In the second package, predicates with
                 names starting with 'p' are defined, so that there is
                 not overhead in calling the standard builtins.
           @item Needed declarations for persistent_dir/2 are now
                 included in the packages.
           @end{itemize}
   
       @item SQL now works with mysql.
       @item system: expanded to contain more predicates which act as
             interface to the underlying system /  operating system.  
       @end{itemize}
   
   @item Other libraries improved:
       @begin{itemize}
       @item xref: creates cross-references among Prolog files.
       @item concurrency: new predicates to create new concurrent
             predicates on-the-fly.
       @item sockets: bugs corrected.
       @item objects: concurrent facts now properly recognized.
       @item fast read/write: bugs corrected.
       @item Added 'webbased' protocol for active modules: publication of
             active module address can now be made through WWW.
       @item Predicates in library(dynmods) moved to library(compiler).
       @item Expansion and meta predicates improved.
       @item Pretty printing.
       @item Assertion processing.
       @item Module-qualified function calls expansion improved.
       @item Module expansion calls goal expansion even at runtime.
       @end{itemize}
   
   @item Updates to builtins (there are a few more; these are the most
         relevant):

       @begin{itemize}
       @item Added a prolog_flag to retrieve the version and patch.
       @item current_predicate/1 in library(dynamic) now enumerates
             non-engine modules, prolog_sys:current_predicate/2 no longer
             exists.
       @item exec/* bug fixed.
       @item srandom/1 bug fixed.
       @end{itemize}
   
   @item Updates for C interface:
         @begin{itemize}
         @item Fixed bugs in already existing code.
         @item Added support for creation and traversing of Prolog data
         structures from C predicates.
         @item Added support for raising Prolog exceptions from C
         predicates. 
         @item Preliminary support for calling Prolog from C.
         @end{itemize}
   
   @item Miscellaneous updates:
         @begin{itemize}
         @item Installation made more robust.
         @item Some pending documentation added.
         @item 'ciao' script now adds (locally) to path the place where
         it has been installed, so that other programs can be located
         without being explicitly in the $PATH.
         @item Loading programs is somewhat faster now.
         @item Some improvement in printing path names in Windows.
         @end{itemize}
   @end{itemize}").

:- comment(version(1*7+203,2002/04/20,13:38*54+'CEST'), "Minor changes
   to Ciao description.  (Manuel Hermenegildo)").

:- comment(version(1*7+155,2001/11/24,11:53*36+'CET'), "Minor changes
   to installation scripts to make sure permissions are left correctly
   if installation is aborted.  (Manuel Hermenegildo)").

:- comment(version(1*7+154,2001/11/23,18:02*30+'CET'), "'ciao' script
   now locally adds CIAOBIN path to PATH if not already present
   (MCL)").

:- comment(version(1*7+108,2001/06/02,12:17*18+'CEST'), "Minor bug in
   main Makefile during uninstallation fixed: added rm -f of engine
   Makefile before linking.  (Manuel Hermenegildo)").

:- comment(version(1*7+101,2001/05/15,17:34*09+'CEST'), "Minor error
   in manual fixed: the section explaining the Ciao name did not
   appear.  (Manuel Hermenegildo)").

:- comment(version(1*7+100,2001/05/13,15:48*57+'CEST'), "Added
   @tt{/usr/share/info} to default @tt{INFOPATH} paths.  (Manuel
   Hermenegildo)").

:- comment(version(1*7+87,2001/04/08,15:15*18+'CEST'), "Added @tt{doc}
   and @tt{installdoc} targets to top level installation @{Makefile}
   (can be used to regenerate and reinstall documentation if
   @apl{lpdoc} is available.  (Manuel Hermenegildo)").

:- comment(version(1*7+14,2000/08/29,12:16*12+'CEST'), "Updated COMMON
   to include makefile-sysindep; changed SETLOCAL{CIAOC,CIAOSHELL} to
   SETLOCALCIAO (MCL)").

:- comment(version(1*7+12,2000/08/22,18:16*33+'CEST'), "Changed a bug
   in the installation: the .sta engine was not being copied!
   (MCL)").

:- comment(version(1*7+0,2000/07/12,19:01*20+'CEST'), "Development
   version following even 1.6 distribution.").


:- comment(version(1*6+0,2000/07/12,18:55*50+'CEST'), "
   @begin{itemize}
   @item Source-level debugger in emacs, breakpts.
   @item Emacs environment improved, added menus for Ciaopp and LPDoc.
   @item Debugger embeddable in executables.
   @item Stand-alone executables available for UNIX-like operating
     systems. 
   @item Many improvements to emacs interface.
   @item Menu-based interface to autodocumenter.
   @item Threads now available in Win32.
   @item Many improvements to threads.
   @item Modular clp(R) / clp(Q).
   @item Libraries implementing And-fair breadth-first and iterative
     deepening included.
   @item Improved syntax for predicate abstractions.
   @item Library of higher-order list predicates.
   @item Better code expansion facilities (macros).
   @item New delay predicates (when/2).
   @item Compressed object code/executables on demand.
   @item The size of atoms is now unbound.
   @item Fast creation of new unique atoms.
   @item Number of clauses/predicates essentially unbound.
   @item Delayed goals with freeze restored.
   @item Faster compilation and startup.
   @item Much faster fast write/read. 
   @item Improved documentation.
   @item Other new libraries.
   @item Improved installation/deinstallation on all platforms.
   @item Many improvements to autodocumenter.
   @item Many bug fixes in libraries and engine.
   @end{itemize}").

:- comment(version(1*5+134,2000/05/09,11:52*13+'CEST'), "Changed
   location of suite to examples, updated documentation.  (MCL)").

:- comment(version(1*5+94,2000/03/28,23:19*20+'CEST'), "The manual
   intro now provides an overview of the different parts of the
   manual.  (Manuel Hermenegildo)").

:- comment(version(1*5+0,1999/11/29,16:16*23+'MEST'),"Development
   version following even 1.4 distribution.").

:- comment(version(1*4+0,1999/11/27,19:00*00+'MEST'),"
   @begin{itemize}
   @item Documentation greatly improved.
   @item Automatic (re)compilation of foreign files.
   @item Concurrency primitives revamped; restored &Prolog-like 
         multiengine capability. 
   @item Windows installation and overall operation greatly improved.
   @item New version of O'Ciao class/object library, with improved performance.
   @item Added support for ""predicate abstractions"" in call/N. 
   @item Implemented reexportation through reexport declarations.
   @item Changed precedence of importations, last one is now higher.
   @item Modules can now implicitly export all predicates.
   @item Many minor bugs fixed.
   @end{itemize}").

:- comment(version(1*3+0,1999/06/16,17:05*58+'MEST'), "Development
   version following even 1.2 distribution.").

:- comment(version(1*2+0,1999/06/14,16:54*55+'MEST'), " Temporary
   version distributed locally for extensive testing of reexportation
   and other 1.3 features.").

:- comment(version(1*1+0,1999/06/04,13:30*37+'MEST'), "Development
   version following even 1.0 distribution.").

:- comment(version(1*0+0,1999/06/04,13:27*42+'MEST'), "
   @begin{itemize}
   @item Added Tcl/Tk interface library to distribution.
   @item Added push_prolog_flag/2 and pop_prolog_flag/1 declarations/builtins.
   @item Filename processing in Windows improved.
   @item Added redefining/1 declaration to avoid redefining warnings.
   @item Changed syntax/1 declaration to use_package/1.
   @item Added add_clause_trans/1 declaration.
   @item Changed format of .itf files such that a '+' stands for all
         the standard imports from engine, which are included in c_itf
         source internally (from engine(builtin_exports)).  Further
         changes in itf data handling, so that once an .itf file is
         read in a session, the file is cached and next time it is
         needed no access to the file system is required.
   @item Many bugs fixed.
   @end{itemize}").

:- comment(version(0*9+32,1999/04/05,20:38*17+'MEST'), "Improved
   uninstallation makefiles so that (almost) nothing is left behind.
   (Manuel Hermenegildo)").

:- comment(version(0*9+0,1999/03/10,17:03*49+'CET'), "
   @begin{itemize}
   @item Test version before 1.0 release. Many bugs fixed.
   @end{itemize}").

:- comment(version(0*8+0,1998/10/27,13:12*36+'MET'), "
   @begin{itemize}
   @item Changed compiler so that only one pass is done, eliminated @tt{.dep}
         files.
   @item New concurrency primitives.
   @item Changed assertion comment operator to #.
   @item Implemented higher-order with call/N.
   @item Integrated SQL-interface to external databases with 
         persistent predicate concept. 
   @item First implementation of object oriented programming package.
   @item Some bugs fixed.
   @end{itemize}").

:- comment(version(0*7+0,1998/09/15,12:12*33+'MEST'), "
   @begin{itemize}
   @item Improved debugger capabilities and made easier to use.
   @item Simplified assertion format.
   @item New arithmetic functions added, which complete all ISO functions.
   @item Some bugs fixed.
   @end{itemize}").

:- comment(version(0*6+0,1998/07/16,21:12*07+'MET DST'), "
   @begin{itemize}
   @item Defining other path aliases (in addition to 'library') which can
         be loaded dynamically in executables is now possible.
   @item Added the posibility to define multifile predicates in the shell.
   @item Added the posibility to define dynamic predicates dynamically.
   @item Added addmodule meta-argument type.
   @item Implemented persistent data predicates.
   @item New version of PiLLoW WWW library (XML, templates, etc.).
   @item Ported active modules from ``distributed Ciao'' (independent 
         development version of Ciao).
   @item Implemented lazy loading in executables.
   @item Modularized engine(builtin).
   @item Some bugs fixed.
   @end{itemize}").

:- comment(version(0*5+0,1998/3/23), "
   @begin{itemize}
   @item First Windows version.
   @item Integrated debugger in toplevel.
   @item Implemented DCG's as (Ciao-style) expansions.
   @item Builtins renamed to match ISO-Prolog.
   @item Made ISO the default syntax/package.
   @end{itemize}").

:- comment(version(0*4+0,1998/2/24), "
   @begin{itemize}
   @item First version with the new Ciao emacs mode.
   @item Full integration of concurrent engine and compiler/library.
   @item Added new_declaration/1 directive.
   @item Added modular syntax enhancements.
   @item Shell script interpreter separated from toplevel shell.
   @item Added new compilation warnings.
   @end{itemize}").

:- comment(version(0*3+0,1997/8/20), "
   @begin{itemize}
   @item Ciao builtins modularized.
   @item New prolog flags can be defined by libraries.
   @item Standalone comand-line compiler available, with automatic ""make"".
   @item Added assertions and regular types.
   @item First version using the automatic documentation generator.
   @end{itemize}").

:- comment(version(0*2+0,1997/4/16), "
   @begin{itemize}
   @item First module system implemented.
   @item Implemented exceptions using catch/3 and throw/1.
   @item Added functional & record syntax.
   @item Added modular sentence, term, and goal translations.
   @item Implemented attributed variables.
   @item First CLPQ/CLPR implementation.
   @item Added the posibility of linking external .so files.
   @item Changes in syntax to allow @tt{P(X)} and @tt{""string""||L}.
   @item Changed to be more similar to ISO-Prolog.
   @item Implemented Prolog shell scripts.
   @item Implemented data predicates.
   @end{itemize}").

:- comment(version(0*1+0,1997/2/13), "First fully integrated,
   standalone Ciao distribution. Based on integrating into an
   evolution of the &-Prolog engine/libraries/preprocessor
   @cite{Hampaper,ngc-and-prolog} many functionalities from several
   previous independent development versions of Ciao
   @cite{ciao-prolog-compulog,ciao-ppcp,att-var-iclp,ciao-manual-tr,
   ciao-comp-dist-tr-deliv,ciao-ilps95,ciao-jicslp96-ws-update,pillow-ws,
   ciao-novascience}.").

