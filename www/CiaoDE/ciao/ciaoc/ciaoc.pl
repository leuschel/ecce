:- module(ciaoc, [main/1], [assertions]).

:- use_module(library(libpaths), [get_alias_path/0]).
:- use_module(library('compiler/exemaker'),
        [make_exec/2, make_actmod/2, force_lazy/1, dynamic_search_path/1]).
:- use_module(library(compiler), [make_po/1, use_module/3]).
:- use_module(engine(internals), ['$bootversion'/0]).

:- comment(title,"The stand-alone command-line compiler").
:- comment(author, "Daniel Cabeza").
:- comment(author, "The CLIP Group").

:- comment(copyright,"
Copyright @copyright{} 1996-2002 Daniel Cabeza/The CLIP Group.

@include{Copyright.Manuals}
").

:- comment(module,"@cindex{compiling, from command line}
@cindex{compiler, standalone} @apl{ciaoc} @cite{ciaoc-entcs} is the
Ciao stand-alone command-line compiler.  @apl{ciaoc} can be used to
create executables or to compile individual files to object code (to
be later linked with other files).  @apl{ciaoc} is specially useful
when working from the command line. Also, it can be called to compile
Ciao programs from other tools such as, e.g., @concept{shell scripts},
@file{Makefile}s, or @concept{project files}. All the capabilities of
@apl{ciaoc} are also available from the interactive top-level shell,
which uses the ciaoc modules as its components. 

@section{Introduction to building executables}

An @index{executable} can be built from a single file or from a
collection of inter-related files. In the case of only one file, this
file must define the predicate @pred{main/0} or @pred{main/1}. This
predicate is the one which will be called when the executable is
started. As an example, consider the following file, called
@tt{hello.pl}:

@begin{verbatim}
main :-
     write('Hello world'), 
     nl.
@end{verbatim}

@noindent To compile it from the command line using the @apl{ciaoc}
standalone compiler it suffices to type ``@tt{ciaoc hello}'' (in Win32
you may have to put the complete path to the @file{ciaoc} folder of
the Ciao distribution, where the installation process leaves a
@file{ciaoc.bat} file):

@begin{verbatim}
/herme@@clip:/tmp
[60]> ciaoc hello

/herme@@clip:/tmp
[61]> 
@end{verbatim}

@noindent This produces an executable called @tt{hello} in Un*x-like
systems and @tt{hello.cpx} under Win32 systems.  This executable can
then be run in Win32 by double-clicking on it and on Un*x systems by
simply typing its name (see for @ref{Running executables from the
command line} for how to run executables from the command line in
Win32):

@begin{verbatim}
/herme@@clip:/tmp
[61]> hello
Hello world

@end{verbatim}

If the application is composed of several files the process is
identical. Assume @tt{hello.pl} is now:

@begin{verbatim}
:- use_module(aux,[p/1]).

main :-
     p(X),
     write(X), 
     nl.
@end{verbatim}

@noindent
where the file @tt{aux.pl} contains:

@begin{verbatim}
:- module(aux,[p/1]).

p('Hello world').
@end{verbatim}

@noindent This can again be compiled using the @apl{ciaoc} standalone
compiler as before:

@begin{verbatim}
/herme@@clip:/tmp
[60]> ciaoc hello

/herme@@clip:/tmp
[61]> hello
Hello world

@end{verbatim}

@noindent The invocation of @tt{ciaoc hello} compiles the file
@tt{hello.pl} and all connected files that may need recompilation --
in this case the file @tt{aux.pl}. Also, if any library files used had
not been compiled previously they would be compiled at this point (See
@ref{Intermediate files in the compilation process}). Also, if, say,
@tt{hello.pl} is changed and recompiled, the object code resulting
from the previous compilation of @tt{aux.pl} will be reused.  This is
all done without any need for @tt{Makefile}s, and considerably
accelerates the development process for large applications. This
process can be observed by selecting the @tt{-v} option when invoking
@tt{ciaoc} (which is equivalent to setting the
@tt{verbose_compilation} Prolog flag to @tt{on} in the top-level
interpreter).

If @pred{main/1} is defined instead of @pred{main/0} then when the
executable is started the argument of @pred{main/1} will be
instantiated to a list of atoms, each one of them corresponding to a
command line option. Consider the file @tt{say.pl}:

@begin{verbatim}
main(Argv) :-
     write_list(Argv), nl.

write_list([]).
write_list([Arg|Args]) :- 
     write(Arg),
     write(' '),
     write_list(Args).
@end{verbatim}

@noindent Compiling this program and running it results in the
following output:

@begin{verbatim}
/herme@@clip:/tmp
[91]> ciaoc say

/herme@@clip:/tmp
[91]> say hello dolly
hello dolly 
@end{verbatim}

The name of the generated executable can be controlled with the
@tt{-o} option (See @ref{Usage (ciaoc)}).

@section{Paths used by the compiler during compilation}

The compiler will look for files mentioned in commands such as
@decl{use_module/1} or @decl{ensure_loaded/1} in the current
directory.  Other paths can be added by including them in a file whose
name is given to @tt{ciaoc} using the @tt{-u} option. This file should
contain facts of the predicates @pred{file_search_path/2} and
@pred{library_directory/1} (see the documentation for these predicates
and also @ref{Customizing library paths and path aliases}
for details).

@section{Running executables from the command line}
@cindex{executables, how to run} 

As mentioned before, what the @tt{ciaoc} compiler generates and how it
is started varies somewhat from OS to OS.  In general, the product of
compiling an application with @tt{ciaoc} is a file that contains the
bytecode (the product of the compilation) and invokes the
@concept{Ciao engine} on it.

@begin{itemize} 

@item Un Un*x this is a @em{script} (see the first lines of the file)
      which invokes the ciao engine on this file. To run the generated
      executable from a Un*x shell, or from the @apl{bash} shell that
      comes with the Cygwin libraries (see @ref{Installation and
      compilation under Windows}) it suffices to type its name at the
      shell command line, as in the examples above.

@item In a Win32 system, the compiler produces a similar file with a
      @tt{.cpx} ending. The Ciao installation process typically makes
      sure that the Windows registry contains the right entries so 
      that this executable will run upon double-cliking on it. 

      In you want to run the executable from the command line an
      additional @tt{.bat} file is typically needed. To help in doing
      this, the Win32 installation process creates a @tt{.bat}
      skeleton file called @tt{bat_skel} in the @file{Win32} folder of
      the distribution) which allows running Ciao executables from the
      command line.  If you want to run a Ciao executable
      @tt{file.cpx} from the command line, you normally copy the
      skeleton file to the folder were the executable is and rename it
      to @tt{file.bat}, then change its contents as explained in a
      comment inside the file itself.  

      Note that this @tt{.bat} file is usually not necessary in NT, as
      its command shell understands file extension associations. I.e.,
      in windows NT it is possible to run the @tt{file.cpx} executable
      directly. Due to limitations of @tt{.bat} files in Windows 
      95/98, in those OSs no more than 9 command line arguments can be
      passed to the executable (in NT there is no such restriction).

      Finally, in a system in which Cygnus Win32 is installed
      executables can also be used directly from the @apl{bash} shell
      command line, without any associated @tt{.bat} files, by simply
      typing their name at the @apl{bash} shell command line, in the
      same way as in Un*x. This only requires that the @apl{bash}
      shell which comes with Cygnus Win32 be installed and accessible:
      simply, make sure that @file{/bin/sh.exe} exists.

@end{itemize}

Except for a couple of header lines, the contents of executables are
almost identical under different OSs (except for self-contained ones).
The bytecode they contain is architecture-independent.  In fact, it
is possible to create an executable under Un*x and run it on Windows
or viceversa, by making only minor modifications (e.g., creating the 
@tt{.bat} file and/or setting environment variables or editing the
start of the file to point to the correct engine location).

@section{Types of executables generated}

@cindex{executables, types}

While the default options used by @apl{ciaoc} are sufficient for
normal use, by selecting other options @apl{ciaoc} can generate
several different types of executables, which offer interesting
tradeoffs among size of the generated executable, portability, and
startup time @cite{ciaoc-entcs}:

@begin{description}

@item{Dynamic executables:} @cindex{executables, dynamic}

  @apl{ciaoc} produces by default @em{dynamic} executables. In this
  case the executable produced is a @concept{platform-independent}
  file which includes in compiled form all the user defined files. On
  the other hand, any system libraries used by the application are
  loaded dynamically at startup. More precisely, any files that appear
  as @tt{library(...)} in @decl{use_module/1} and
  @decl{ensure_loaded/1} declarations will not be included explicitly
  in the executable and will instead be loaded dynamically.  Is is also
  possible to mark other @concept{path aliases} (see the documentation
  for @pred{file_search_path/2}) for dynamic loading by using the
  @tt{-d} option. Files accessed through such aliases will also be
  loaded dynamically.

  Dynamic loading allows making smaller executables. Such executables
  may be used directly in the same machine in which they were
  compiled, since suitable paths to the location of the libraries will
  be included as default in the executable by @apl{ciaoc} during
  compilation.

  The executable can also be used in another machine, even if the
  architecture and OS are different. The requirement is that the Ciao
  libraries (which will also include the appropriate @concept{Ciao
  engine} for that architecture and OS) be installed in the target
  machine, and that the @tt{CIAOLIB} and @tt{CIAOENGINE} environment
  variables are set appropriately for the executable to be able to
  find them (see @ref{Environment variables used by Ciao
  executables}).  How to do this differs slightly from OS to OS. 

@item{Static executables:} @cindex{executables, static}

  Selecting the @tt{-s} option @tt{ciaoc} produces a @em{static}
  executable. In this case the executable produced (again a
  @concept{platform-independent} file) will include in it all the
  auxiliary files and any system libraries needed by the
  application. Thus, such an executable is almost complete, needing in
  order to run only the @concept{Ciao engine}, which is
  platform-specific.@footnote{Currently there is an exception to this
  related to libraries which are written in languages other than
  Prolog, as, e.g., C. C files are currently always compiled to
  dynamically loadable object files (@tt{.so} files), and they thus
  need to be included manually in a distribution of an
  application. This will be automated in upcoming versions of the Ciao
  system.} Again, if the executable is run in the same machine in
  which it was compiled then the engine is found automatically. If the
  executable is moved to another machine, the executable only needs
  access to a suitable engine (which can be done by setting the
  @tt{CIAOENGINE} environment variable to point to this engine).

  This type of compilation produces larger executables, but has the
  advantage that these executables can be installed and run in a
  different machine, with different architecture and OS, even if Ciao
  is not installed on that machine. To install (or distribute) such an
  executable, one only needs to copy the executable file itself and
  the appropriate engine for the target platform (See @ref{Installing
  Ciao from the source distribution} or @ref{Installing Ciao from a
  Win32 binary distribution} and @ref{Multiarchitecture support}), and
  to set things so that the executable can find the
  engine. @footnote{It is also possible to produce real standalone
  executables, i.e., executables that do not need to have an engine
  around. However, this is not automated yet, although it is planned
  for an upcoming version of the compiler. In particular, the compiler
  can generate a @tt{.c} file for each @tt{.pl} file. Then all the
  @tt{.c} files can be compiled together into a real executable (the
  engine is added one more element during link time) producing a
  complete executable for a given architecture. The downside of course
  is that such an executable will not be portable to other
  architectures without recompilation.}

@item{Dynamic executables, with lazy loading:} @cindex{executables, lazy load}

  Selecting the @tt{-l} option is very similar to the case of dynamic
  executables above, except that the code in the library modules is
  not loaded when the program is started but rather it is done during
  execution, the first time a predicate defined in that file is
  called. This is advantageous if a large application is composed of
  many parts but is such that typically only some of the parts are
  used in each invocation. The Ciao preprocessor, @apl{ciaopp}, is a
  good example of this: it has many capabilitites but typically only
  some of them are used in a given session. An executable with lazy
  load has the advantage that it starts fast, loading a minimal
  functionality on startup, and then loads the different modules
  automatically as needed.

@item{Self-contained executables:} @cindex{executables, self-contained} 

  @em{Self-contained} executables are static executables (i.e., this
  option also implies @em{static} compilation) which include a Ciao
  engine along with the bytecode, so they do not depend on an external
  one for their execution.  This is useful to create executables which
  run even if the machine where the program is to be executed does not
  have a Ciao engine installed and/or libraries. The disadvantage is
  that such execuatbles are @concept{platform-dependent} (as well as
  larger than those that simply use an external library).  This type
  of compilation is selected with the @tt{-S}
  option. Cross-compilation is also possible with the @tt{-SS} option,
  so you can specify the target OS and architecture (e.g. LINUXi86).
  To be able to use the latter option, it is necessary to have
  installed a ciaoengine for the target machine in the Ciao library
  (this requires compiling the engine in that OS/architecture and
  installing it, so that it is available in the library). 

@item{Compressed executables:} @cindex{executables, compressed}

  In @em{compressed} executables the bytecode is compressed. This
  allows producing smaller executables, at the cost of a slightly
  slower startup time. This is selected with the @tt{-z} option. You
  can also produce compressed libraries if you use @tt{-zl} along with
  the @tt{-c} option.  If you select @tt{-zl} while generating an
  executable, any library which is compiled to accomplish this will be
  also compressed.

@item{Active modules:} @cindex{modules, active} 

  The compiler can also compile (via the @tt{-a} option) a given file
  into an @index{active module} (see
  @ref{Active modules (high-level distributed execution)}
  for a description of this).  

  @comment{The way the ... using address publish module of
  name @var{PublishMod} (which needs to be in the library paths).}

@end{description}

@section{Environment variables used by Ciao executables}

    The executables generated by the Ciao compiler (including the ciao
development tools themselves) locate automatically where the Ciao
engine and libraries have been installed, since those paths are stored
as defaults in the engine and compiler at installation time. Thus,
there is no need for setting any environment variables in order to
@em{run} Ciao executables (on a single architecture -- see 
@ref{Multiarchitecture support} for running on multiple
architectures).

   However, the default paths can be overridden by using the
environment variables @tt{CIAOENGINE} and @tt{CIAOLIB}.  The first one
will tell the Ciao executables where to look for an engine, and the
second will tell them where to look for the libraries.  Thus, it is
possible to actually use the Ciao system without installing it by
setting these variables to the following values: @begin{itemize}

@item @tt{CIAOENGINE}:  @tt{$(CIAOSRC)/bin/$(CIAOARCH)/ciaoengine}

@item @tt{CIAOLIB}:     @tt{$(CIAOSRC)}

@end{itemize} 
@noindent
where @tt{$(CIAOARCH)} is the string echoed by the command
@tt{CIAOSRC/etc/ciao_get_arch} (or @tt{BINROOT/ciao_get_arch}, after
installation).

   This allows @concept{using alternate engines or libraries}, which
can be very useful for system development and experimentation.

@section{Intermediate files in the compilation process}

Compiling an individual source (i.e., @tt{.pl}) file produces a
@tt{.itf} file and a @tt{.po} file. The @tt{.itf} file contains
information of the @index{modular interface} of the file, such as
information on exported and imported predicates and on the other
modules used by this module. This information is used to know if a
given file should be recompiled at a given point in time and also to
be able to detect more errors statically including undefined
predicates, mismatches on predicate charaterictics across modules,
etc. The @tt{.po} file contains the platform-independent object code
for a file, ready for linking (statically or dynamically).

It is also possible to use @tt{ciaoc} to explicitly generate the
@tt{.po} file for one or more @tt{.pl} files by using the @tt{-c}
option. 

@section{Usage (ciaoc)}

The following provides details on the different command line options
available when invoking @apl{ciaoc}:

@sp{2}

@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}
").



main(Args) :-
        get_alias_path,
        handle_args(Args).

handle_args([A|As]) :-
        handle_args_(A, As).
handle_args([]) :-
        usage,
        halt(1).

handle_args_('-h', _) :- !,
        usage.
handle_args_('-v', Args) :- !,
        set_prolog_flag(verbose_compilation, on),
        handle_args(Args).
handle_args_('-ri', Args) :- !,
        set_prolog_flag(itf_format, r),
        handle_args(Args).
handle_args_('-x', Args) :- !,
        set_prolog_flag(check_libraries, on),
        handle_args(Args).
handle_args_('-u', [CFile|Args]) :- !,
        use_module(CFile, all, c_itf_internal),
        handle_args(Args).
handle_args_('-u', []) :-
        usage.
handle_args_('-c', Args) :- !,
        verbose_version,
        make_po(Args).
handle_args_('-s', Args) :- !,
        set_prolog_flag(executables, static),
        handle_args(Args).
handle_args_('-S', Args) :- !,
        set_prolog_flag(executables, static),
        get_os(Os),
        get_arch(Arch),
        atom_concat(Os,Arch,Target),
        set_prolog_flag(self_contained,Target),
        handle_args(Args).
handle_args_('-SS',[Target|Args]) :- !,
        set_prolog_flag(executables, static),
        set_prolog_flag(self_contained,Target),
        handle_args(Args).
handle_args_('-SS',[]) :- !,
        usage.
handle_args_('-z', Args) :- !,
        set_prolog_flag(compress_exec,yes),
        handle_args(Args).
handle_args_('-zl',Args) :- !,
        set_prolog_flag(compress_lib,yes),
        handle_args(Args).
handle_args_('-e', Args) :- !,
        set_prolog_flag(executables, eagerload),
        handle_args(Args).
handle_args_('-l', Args) :- !,
        set_prolog_flag(executables, lazyload),
        handle_args(Args).
handle_args_('-ll', [Module|Args]) :- !,
        set_prolog_flag(executables, lazyload),
        force_lazy(Module),
        handle_args(Args).
handle_args_('-ll', []) :- !,
        usage.
handle_args_('-d', [Path|Args]) :- !,
        assertz_fact(dynamic_search_path(Path)),
        handle_args(Args).
handle_args_('-d', []) :- !,
        usage.
handle_args_('-o', [ExecName,File|Files]) :- !,
        verbose_version,
        make_exec([File|Files], ExecName).
handle_args_('-o', _) :- !,
        usage.
handle_args_('-a', [PublishMod,Module]) :- !,
        verbose_version,
        make_actmod(Module, PublishMod).
handle_args_('-a', _) :- !,
        usage.
handle_args_(File, Files) :-
        verbose_version,
        make_exec([File|Files],_ExecName).

verbose_version :-
        current_prolog_flag(verbose_compilation, on), !,
        '$bootversion'.
verbose_version.

usage :-
    '$bootversion',
    usage_message(Usage),
    message(['Usages:\n',$$(Usage)]).

usage_message("\
ciaoc <MiscOpts> <ExecOpts> [-o <execname>] <file> ...

  Make an executable from the listed files.  If there is
  more than one file, they must be non-module, and the
  first one must include the main predicate.  The -o
  option allows generating an arbitrary executable name.

ciaoc <MiscOpts> <ExecOpts> -a <publishmod> <module>

  Make an active module executable from <module> with
  address publish module <publishmod>.

ciaoc <MiscOpts> -c  <file> ...

  Compile listed files (make .po objects).

<MiscOpts> can be: [-v] [-ri] [-u <file>]

-v  verbose mode

-ri generate human readable .itf files

-u  use <file> for compilation

<ExecOpts> can be: [-s|-S|-SS <target>|-z|-zl|-e|-l|(-ll <module>)*]
                   (-d <alias>)* [-x]

-s  make a static executable (otherwise dynamic files are not included)

-S  make standalone executable for the current OS and architecture

-SS make standalone executable for <target> OS and architecture
    valid <target> values may be: LINUXi86, SolarisSparc...

    (both -S and -SS imply -s)

-z  generate executables with compressed bytecode

-zl generate libraries with compressed bytecode - any library (re)compiled
    as consequence of normal executable compilation will also be affected

-e  make executable with eager load of dynamic files at startup (default)

-l  idem with lazy load of dynamic files (except insecure cases)

-ll force <module> to be loaded lazily,  implies -l

-d  files using this path alias are dynamic (default: library)

-x  Extended recompilation: only useful for Ciao standard library developers

default extension for files is '.pl'
").
