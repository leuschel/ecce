
%% :- module(modules, [], [assertions]).
:- use_package([assertions]).

:- comment(title, "The module system").

:- comment(author,"Daniel Cabeza").

:- comment(usage, "Modules are an intrinsic feature of CIAO, so nothing
   special has to be done to use them.").

:- comment(module, "Modularity is a basic notion in a modern computer
   language.  Modules allow dividing programs in several parts, which
   have its own independent name spaces.  The module system in CIAO, as
   in many other Prolog implementations, is procedure based.  This means
   that predicate names are local to a module, but functor/atom names in
   data are shared.

   The predicates visible in a module are the predicates defined in that
   module, plus the predicates imported from other modules.  Only
   predicates exported by a module can be imported from other modules.
   The default module of a given predicate name is the local one if the
   predicate is defined locally, else the first module from which the
   predicate is imported.  To refer to a predicate from a module which
   is not the default for that predicate the name has to be module
   @cindex{module qualification}qualified.  A module qualified predicate
   name has the form @var{Module}:@var{Predicate} as in the call
   @tt{debugger:debug_module(M)}.  Note that this does not allow having
   access to predicates not imported, nor defining clauses of other
   modules.

   When a module exports a predicate which is not defined in the module,
   but imported from other module, a @index{bridge predicate} is
   automatically added which connects the exported predicate and the
   imported predicate.  Note that this makes that the exported predicate
   does not inherit the dynamic properties of the imported predicate.

   All predicates defined in files with no module declaration belong to
   a special module called @cindex{user module} @tt{user}, and all are
   implicitly exported.  This allows dividing programs in several files
   without being aware of the module system at all.  Note that this
   feature is only supported for compatibility reasons, being its use
   discouraged.  Many attractive compilation features of CIAO cannot be
   performed in @tt{user} modules.

   The case of multifile predicates (defined with the declaration
   @decl{multifile/1}) is also special.  Multifile predicates can be
   defined by clauses in several modules, and all modules which define a
   predicate as multifile can use that predicate.  The name space of
   multifile predicates is independent, as if they belonged to special
   module @tt{multifile}.

   Every @tt{user} or module file imports implicitly a number of modules
   called @concept{builtin modules}.  They are imported after all other
   importations of the module, allowing thus redefining any of their
   predicates (with the exception of @pred{true/0}) by defining local
   versions or importing them from other modules.  Importing explicitly
   from a builtin module, however, disables the implicit importation of
   the rest (this feature is used by package @lib{library(pure)} to
   define pure prolog code).").


:- true decl module(Name, Exports, Packages)
        : modulename * list(predname) * list(sourcename)
        # "Declares a module of name @var{Name} which exports the
          predicates in @var{Exports}, and uses the packages in
          @var{Packages}.  @var{Name} must match with the name of the
          file where the module resides, without extension.  For each
          source in @var{Packages}, a @concept{package file} is
          included, as if by an @decl{include/1} declaration.  If the
          source is specified with a @concept{path alias}, this is
          the file included, if it is an atom, the library paths are
          searched. Package files provide functionalities by
          declaring imports from other modules, defining operators, new
          declarations, translations of code, etc.

          This directive must appear the first in the file.

          Also, if the compiler finds an unknown declaration as the
          first term in a file, the name of the declaration is regarded
          as a package library to be included, and the arguments of the
          declaration (if present) are interpreted like the arguments of
          @decl{module/3}.".


:- true decl module(Name, Exports) : modulename * list(predname)
        # "Same as directive @decl{module/3}, with an implicit package
          @tt{iso}, which enables to include @concept{ISO-Prolog}
          compatible code (compatibility not 100\% yet).".

:- true decl export(Exports) : list(predname)
        # "Adds @var{Exports} to the set of exported predicates.".

:- true decl use_module(Module, Imports) : sourcename * list(predname)
        # "Specifies that this code imports from the module defined in
          @var{Module} the predicates in @var{Imports}.  The imported
          predicates must be exported by the other module.".

:- true decl use_module(Module) : sourcename
        # "Specifies that this code imports from the module defined in
          @var{Module} all the predicates exported by it.  The previous
          version with the explicit import list is preferred to this as
          it minimizes the chances to have to recompile this code if the
          other module changes.".

:- true decl import(Module, Imports) : modulename * list(predname)
        # "Declares that this code imports from the module with name
          @var{Module} the predicates in @var{Imports}.

          @bf{Important note:} this declaration is intended to be used
          when the current module or the imported module is going to
          be dynamically loaded, and so the compiler does not include
          the code of the imported module in the current executable
          (if only because the compiler cannot know the location of
          the module file at the time of compilation).  For the same
          reason the predicates imported are not checked to be
          exported by @var{Module}.  Its use in other cases is
          strongly discouraged.".

:- true decl meta_predicate(MetaSpecs) : sequence(metaspec)
        # "Specifies that the predicates in @var{MetaSpecs} have
          arguments which represent predicates and thus have to be
          module expanded.  The directive is not mandatory in programs
          which do not use modules.  This directive is defined as a
          prefix operator.".

:- comment(doinclude, modulename/1).

:- prop modulename(M) # "@var{M} is a module name (an atom).".

modulename(M) :- atm(M).

:- comment(doinclude, metaspec/1). %%%%%% add pred(N) %%%%%%

:- comment(metaspec/1, "A meta-predicate specification for a predicate
        is the functor of that predicate applied to atoms which
        represent the kind of module expansion that should be done with
        the arguments.  Possible contents are represented as:

        @begin{itemize}

        @item @tt{goal} This argument will be a term denoting a goal
        (either a simple or complex one) which will be called.  For
        commpatibility reasons it can be named as @tt{:} as well.

        @item @tt{clause} This argument will be a term denoting a clause.

        @item @tt{fact} This argument should be instantiated to a term
        denoting a fact (head-only clause).

        @item @tt{spec} This argument should be instantiated to a predicate
        name, as Functor/Arity.

        @item @tt{pred(@em{N})} This argument should be instantiated to
        a predicate construct to be called by means of a
        @tt{call/@em{N}} predicate call (see @pred{call/2}).  Thus, it
        should be an atom equal to the name of a predicate of arity
        @em{N}, or a structure with functor the name of a predicate of
        arity @em{M} (greater than @em{N}) and with @em{M}-@em{N}
        arguments.

        @item @tt{?,+,-,_} This other values denote that this argument is not
        module expanded.

        @end{itemize}").

:- prop metaspec(M) # "@var{M} is a meta-predicate specification.".

metaspec(M) :- struct(M).
