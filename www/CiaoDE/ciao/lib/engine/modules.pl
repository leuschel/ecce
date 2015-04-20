
:- module(modules, [], [assertions]).

:- comment(title, "The module system").

:- comment(author,"Daniel Cabeza and the CLIP Group").

:- comment(usage, "Modules are an intrinsic feature of Ciao, so nothing
   special has to be done to use them.").

:- comment(summary, "Modularity is a basic notion in a modern computer
   language.  Modules allow dividing programs in several parts, which
   have their own independent name spaces.").

:- comment(module, "Modularity is a basic notion in a modern computer
   language.  Modules allow dividing programs in several parts, which
   have its own independent name spaces.  The module system in Ciao
   @cite{ciao-modules-cl2000}, as in many other Prolog implementations,
   is procedure based.  This means that predicate names are local to a
   module, but functor/atom names in data are shared.

   The predicates visible in a module are the predicates defined in that
   module, plus the predicates imported from other modules.  Only
   predicates exported by a module can be imported from other modules.
   The default module of a given predicate name is the local one if the
   predicate is defined locally, else the last module from which the
   predicate is imported, having explicit imports priority (that is, a
   predicate imported by an @tt{use_module/2} declaration is always
   preferred above a predicate imported by an @tt{use_module/1}
   declaration).  To refer to a predicate from a module which is not the
   default for that predicate the name has to be module @cindex{module
   qualification}qualified.  A module qualified predicate name has the
   form @var{Module}:@var{Predicate} as in the call
   @tt{debugger:debug_module(M)}.  Note that this does not allow having
   access to predicates not imported, nor defining clauses of other
   modules.

   All predicates defined in files with no module declaration belong to
   a special module called @cindex{user module} @tt{user}, and all are
   implicitly exported.  This allows dividing programs in several files
   without being aware of the module system at all.  Note that this
   feature is only supported for compatibility reasons, being its use
   discouraged.  Many attractive compilation features of Ciao cannot be
   performed in @tt{user} modules.

   The case of multifile predicates (defined with the declaration
   @decl{multifile/1}) is also special.  Multifile predicates can be
   defined by clauses distributed in several modules, and all modules
   which define a predicate as multifile can use that predicate.  The
   name space of multifile predicates is independent, as if they
   belonged to special module @tt{multifile}.

   Every @tt{user} or module file imports implicitly a number of modules
   called @concept{builtin modules}.  They are imported before all other
   importations of the module, allowing thus redefining any of their
   predicates (with the exception of @pred{true/0}) by defining local
   versions or importing them from other modules.  Importing explicitly
   from a builtin module, however, disables the implicit importation of
   the rest (this feature is used by package @lib{library(pure)} to
   define pure prolog code).").

:- comment(doinclude,module/3).
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

:- comment(doinclude,module/2).
:- true decl module(Name, Exports) : modulename * list(predname)
        # "Same as directive @decl{module/3}, with an implicit package
          @tt{default}.".

:- comment(doinclude,export/1).
:- true decl export(Pred) : predname
        # "Adds @var{Pred} to the set of exported predicates.".
:- true decl export(Exports) : list(predname)
        # "Adds @var{Exports} to the set of exported predicates.".

:- comment(doinclude,use_module/2).
:- true decl use_module(Module, Imports) : sourcename * list(predname)
        # "Specifies that this code imports from the module defined in
          @var{Module} the predicates in @var{Imports}.  The imported
          predicates must be exported by the other module.".

:- comment(doinclude,use_module/1).
:- true decl use_module(Module) : sourcename
        # "Specifies that this code imports from the module defined in
          @var{Module} all the predicates exported by it.  The previous
          version with the explicit import list is preferred to this as
          it minimizes the chances to have to recompile this code if the
          other module changes.".

:- comment(doinclude,import/2).
:- true decl import(Module, Imports) : modulename * list(predname)
        # "Declares that this code imports from the module with name
          @var{Module} the predicates in @var{Imports}.

          @bf{Important note:} this declaration is intended to be used
          when the current module or the imported module is going to be
          dynamically loaded, and so the compiler does not include the
          code of the imported module in the current executable (if only
          because the compiler cannot know the location of the module
          file at the time of compilation).  For the same reason the
          predicates imported are not checked to be exported by
          @var{Module}.  Its use in other cases is strongly discouraged,
          as it disallows many compiler optimizations.".

:- comment(doinclude,reexport/2).
:- true decl reexport(Module, Preds) : sourcename * list(predname)
        # "Specifies that this code reexports from the module defined in
          @var{Module} the predicates in @var{Preds}. This implies that
          this module imports from the module defined in @var{Module}
          the predicates in @var{Preds}, an also that this module
          exports the predicates in @var{Preds} .".

:- comment(doinclude,reexport/1).
:- true decl reexport(Module) : sourcename
        # "Specifies that this code reexports from the module defined in
          @var{Module} all the predicates exported by it. This implies that
          this module imports from the module defined in @var{Module}
          all the predicates exported by it, an also that this module
          exports all such predicates .".

:- comment(doinclude,meta_predicate/1).

:- true decl meta_predicate(MetaSpecs) : sequence(metaspec)
        # "Specifies that the predicates in @var{MetaSpecs} have
          arguments which represent predicates and thus have to be
          module expanded.  The directive is only mandatory for exported
          predicates (in modules).  This directive is defined as a
          prefix operator in the compiler.".

:- comment(doinclude, modulename/1).

:- comment(modulename/1, "A module name is an atom, not containing
        characters `:' or `$'.  Also, @tt{user} and @tt{multifile} are
        reserved, as well as the module names of all builtin modules
        (because in an executable all modules must have distinct
        names).").

:- prop modulename(M) + regtype # "@var{M} is a module name (an atom).".

modulename(M) :- atm(M).

:- comment(doinclude, metaspec/1).

:- comment(metaspec/1, "A meta-predicate specification for a predicate
        is the functor of that predicate applied to atoms which
        represent the kind of module expansion that should be done with
        the arguments.  Possible contents are represented as:

        @begin{description}

        @item{@tt{goal}} This argument will be a term denoting a goal
        (either a simple or complex one) which will be called.  For
        commpatibility reasons it can be named as @tt{:} as well.

        @item{@tt{clause}} This argument will be a term denoting a clause.

        @item{@tt{fact}} This argument should be instantiated to a term
        denoting a fact (head-only clause).

        @item{@tt{spec}} This argument should be instantiated to a predicate
        name, as Functor/Arity.

        @item{@tt{pred(@em{N})}} This argument should be instantiated to
        a predicate construct to be called by means of a
        @tt{call/@em{N}} predicate call (see @pred{call/2}).

        @item{@tt{addmodule}} This is in fact is not a real meta-data
        specification.  It specifies that in an argument added after
        this one will be passed the calling module, to allow
        handling more involved meta-data (e.g., lists of goals) by using
        conversion builtins.

        @item{@tt{?,+,-,_}} These other values denote that this argument is not
        module expanded.

        @end{description}").

:- prop metaspec(M) + regtype # "@var{M} is a meta-predicate specification.".

metaspec(M) :- struct(M).
