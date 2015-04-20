:- module(_, [], [assertions]).

:- comment(title, "Directives for using code in other files").

:- comment(author, "Daniel Cabeza").

:- comment(module, 
        "Documentation for the directives used to load code into Ciao Prolog (both from the toplevel shell and by other modules).").

:- comment(usage, "These directives are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(doinclude,ensure_loaded/1).
:- decl ensure_loaded(File) : sourcename + iso
        # "Specifies that the code present in @var{File} will be
          included in the executable being prepared, in the @tt{user}
          module.  The file @var{File} cannot have a module
          declaration.  This directive is intended to be used by
          programs not divided in modules.  Dividing programs into
          modules is however strongly encouraged, since most of the
          attractive features of Ciao (such as static debugging and
          global optimization) are only partially available for
          @tt{user} modules.".

:- comment(doinclude,include/1).
:- decl include(File) : sourcename + iso
        # "The contents of the file @var{File} are included in the
          current program text exactly as if they had been written in
          place of this directive.".

:- comment(doinclude,use_package/1).
:- comment(use_package(Package),"Specifies the use in this file of the
          packages defined in @var{Package}.  See the description of the
          third argument of @decl{module/3} for an explanation of
          @concept{package file}s.

          This directive must appear the first in the file, or just
          after a @decl{module/3} declaration.  A file with no module
          declaration, in the absence of this directive, uses an
          implicit package @tt{default} (see @ref{Other predicates and
          features defined by default}).").

:- decl use_package(Package) : sourcename.
:- decl use_package(Package) : list(sourcename).
