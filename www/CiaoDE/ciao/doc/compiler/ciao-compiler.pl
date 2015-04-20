:- use_package(assertions).

:- comment(title,"The CIAO compiler").

:- comment(subtitle,"@bf{The CIAO System Documentation Series}").
:- comment(subtitle,"@em{Draft printed on:} @today{}").

:- include(library('ClipAddress')).

:- include(library('Copyright')).

:- comment(bug,"This is still just a first shot...").

:- comment(summary,"

   @include{Warning.lpdoc}

   @includefact{this_manual/1}

   ").

:- comment(module,"

   @include{Warning.lpdoc}

   @includefact{this_manual/1}
").

this_manual("This is the manual for the CIAO Prolog compiler.  It
  documents the @apl{ciaoc} application and the libraries which provide
  the functionalities for compiling/processing code.").

main.
