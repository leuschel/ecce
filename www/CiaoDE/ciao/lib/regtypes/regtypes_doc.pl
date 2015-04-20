
:- use_package([assertions,pure]).
:- comment(nodoc,assertions).
:- comment(nodoc,pure).

:- use_module(library(assertions_props)).

:- comment(filetype,package).

:- comment(title,"Declaring regular types").
 
:- comment(author, "Manuel Hermenegildo").
:- comment(author, "Pedro Lopez").
:- comment(author, "Francisco Bueno").

:- comment(module,"This library package adds some new declaration
   definitions and new operator definitions to user programs. These
   new declarations and operators provide some very simple syntactic
   sugar to support @concept{regular type definitions} in source code.
   Regular types are just properties which have the additional 
   characteristic of being @concept{regular types}
   (@pred{basic_props:regtype/1}). 

   For example, this library package allows writing:
   @begin{verbatim}
   :- regtype tree(X) # ""@var{X} is a tree."".
   @end{verbatim}
   instead of the more combersome:
   @begin{verbatim}
   :- prop tree(X) + regtype # ""@var{X} is a tree."".
   @end{verbatim}

   Regular types can be used as properties to describe
   predicates and play an essential role in program debugging (see the 
   Ciao Prolog preprocessor (@tt{ciaopp}) manual).

   In this chapter we explain some general considerations worth taking
   into account when writing properties in general, not just regular
   types. The exact @concept{syntax of regular types} is also described.

   @include{writing_props}
").


:- include(library(regtypes)).

:- comment(regtype/1,
    "@cindex{regtype assertion} This assertion is similar to a pred
     assertion but it flags that the predicate being documented is
     also a ``@concept{regular type}.'' This allows for example
     checking whether it is in the class of types supported by the
     type checking and inference modules. Currently, types are
     properties whose definitions are @em{regular programs}.

     @include{regular_type_syntax}

     The set of types is thus a well defined subset of the set of
     properties. Note that types can be used to describe
     characteristics of arguments in assertions and they can also be
     executed (called) as any other predicates.
").
:- decl regtype(AssertionBody) : assrt_body.

%% :- decl type/1
%%    # "Same as @pred{regtype/1}. Deprecated. Included only for
%%       backwards compatibility.".

%:- new_declaration(regtype/2).
:- comment(regtype/2,
    "@cindex{regtype assertion} This assertion is similar to a
     @pred{regtype/1} assertion but it is explicitely qualified.
     Non-qualified @pred{regtype/1} assertions are assumed the qualifier
     @tt{check}.
     Note that checking regular type definitions should be done with the
     @tt{ciaopp} preprocessor.
").
:- decl regtype(AssertionStatus,AssertionBody) : assrt_status * assrt_body.

%% :- decl type/2
%%    # "Same as @pred{regtype/2}. Deprecated. Included only for
%%       backwards compatibility.".
