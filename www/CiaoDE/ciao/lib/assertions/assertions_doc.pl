
:- use_package([assertions]).

:- comment(filetype,package).

:- use_module(library(assertions_props)).

:- comment(title,"The Ciao assertion package").

:- comment(author, "Manuel Hermenegildo").
:- comment(author, "Francisco Bueno").
:- comment(author, "German Puebla").

:- comment(copyright,"
Copyright @copyright{} 1989-2002 The CLIP Group / UPM

@include{Copyright.Manuals}
").

:- comment(summary,"This library provides modules which allow
   including @concept{program assertions} in user programs. Such
   assertions can be used to describe predicates, properties, modules,
   applications, etc. These descriptions can be formal specifications
   (such as preconditions and post-conditions) or machine-readable
   textual comments. The information contained in the assertions will
   be used as input by other tools for static or dynamic debugging and
   for automatic documentation generation.").

:- comment(usage,"The recommended procedure in order to make use of
   assertions in user programs is to include the @lib{assertions}
   syntax library, using one of the following declarations, as
   appropriate:

@begin{verbatim}
   :- module(...,...,[assertions]).
   :- include(library(assertions)).
   :- use_package([assertions]).
@end{verbatim}
   ").

:- comment(module,"The @lib{assertions} package adds a number of new 
   declaration definitions and new operator definitions which allow including
   @concept{program assertions} in user programs. Such assertions can
   be used to describe predicates, properties, modules, applications,
   etc. These descriptions can be formal specifications (such as
   preconditions and post-conditions) or machine-readable textual
   comments. 

   This module is part of the @lib{assertions} library. It defines the
   basic code-related assertions, i.e., those intended to be used
   mainly by compilation-related tools, such as the static analyzer or
   the run-time test generator.

   Giving @concept{specifications} for predicates and other program elements
   is the main functionality documented here. 
   The exact syntax of comments @cindex{comments, machine readable}
   is described in the autodocumenter
   (@apl{lpdoc} @cite{knuth-lit,lpdoc-tr}) manual,
   although some support for adding machine-readable comments in assertions
   is also mentioned here.

   There are two kinds of assertions: predicate assertions and program
   point assertions. 
   All predicate assertions are currently placed as directives in the source
   code, i.e., preceded by ``@tt{:-}''.
   Program point assertions are placed as goals in clause bodies.

   @section{More info} 

   The facilities provided by the library are documented in the
   description of its component modules. This documentation is
   intended to provide information only at a ``reference manual''
   level. For a more tutorial introduction to the subject and some
   more examples please see @cite{assert-lang-disciplbook}.
@comment{
% the document ``An Assertion Language for
%    Debugging of Constraint Logic Programs (Technical Report
%    CLIP2/97.1)''. 
}
   The assertion language implemented in this library
   is modeled after this design document, although, due to
   implementation issues, it may differ in some details. The purpose
   of this manual is to document precisely what the implementation of
   the library supports at any given point in time.

   @section{Some attention points} 

   @begin{itemize}

   @item @bf{Formatting commands within text strings:} @cindex{formatting
   commands} many of the predicates defined in these modules include
   arguments intended for providing textual information. This includes
   titles, descriptions, comments, etc. The type of this argument is a
   character string. In order for the automatic generation of
   documentation to work correctly, this @concept{character string}
   should adhere to certain conventions. See the description of the
   @pred{docstring/1} type/grammar for details.

   @item @bf{Referring to variables:} In order for the automatic
   documentation system to work correctly, @concept{variable names}
   (for example, when referring to arguments in the head patterns of
   @em{pred} declarations) must be surrounded by an @tt{@@var}
   command. For example, @tt{@@var@{VariableName@}} should be used for
   referring to the variable ``VariableName'', which will appear then
   formatted as follows: @var{VariableName}. See the description of
   the @pred{docstring/1} type/grammar for details.

   @end{itemize}

").

% ----------------------------------------------------------------------------
% Assertion-related declarations
% ----------------------------------------------------------------------------

:- comment(pred/1,
    "@cindex{pred assertion} This assertion provides information on
     a predicate.  The body of the assertion (its only argument) contains
     properties or comments in the formats defined by
     @pred{assrt_body/1}. 

     More than one of these assertions may appear per predicate, in
     which case each one represents a possible ``@concept{mode}'' of
     use (@concept{usage}) of the predicate. The exact scope of the
     usage is defined by the properties given for calls in the body of
     each assertion (which should thus distinguish the different usages
     intended). All of them together cover all possible modes of usage.

     For example, the following assertions describe (all the and the only)
     modes of usage
     of predicate @tt{length/2} (see @lib{lists}):
     @begin{verbatim}
:- pred length(L,N) : list * var => list * integer
	# ""Computes the length of @var{L}."".
:- pred length(L,N) : var * integer => list * integer
	# ""Outputs @var{L} of length @var{N}."".
:- pred length(L,N) : list * integer => list * integer
	# ""Checks that @var{L} is of length @var{N}."".
     @end{verbatim}
").
:- decl pred(AssertionBody) : assrt_body.

:- comment(pred/2,
    "@cindex{pred assertion} This assertion is similar to a
     @pred{pred/1} assertion but it is explicitely qualified.
     Non-qualified @pred{pred/1} assertions are assumed the qualifier
     @tt{check}.
").
:- decl pred(AssertionStatus,AssertionBody) : assrt_status * assrt_body.

% ----------------------------------------------------------------------------

:- comment(calls/1,
    "@cindex{calls assertion} This assertion is similar to a
     @pred{pred/1} assertion but it only provides information about
     the calls to a predicate.
     If one or several calls assertions are given they are understood to
     describe all possible calls to the predicate.

     For example, the following assertion describes all possible calls
     to predicate @tt{is/2} (see @lib{arithmetic}):
     @begin{verbatim}
:- calls is(term,arithexpression).
     @end{verbatim}
").
:- decl calls(AssertionBody) : c_assrt_body.

:- comment(calls/2,
    "@cindex{calls assertion} This assertion is similar to a
     @pred{calls/1} assertion but it is explicitely qualified.
     Non-qualified @pred{calls/1} assertions are assumed the qualifier
     @tt{check}.
").
:- decl calls(AssertionStatus,AssertionBody) : assrt_status * c_assrt_body.

% ----------------------------------------------------------------------------

:- comment(success/1,
    "@cindex{success assertion} This assertion is similar to a
     @pred{pred/1} assertion but it only provides information about
     the answers to a predicate.
     The described answers might be conditioned to a particular way
     of calling the predicate.

     For example, the following assertion specifies the answers of the
     @tt{length/2} predicate @em{if} it is called as in the first mode
     of usage above (note that the previous pred assertion already 
     conveys such information, however it also compelled the predicate
     calls, while the success assertion does not):
     @begin{verbatim}
:- success length(L,N) : list * var => list * integer.
     @end{verbatim}
").
:- decl success(AssertionBody) : s_assrt_body.

:- push_prolog_flag(multi_arity_warnings,off).

:- comment(success/2,
    "@cindex{success assertion} This assertion is similar to a
     @pred{success/1} assertion but it is explicitely qualified.
     Non-qualified @pred{success/1} assertions are assumed the qualifier
     @tt{check}.
").
:- decl success(AssertionStatus,AssertionBody) : assrt_status * s_assrt_body.

% ----------------------------------------------------------------------------

:- comment(comp/1,
    "@cindex{comp assertion} This assertion is similar to a
     @pred{pred/1} assertion but it only provides information about
     the global execution properties of a predicate
     (note that such kind of information is also conveyed by pred assertions).
     The described properties might be conditioned to a particular way
     of calling the predicate.

     For example, the following assertion specifies that the computation of
     @tt{append/3} (see @lib{lists}) will not fail @em{if} it is 
     called as described (but does not compel the predicate to be called
     that way):
     @begin{verbatim}
:- comp append(Xs,Ys,Zs) : var * var * var + not_fail.
     @end{verbatim}
").
:- decl comp(AssertionBody) : g_assrt_body.

:- comment(comp/2,
    "@cindex{comp assertion} This assertion is similar to a
     @pred{comp/1} assertion but it is explicitely qualified.
     Non-qualified @pred{comp/1} assertions are assumed the qualifier
     @tt{check}.
").
:- decl comp(AssertionStatus,AssertionBody) : assrt_status * g_assrt_body.

% ----------------------------------------------------------------------------

:- comment(prop/1,
    "@cindex{prop assertion} This assertion is similar to a @tt{pred/1}
     assertion but it flags that the predicate being documented is
     also a ``@concept{property}.''

     Properties are standard predicates, but which are @em{guaranteed
     to terminate for any possible instantiation state of their
     argument(s)}, do not perform side-effects which may interfere with 
     the program behaviour, and do not further instantiate their
     arguments or add new constraints.

     Provided the above holds, properties can thus be safely used as
     @concept{run-time checks}. The program transformation used
     in @tt{ciaopp} for run-time checking guarantees the third requirement.
     It also performs some basic checks on properties which in most cases
     are enough for the second requirement. However, it is the user's
     responsibility to guarantee termination of the properties defined.
     (See also @ref{Declaring regular types} for some considerations
     applicable to writing properties.)

     The set of properties is thus a strict subset of the set of
     predicates. Note that properties can be used to describe
     characteristics of arguments in assertions and they can also be
     executed (called) as any other predicates.
").
:- decl prop(AssertionBody) : assrt_body.

:- comment(prop/2,
    "@cindex{prop assertion} This assertion is similar to a
     @pred{prop/1} assertion but it is explicitely qualified.
     Non-qualified @pred{prop/1} assertions are assumed the qualifier
     @tt{check}.
").
:- decl prop(AssertionStatus,AssertionBody) : assrt_status * assrt_body.

% ----------------------------------------------------------------------------

:- comment(entry/1,
    "@cindex{entry assertion} This assertion provides information
     about the @em{external} calls to a predicate. It is identical
     syntactically to a @pred{calls/1} assertion. However, they
     describe only external calls, i.e., calls to the exported predicates
     of a module from outside the module, or calls to the predicates
     in a non-modular file from other files (or the user).

     These assertions are @em{trusted} by the compiler. As a result,
     if their descriptions are erroneous
     they can introduce bugs in programs. Thus, @pred{entry/1}
     assertions should be written with care.

     An important use of these assertions is in @concept{providing
     information to the compiler} which it may not be able to infer
     from the program. The main use is in providing information on the
     ways in which exported predicates of a module will be called from
     outside the module. This will greatly improve the precision of
     the analyzer, which otherwise has to assume that the arguments
     that exported predicates receive are any arbitrary term.
").
:- decl entry(AssertionBody) : c_assrt_body.

% ----------------------------------------------------------------------------

:- comment(modedef/1,
  "This assertion is used to define modes. A mode defines in a compact
   way a set of call and success properties. Once defined, modes can
   be applied to predicate arguments in assertions. The meaning of
   this application is that the call and success properties defined by
   the mode hold for the argument to which the mode is applied. Thus,
   a mode is conceptually a ``property macro''.

   The syntax of mode definitions is similar to that of pred
   declarations. For example, the following set of assertions:
 
@begin{verbatim}
:- modedef +A : nonvar(A) # ""@var{A} is bound upon predicate entry."".

:- pred p(+A,B) : integer(A) =>  ground(B).
@end{verbatim}

  is equivalent to:

@begin{verbatim}
:- pred p(A,B) : (nonvar(A),integer(A)) =>  ground(B)
   # ""@var{A} is bound upon predicate entry."".
@end{verbatim}
").
:- decl modedef(AssertionBody) : assrt_body.

:- comment(decl/1,
    "@cindex{decl assertion} This assertion is similar to a
     @pred{pred/1} assertion but it is used for declarations instead 
     than for predicates.
").
:- decl decl(AssertionBody) : assrt_body.

:- comment(decl/2,
    "@cindex{decl assertion} This assertion is similar to a
     @pred{decl/1} assertion but it is explicitely qualified.
     Non-qualified @pred{decl/1} assertions are assumed the qualifier
     @tt{check}.
").
:- decl decl(AssertionStatus,AssertionBody) : assrt_status * assrt_body.

:- decl comment(Pred,Comment) : head_pattern * docstring
  # "@cindex{comment assertion} This assertion gives a text 
     @var{Comment} for a given predicate @var{Pred}.".

% ----------------------------------------------------------------------------
% Assertion-related predicates
% ----------------------------------------------------------------------------
% Should be in rtchecks????

:- pred check(PropertyConjunction) : property_conjunction
   # "@cindex{check assertion} This assertion provides information on
     a clause program point (position in the body of a clause). Calls
     to a @pred{check/1} assertion can appear in the body of a clause
     in any place where a literal can normally appear. The property
     defined by @var{PropertyConjunction} should hold in all the
     run-time stores corresponding to that program point.
     See also @ref{Run-time checking of assertions}.".

check(_).

:- pred trust(PropertyConjunction) : property_conjunction
   # "@cindex{trust assertion} This assertion also provides information on
     a clause program point. It is identical syntactically to a @pred{check/1}
     assertion. However, the properties stated are not taken as
     something to be checked but are instead @em{trusted} by the
     compiler. While the compiler may in some cases detect an
     inconsistency between a @pred{trust/1} assertion and the program,
     in all other cases the information given in the assertion will be
     taken to be true.  As a result, if these assertions are erroneous
     they can introduce bugs in programs. Thus, @pred{trust/1}
     assertions should be written with care.

     An important use of these assertions is in @concept{providing
     information to the compiler} which it may not be able to infer
     from the program (either because the information is not present or
     because the analyzer being used is not precise enough). In
     particular, providing information on external predicates which
     may not be accessible at the time of compiling the module can
     greatly improve the precision of the analyzer. This can be easily
     done with trust assertion. ".

trust(_).

:- pred true(PropertyConjunction) : property_conjunction
   # "@cindex{true assertion} This assertion is identical
     syntactically to a @pred{check/1} assertion. However, the
     properties stated have been proved to hold by the analyzer. Thus,
     these assertions often represent the @concept{analyzer output}.".

true(_).

:- pred false(PropertyConjunction) : property_conjunction
   # "@cindex{false assertion} This assertion is identical
     syntactically to a @pred{check/1} assertion. However, the
     properties stated have been proved not to hold by the
     analyzer. Thus, these assertions often represent the
     @concept{analyzer output}.".

false(_).
