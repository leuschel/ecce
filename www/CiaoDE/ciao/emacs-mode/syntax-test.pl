#!/bin/sh
exec ciao-shell $0 "$@" # -*- mode: ciao; -*-
% (Script headers)

 /* This is a simple file 
   for testing syntax coloring 
   (and this is a test of one type of comment)

   Remember that there is a Ciao command/button to recompute highlighting */

% Module-related directives:
:- module('syntax-test',[
        write_term/3, write_term/2,
        'write_option'/1
        ], 
        [dcg,assertions,isomodes]).

:- use_module(engine(internals), 
	['$atom_mode'/2]).

% builtin directives

:- set_prolog_flag(write_strings,on).

% predicate directives

:- multifile define_flag/3.

% library directives

:- function(foo/2).

% user-defined directives

:- mydirective(foo/2).

%% Another kind of comment
foo.                % More comments, not first line

% LPdoc comments
% bugs:

:- comment(bug,"A bug").  

% version comments:

:- comment(version_maintenance,off).

:- comment(nodoc,foo/2).

:- comment(hide,bar/2).

:- comment(version(1*5+144,2000/05/17,21:08*21+'CEST'), "Changed write
   predicates so that term -a is written as is (-(2) is still written
   this way, though). (Daniel Cabeza Gras)").

% Characters, strings, heads, necks, unrecognized decls, cut, 
% concurrency operators, ... 

:- foo(unreconized,"A string").

writeq_quick(Term) :- var(_Term) @ foo && 
	displayq(Term,"hello"),
	!,
	'is not cut!'.
writeq_quick(_term) :- 
	atomic("This is

                a longer string"), 
	displayq(0'n),
	display(0'\\).

write_option(priority('Prio')) --> integer(Prio), Prio >= 1, Prio =< 1200.

% other comments and
% lpdoc commands in comments

:- comment(title, "Term output" ).

:- comment(author,"Adapted from shared code written by Richard
   A. O'Keefe. :- Changes by Mats Carlsson, Daniel Cabeza, Manuel
   Hermenegildo, and Manuel Carro.").

:- comment(define_flag/3,"Defines flags :- as follows:
	@includedef{define_flag/3} (See @ref{Changing system behaviour
	and various flags}).

   If flag is @tt{on}, lists which may be written as
   @pred{display/1}-@pred{display/2} strings are.

   ISO-Prolog are included, plus other traditionally provided by
   Prolog
@begin{verbatim}
  This is verbatim
@end{verbatim}
   Implementations. Output predicates are not provided in two
    @include{foobar}
   versions: one that uses the current output stream and other in
         ").

:- comment(write_option/1, "@var{Opt} is a valid write option which
   affects the predicate @tt{write_term/3} and similar ones. Possible
   write_options are: @begin{itemize} @item
   @bf{quoted(}@em{bool}@bf{):} If @em{bool} is @tt{true}, atoms and
   functors that can't be read back by @pred{read_term/3} are quoted,
   if it is @tt{false}, each atom and functor is written as its
   name. Default value is @tt{false}.

   @item @bf{ignore_ops(}@em{flag}@bf{):} If @em{flag} is @tt{true},
   each compound term is output in functional notation, if it is
   @tt{ops}, curly bracketed notation and list notation is enabled
   when outputing compound terms, if it is @tt{false}, also operator
   notation is enabled when outputing compound terms. Default value is
   @tt{false}.").

% Assertions, 
% comment string in assertions

:- checked pred write_term(@Stream, ?Term, +OptList) 
   => stream * term * list(write_option) +  iso

   # "Outputs the term @var{Term} to the stream @var{Stream}, with the
      list of write-options @var{OptList}. See @pred{write_option/1}
      type for default options.".

:- check 

   pred fooo 

   # "lkjhlk".

:- pred fooo # "lkjhlk" .

:- true pred fooo # "lkjhlk" .

:- false pred fooo # "lkjhlk" .

:- trust pred fooo # "lkjhlk" .

:- entry pred fooo # "lkjhlk" .

:- checked pred fooo # "lkjhlk" .

main :- 
	true,

	check(([term(T),term(X)] ; lkjh)),
	true,
	trust(( jhlkjhlkjhlkjhlkjhlk
              ; jhlkjhlkjhlkjhlkjhlk
              ; jhlkjhlkjhlkjhlkjhlk )),
	true,
	check([term(T),term(X)] , lkjh),
        checked(([mytype(X), ']))'pr_pp ],
                ('end of predicate p'))),
	check([term(T),term(X)] , lkjh),
        true([term(T),term(X)] 
               , lkjh),
	true,
	check(mytype(X),pr_pp('end of predicate p')),
        false(([term(T),term(X)] ; lkjh )),
	true,
        true(([term(T),term(X)] ; 
              mshare([[T],[T,X],[X]]))),
	display(hello),
	nl. 

:- pred write_term(?Term, +OptList) => term * list(write_option) + iso

   # "Behaves like @tt{current_output(S),
      write_term(S,Term,OptList)}.".

:- pred write_canonical(@Stream, ?Term) => stream * term + iso
       # "Behaves like @tt{write_term(Stream, Term, [quoted(true),
          ignore_ops(true)])}.  The output of this predicate can

          always be parsed by @pred{read_term/2} even if the term
          contains special characters or if operator declarations have
          changed.".

:- prop write_option(Opt) 
        # "@var{Opt} is a valid @ref{Changing system} write option.".

:- regtype write_option/2.

:- prop mshare(X) + native(sharing(X))
# "The sharing pattern is @tt{@var{X}}.".

:- impl_defined(mshare/1).

:- comment(fails(X), "Calls of the form @var{X} fail.").

:- prop fails(X) 
	# "Calls of the form @var{X} fail.".

:- impl_defined(fails/1).

:- comment(possibly_fails(X), "Non-failure is not ensured for any call
of the form @var{X} @cite{non-failure-iclp97}. In other words, nothing
can be ensured about non-failure nor termination of such calls.").

:- prop possibly_fails(X) # 
	"Non-failure is not ensured for calls of the form @var{X}.".

:- comment(version(1*5+144,2000/05/17,21:08*21+'CEST'), "Changed write
   predicates so that term -a is written as is (-(2) is still written
   this way, though). (Daniel Cabeza Gras)").

:- comment(version(1*5+144,2000/05/17,21:08*21+'CEST'), "Changed write
   predicates so that term -a is written as is (-(2) is still written
   this way, though). (Daniel Cabeza Gras)").

:- comment(version(1*5+144,2000/05/17,21:08*21+'CEST'), "Changed write
   predicates so that term -a is written as is (-(2) is still written
   this way, though). (Daniel Cabeza Gras)").

:- comment(version(1*5+144,2000/05/17,21:08*21+'CEST'), "Changed write
   predicates so that term -a is written as is (-(2) is still written
   this way, though). (Daniel Cabeza Gras)").

:- comment(version(1*5+144,2000/05/17,21:08*21+'CEST'), "Changed write
   predicates so that term -a is written as is (-(2) is still written
   this way, though). (Daniel Cabeza Gras)").

:- comment(version(1*5+144,2000/05/17,21:08*21+'CEST'), "Changed write
   predicates so that term -a is written as is (-(2) is still written
   this way, though). (Daniel Cabeza Gras)").

trans_info(eapp026, 'Webmergers 120').
trans_info(eapp030, 'QSP''s ASP business').
trans_info(tech055, 'AiSoftw@re SpA').
trans_info(eser002, 'ISP Europeo, la participacion del 49,5 % poseida').
