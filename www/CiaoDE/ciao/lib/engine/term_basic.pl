:- module(term_basic, [
        (=)/2, (\=)/2, arg/3, functor/3, (=..)/2, copy_term/2, 'C'/3],
        [assertions, isomodes]).

:- comment(title,"Basic term manipulation").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"This module provides basic term manipulation.").

:- true pred copy_term(Term, Copy) + ( sideff(free), native, iso )

        # "@var{Copy} is a renaming of @var{Term}, such that brand new
           variables have been substituted for all variables in
           @var{Term}.  If any of the variables of @var{Term} have
           @concept{attributes}, the copied variables will have copies
           of the attributes as well. It behaves as if defined by:

@begin{verbatim}
:- data 'copy of'/1.

copy_term(X, Y) :-
        asserta_fact('copy of'(X)),
        retract_fact('copy of'(Y)).
@end{verbatim}".

:- true comp copy_term(Term, Copy) : ground(Term) + eval.

:- impl_defined(copy_term/2).

% Compiled inline -- these provide hooks for the interpreter and comments.

:- true pred '='(X,Y) + (sideff(free), native, iso, eval)
	# "@var{X} and @var{Y} unify.". 

:- true prop '='/2.

X=Y :- X=Y.

:- true pred '\='(X,Y) + (sideff(free), bind_ins, iso)
	# "@var{X} and @var{Y} are not unifiable.". 

X \= X :- !, fail.
_ \= _.

:- true pred arg(ArgNo,Term,Arg) + ( sideff(free), native, iso, bind_ins ) 
	# "Argument @var{ArgNo} of the term @var{Term} is @var{Arg}.".

:- true pred arg(+ArgNo,+Term,?Arg)
	: (nnegint(ArgNo), struct(Term)) + eval.

arg(X, Y, Z) :- arg(X, Y, Z).

:- true pred functor(Term,Name,Arity) => ( atm(Name), num(Arity) )
	+ ( sideff(free), native, iso, bind_ins )
        # "The principal functor of the  term @var{Term} has name @var{Name}
           and arity @var{Arity}.".
:- true comp functor(Term,Name,Arity) : nonvar(Term) + eval.
:- true comp functor(Term,Name,Arity) : (atom(Name), nnegint(Arity)) + eval.

functor(X, Y, Z) :- functor(X, Y, Z).

:- true pred (?Term =.. ?List) => list(List) + ( sideff(free), native, iso )
	# "The functor and arguments of the term @var{Term} comprise the 
           list @var{List}.".
:- true comp (+ =.. ?)  + eval.
:- true comp (_ =.. List) : (list(List),atom_head(List)) + eval.

X=..Y :- X=..Y.

:- prop atom_head/1.

atom_head([Head|_]):-
	atom(Head).

:- true pred 'C'(?S1,?Terminal,?S2) + ( sideff(free), native )
	# "@var{S1} is connected by the terminal @var{Terminal} to @var{S2}.
           Internally used in @em{DCG grammar rules}. Defined as if by the 
           single clause: @tt{'C'([X|S], X, S).}".

:- true comp 'C'(+,?,?) + eval.

'C'(X, Y, Z) :- 'C'(X, Y, Z).
