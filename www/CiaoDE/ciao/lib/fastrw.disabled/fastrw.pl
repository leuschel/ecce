:- module(fastrw,
        [fast_read/1,
         fast_write/1,
         fast_write_to_string/3],
         [dcg,assertions]).

:- impl_defined(
        [fast_read/1,
         fast_write/1]).

:- comment(title, "Fast Reading and Writing of Terms").

:- comment(author, "Daniel Cabeza").
:- comment(author, "Oscar Portela Arjona").

:- comment(module, "This library provides predicates to support reading /
	writing of terms on a format designed to be handled on read faster
	than standard representation.").

:- comment(fast_read(Term), "The next term is read from current standart
	input and is unified with @var{Term}. The syntax of the term must
	agree with fast_read / fast_write format. If the end of the input
	has been reached, @var{Term} is unified with the term
	'end_of_file'. Further calls to @pred{fast_read/1} will then cause
	an error.").

:- comment(fast_write(Term),"Output @var{Term} in a way that
	@pred{fast_read/1} will be able to read it back.").

:- use_module(library(dict)).

version(0'C).

index_of(Dict, T, N) :- index_of_(Dict, T, 0, N).

index_of_(V, T, I, N) :-
        var(V), !,
        V = [T|_], N = I.
index_of_([T0|_], T, I, N) :-
        T0 == T, !,
        N = I.
index_of_([_|D], T, I, N) :-
        I1 is I+1,
        index_of_(D, T, I1, N).

fast_write_to_string(T, S, R) :-
        version(V),
        S = [V|S_],
        fastrw_term(T,_Dict, S_, R).

fastrw_term(V,Vdict) --> {var(V)}, !,
        "_",
        {index_of(Vdict, V, N), number_codes(N,S)},
        string(S), [0].
fastrw_term(I,_) --> {integer(I)}, !,
        "I",
        {number_codes(I,S)},
        string(S), [0].
fastrw_term(F,_) --> {float(F)}, !,
        "F",
        {number_codes(F,S)},
        string(S), [0].
fastrw_term([],_) --> !,
        "]".
fastrw_term(A,_) --> {atom(A)}, !,
        "A",
        {atom_codes(A,S)},
        string(S), [0].
fastrw_term([X|Xs],Vdict) --> {integer(X), X > 0, X =< 255}, !,
        """",
        [X],
        fastrw_string(Xs, Vdict).
fastrw_term([X|Xs],Vdict) --> !,
        "[",
        fastrw_term(X,Vdict),
        fastrw_term(Xs,Vdict).
fastrw_term(S,Vdict) --> {functor(S,F,A)},
        "S",
        {atom_codes(F,C)},
        string(C), [0,A],
        fastrw_args(S, 1, A, Vdict).

fastrw_args(S,I,N,Vdict) --> {I =< N}, !,
        {arg(I,S,T)},
        fastrw_term(T,Vdict),
        {I1 is I+1},
        fastrw_args(S,I1,N,Vdict).
fastrw_args(_,_,_,_) --> "".

fastrw_string([X|Xs],Vdict) --> {integer(X), X > 0, X =< 255}, !,
        [X],
        fastrw_string(Xs,Vdict).
fastrw_string(T,Vdict) -->
        [0],
        fastrw_term(T,Vdict).

string([]) --> "".
string([C|Cs]) -->
        [C],
        string(Cs).
