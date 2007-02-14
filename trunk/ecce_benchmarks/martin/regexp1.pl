:- module(regexp1, [generate/4]).
:- ensure_loaded(local).

generate(X, Y, Z, Trace):-
    term_size(X, 0, Level),
    generate(X, Y, Z, Level, unbounded, Trace).

generate(X, Y, Z, D, Mode, Trace):-
    (D >= 0 ->
            generate1(X, Y, Z, D, Mode, Trace)
     ;
            Mode = unbounded,
            freeze(Level, generate1(X, Y, Z, Level, bounded, Trace))
    ).

generate1(empty,T,T, _, _, 1).

generate1(char(X),[X|T],T, _, _, 2).

generate1(or(X,_Y),H,T, D, Mode, trace(3, G)):-
    D1 is D - 1,
    generate(X,H,T, D1, Mode, G).
generate1(or(_X,Y),H,T, D, Mode, trace(4, G)):-
    D1 is D - 1,
    generate(Y,H,T, D1, Mode, G).

generate1(cat(X,Y),H,T, D, Mode, trace(5, G1, G2)):-
    D1 is D - 1,
    generate(X,H,T1, D1, Mode, G1),
    generate(Y,T1,T, D1, Mode, G2).

generate1(star(_X),T,T, _, _, 6).
generate1(star(X),H,T, D, Mode, trace(7, G1, G2)):-
    D1 is D - 1,
    generate(X,H,T1, D1, Mode, G1),
    generate(star(X),T1,T, -1, Mode, G2).

term_size(V, D, D):-
   var(V), !.
term_size(A, D, D):-
    atom(A), !.
term_size(char(X), D_in, D_out):-
    D_in1 is D_in + 1,
    term_size(X, D_in1, D_out).
term_size(or(X, Y), D_in, D_out):-
    D_in1 is D_in + 1,
    term_size(X, D_in1, D_mid),
    term_size(Y, D_mid, D_out).
term_size(cat(X, Y), D_in, D_out):-
    D_in1 is D_in + 1,
    term_size(X, D_in1, D_mid),
    term_size(Y, D_mid, D_out).
term_size(star(X), D_in, D_out):-
    D_in1 is D_in + 1,
    term_size(X, D_in1, D_out).