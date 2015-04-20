police :-
    start_P(P),
    start_T(T),
    init_output(P,T),
    game(P,T).
police :- end_output.

start_P(p(25, 25, d(1,0))).
start_T(p(30, 20, d(1,0))).


game(P, T) :-
    t_moves(T, NT),
    p_moves(P, NT).


t_moves(T, NT) :-
    repeat,    
    get_move(M),
    apply_move(M, T ,NT), !,
    drawpath(T, NT, thief).


apply_move(right, p(X, Y, D), p(NX, NY, ND)) :-
    right(D, ND),
    ND = d(Ix, Iy),
    NX is X+Ix,
    NY is Y+Iy.

apply_move(left, p(X, Y, D), p(NX, NY, ND)) :-
    right(ND, D),
    ND = d(Ix, Iy),
    NX is X+Ix,
    NY is Y+Iy.
    
apply_move(forward,  p(X, Y, D), p(NX, NY, D)) :-
    D = d(Ix, Iy),
    NX is X+Ix,
    NY is Y+Iy.

apply_move(quit, _, none).

p_moves(P, T) :- 
    igotyou(P, T), !,
    drawpath(P, T, police),
    end_output.

p_moves(P, T) :-
    trymove(P, NP),
    correct(T, NP), !,
    drawpath(P, NP, police),
    game(NP, T).

p_moves(P, T) :-
    trymove(P, NP), !,
    drawpath(P, NP, police),
    game(NP, T).


igotyou(P, T) :-
    trymove(P, NP),
    eqplace(T, NP).


eqplace(p(X,Y,_), p(X,Y,_)).


trymove(P, NP) :-
    move1(P, P1),
    move1or0(P1, NP).


move1(P, NP) :-
    apply_move(right, P, NP).

move1(P, NP) :-
    apply_move(forward, P, NP).


move1or0(P, NP) :- move1(P, NP).

move1or0(P, P).


correct(p(Tx, Ty, Td), p(Px, Py, Pd)) :-
    right(Pd, Td), !,
    Td=d(ITx, ITy),
    Pd=d(IPx, IPy),
    D_0 is ITx * (Tx - Px) + ITy * (Ty - Py),
    D_1 is IPx * (Tx - Px) + IPy * (Ty - Py),
    D_0 > 0,
    D_1 =< 2*D_0 + 1.

correct(p(Tx, Ty, Td), p(Px, Py, Pd)) :-
    right(Pd, Dd),
    Dd=d(IDx, IDy),
    Pd=d(IPx, IPy),
    D_0 is IDx * (Tx - Px) + IDy * (Ty - Py),
    D_1 is IPx * (Tx - Px) + IPy * (Ty - Py),
    D_0 > 0,
    D_1 =< 2*D_0.


right(d( 1, 0), d( 0,-1)).
right(d( 0,-1), d(-1, 0)).
right(d(-1, 0), d( 0, 1)).
right(d( 0, 1), d( 1, 0)).

/* I/O predicates */

:- use_module(library(gmlib)).

init_output(P,T) :-
    start,
    city <= view(500,500),
    wind <= window("Police & Thief",
                      vbox([scroller(city),
                            space,
                            hbox([space,
                                  button("Left", left, style(fancy)),
                                  space,
                                  button("Forward", forward, style(fancy)),
                                  space,
                                  button("Right", right, style(fancy)),
                                  space,
                                  button("Quit", quit, style(fancy)),
                                  space]),
                            space])),
    wind => open,
    city => setbrush(0,2),
    drawpoint(P, police),
    drawpoint(T, thief).

get_move(M) :-
    waitevent(button(_, M)).

drawpoint(p(X, Y, _), Player) :-
    color(Player, C),
    city => setcolors(C, white),
    radius(Player, R),
    TX is 5*X,
    TY is 5*Y,
    city => fillcircle(TX,TY,R).

drawpath(p(X, Y, _), p(NX, NY, _), Player) :-
    color(Player, C),
    city => setcolors(C, white),
    radius(Player, R),
    TX is 5*X,
    TY is 5*Y,
    TNX is 5*NX,
    TNY is 5*NY,
    city => line(TX, TY, TNX, TNY),
    city => fillcircle(TNX, TNY, R). 

color(thief, red).
color(police, blue).

radius(thief, 2).
radius(police, 3).

end_output :-
    end.
