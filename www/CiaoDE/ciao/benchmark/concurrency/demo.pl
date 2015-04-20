
 %% :- initialization(p).
 %% 

:- use_module(library(write)).
:- use_module(library(listing)).
:- use_module(library(dummy)).


nothing:-
        q.
nothing.

q.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

w :-
        wrt(10).
wrt(0).
wrt(N):-
        write(N), nl,
%%        unix(system('sleep 1')),
        N1 is N - 1,
        wrt(N1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- concurrent p/2.

get_p:-
        current_fact(p(X,Y)),
        write(got(p(X,Y))), nl,
        fail.
get_p:-
        write('get_p finished!'), nl.


get_p_nb:-
        current_fact_nb(p(X,Y)),
        write(got(p(X,Y))), nl,
        fail.
get_p_nb:-
        write('get_p finished!'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_some_p:-
        current_fact(p(Item, More)),
        get_some_p(More, Item).

get_some_p(no, _).
get_some_p(yes, Item):-
        write(got(Item)), nl,
        fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- concurrent p/3.

get_some_p_2:-
        current_fact(p(Id, Item, Next)),
        get_some_p_2(Id, Item, Next).

get_some_p_2(-1, _, _).
get_some_p_2(_Id, Item, Next):-
        write(got(Item)), nl,
        current_fact(p(Next, Item2, More)),
        get_some_p_2(Next, Item2, More).
