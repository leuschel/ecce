
:- module(xgrun,[ terminal/5, virtual/3 ],[ ]). % pure

/*
:- mode terminal(?,+,?,+,?),
        gap(+),
        virtual(+,+,?).
*/

terminal(T,S,S,x(_,terminal,T,X),X).
terminal(T,[T|S],S,X,X) :-
   gap(X).

gap(x(gap,_,_,_)).
gap([]).

virtual(NT,x(_,nonterminal,NT,X),X).
