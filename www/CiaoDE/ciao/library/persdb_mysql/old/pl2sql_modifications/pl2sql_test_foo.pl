:- use_module(library('persdb_sql/pl2sql')).
:- use_module(library(strings)).
:- use_module(library(format)).

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.

%issue_debug_messages(pl2sql).

:- multifile [relation/3,attribute/4].
:- data [relation/3,attribute/4].

relation(product,3,'PRODUCT').
attribute(1,'PRODUCT','ID',int).
attribute(2,'PRODUCT','QUANTITY',int).
attribute(3,'PRODUCT','NAME',string).

main0 :- 
     pl2sqlterm( f(L,K), 
          ((product(L,N,"lines and lines of a string text, lines and lines of a string text, lines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string text more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more ' lines more lines more lines more lines more lines  "); product(L,N,b)),
	   \+ product(2,3,b), 
	   L + 2 > avg(Y, Z^product(Z,Y,a)),
	   K is N + max(X, product(X,2,b))
           ), T),
     printqueries(T).

main1 :-
     pl2sqlstring( f(L,K), 
          ((product(L,N,"lines and lines of a string text, lines and lines of a string text, lines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string textlines and lines of a string text more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more lines more ' lines more lines more lines more lines more lines  "); product(L,N,b)),
	   \+ product(2,3,b), 
	   L + 2 > avg(Y, Z^product(Z,Y,a)),
	   K is N + max(X, product(X,2,b))
	   ), Str),
     write_string(Str).

main1_doesnt_work :- 
     pl2sqlstring( f(L,K), 
          ((product(L,N,a); product(L,N,b)),
	   \+ product(2,3,b), 
	   L + 2 > avg(Y, Z^product(Z,Y,a)),
	   K is N + max(X, product(X,2,b))
	   ), Str),
     atom_codes(At,Str), %% doesn't work because the string oversizes the maximum atom size
     write(At)
.

main2 :-
     pl2sqlterm( product(L,N,aaa), 
          (product(L,N,aaa)), 
	  T),
     printqueries(T).

main3 :-
     pl2sqlstring( product(L,N,a),
          (product(L,N,a)), 
	  Str),
     atom_codes(At,Str),
     write(At).
