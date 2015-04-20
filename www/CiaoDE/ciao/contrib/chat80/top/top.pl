
:- module(top,[ hi/0, control/3 ],[ ]).
% Chat-80 : A small subset of English for database querying.
% New top for WWW inteface (MH)

:- use_module(library(format),[ format/2 ]).
:- use_module(library(lists),[ length/2 ]).
:- use_module(library(prolog_sys),[ statistics/2 ]).
:- use_module(library(ttyout),[ ttyflush/0 ]).
:- use_module(library(write),[ numbervars/3, write/1 ]).

:- use_module(readin,[ read_in/1 ]).
:- use_module(ptree,[ print_tree/1 ]).

:- use_module('../nl/newdic',[ word/1 ]).
:- use_module('../nl/newerg',[ sentence/5 ]).
:- use_module('../nl/qplan',[ qplan/2 ]).
:- use_module('../nl/scopes',[ clausify/2 ]).
:- use_module('../nl/slots',[ i_sentence/2 ]).
:- use_module('../nl/talkr',[ answer/2, dlst/3, write_tree/1 ]).

:- use_module('../undefine',[ pp_quant/2 ]).

:- include('../chatops').

/* Control loop  -- tail recursive M.H. */

hi :-
      write('Question: '),
      ttyflush,
      read_in(P),
      control(P,A,_T), !,
%      format("Info: ~s~n~n",[T]),
      format("~s~n~n",[A]),
      ( P = [bye,'.'] 
      -> true 
       ; hi ).

% Changed from failing by default, fixed output M.H.
control([bye,'.'],"Cheerio.",[]) :- 
	!.
control([trace,'.'],"Tracing from now on!",[]) :-
	tracing `= on,
	!.
control([do,not,trace,'.'],"No longer tracing.",[]) :-
	tracing `= off,
        !.
control(U,A,T) :-
	check_words(U,[]),
	!,
	process(U,A,T).
control(U,L,[]) :-
	check_words(U,WrongWords),
	dlst("I do not understand the following words: ",L,E),
	doing(WrongWords,41,E,[]).

doing([],_,Q,Q) :- !.
doing([X|L],N0,Q,Qs) :-
	out(X,Q,Q1),
	advance(X,N0,N,Q1,Q2),
	doing(L,N,Q2,Qs).

out(nb(X),DXs,E) :- !,
	name(X,Xs),
	dlst(Xs,DXs,E).
out(X,DXs,E) :-
	name(X,Xs),
	dlst(Xs,DXs,E).

advance(X,N0,N,L,E) :-
	uses(X,K),
	M is N0+K,
	( M>72 
	-> L = [10|E],
	   N is 0
	 ; N is M+1,
	   L = [0' |E]
        ).

uses(nb(X),N) :- !,
	chars(X,N).
uses(X,N) :-
	chars(X,N).

chars(X,N) :- atomic(X), !,
	name(X,L),
	length(L,N).
chars(_,2).

%% This still has to be fixed: output into the extra argument (dlist) 
%% T the tree etc. instead of simple writes. M.H. 
process(U,A,_T) :-
	statistics(runtime,_),
	sentence(E,U,[],[],[]),
	statistics(runtime,Et0),
	report(E,'Parse',Et0,tree),
	statistics(runtime,_),
	i_sentence(E,QT),
	clausify(QT,UE),
	simplify(UE,S),
	statistics(runtime,Et1),
	report(S,'Semantics',Et1,expr),
	statistics(runtime,_),
	qplan(S,S1), !,
	statistics(runtime,Et2),
	report(S1,'Planning',Et2,expr),
	statistics(runtime,_),
	answer(S1,A), !, 
	statistics(runtime,Et3),
	report(_,'Reply',Et3,none).
process(_,A,[]) :-
	failure(A).

failure("I do not understand the question. Please rephrase it.").
%% failure :-
%%    write('I do no''t understand!'), nl.

report(Item,Label,Time,Mode) :-
	tracing =: on, !,
	nl, write(Label), write(': '), write(Time), write(' msec.'), nl,
	report_item(Mode,Item).
report(_,_,_,_).

report_item(none,_).
report_item(expr,Item) :-
	write_tree(Item), nl.
report_item(tree,Item) :-
	print_tree(Item), nl.
report_item(quant,Item) :-
	pp_quant(Item,2), nl.

:- push_prolog_flag(multi_arity_warnings,off).

simplify(C,(P:-R)) :- !,
	unequalise(C,(P:-Q)),
	simplify(Q,R,true).

simplify(setof(X,P0,S),R,R0) :- !,
	simplify(P0,P,true),
	revand(R0,setof(X,P,S),R).
simplify((P,Q),R,R0) :-
	simplify(Q,R1,R0),
	simplify(P,R,R1).
simplify(true,R,R) :- !.
simplify(X^P0,R,R0) :- !,
	simplify(P0,P,true),
	revand(R0,X^P,R).
simplify(numberof(X,P0,Y),R,R0) :- !,
	simplify(P0,P,true),
	revand(R0,numberof(X,P,Y),R).
simplify(\+P0,R,R0) :- !,
	simplify(P0,P1,true),
	simplify_not(P1,P),
	revand(R0,P,R).
simplify(P,R,R0) :-
	revand(R0,P,R).

:- pop_prolog_flag(multi_arity_warnings).

simplify_not(\+P,P) :- !.
simplify_not(P,\+P).

revand(true,P,P) :- !.
revand(P,true,P) :- !.
revand(P,Q,(Q,P)).

unequalise(C0,C) :-
	numbervars(C0,1,N),
	functor(V,v,N),
	functor(M,v,N),
	inv_map(C0,V,M,C).

inv_map('$VAR'(I),V,_,X) :- !,
	arg(I,V,X).
inv_map(A=B,V,M,T) :- !,
	drop_eq(A,B,V,M,T).
inv_map(X^P0,V,M,P) :- !,
	inv_map(P0,V,M,P1),
	exquant(X,V,M,P1,P).
inv_map(A,_,_,A) :- atomic(A), !.
inv_map(T,V,M,R) :-
	functor(T,F,K),
	functor(R,F,K),
	inv_map_list(K,T,V,M,R).

inv_map_list(0,_,_,_,_) :- !.
inv_map_list(K0,T,V,M,R) :-
	arg(K0,T,A),
	arg(K0,R,B),
	inv_map(A,V,M,B),
	K is K0-1,
	inv_map_list(K,T,V,M,R).

drop_eq('$VAR'(I),'$VAR'(J),V,M,true) :- !,
	( I=\=J ->
	  irev(I,J,K,L), 
	  arg(K,M,L),
	  arg(K,V,X),
	  arg(L,V,X)
	; true ).
drop_eq('$VAR'(I),T,V,M,true) :- !,
	deref(I,M,J),
	arg(J,V,T),
	arg(J,M,0).
drop_eq(T,'$VAR'(I),V,M,true) :- !,
	deref(I,M,J),
	arg(J,V,T),
	arg(J,M,0).
drop_eq(X,Y,_,_,X=Y).

deref(I,M,J) :-
	arg(I,M,X),
	( var(X) ->
	  I=J
	; deref(X,M,J) ).

exquant('$VAR'(I),V,M,P0,P) :-
	arg(I,M,U),
	( var(U) ->
	  arg(I,V,X),
	  P=(X^P0)
	; P=P0 ).

irev(I,J,I,J) :- I>J, !.
irev(I,J,J,I).

%% :- mode check_words(+,-).

%% M.H.
check_words([],[]).
check_words([Word|Words],WrongWords) :-
	word(Word),
	!,
	check_words(Words,WrongWords).
check_words([WrongWord|Words],[WrongWord|WrongWords]) :-
	check_words(Words,WrongWords).

%% :- mode check_word(+,-).

%% M.H.
%% check_word(Word,Word) :- word(Word).
%% check_word(Word,NewWord) :-
%%    write('? '), write(Word), write(' -> (!. to abort) '), ttyflush,
%%    read(NewWord0),
%%    NewWord0 \== !,
%%    check_word(NewWord0,NewWord).
%check_word(Word,Word) :-
%   write('I do not understand "'), write(Word), write('".'), ttyflush,
%   fail.

%% Uurgh... Changed to use assert. M.H. 

%% :- mode `=(+,+), =+(+,-), =:(+,?).

:- data chat_value/2.
chat_value(tracing,off).

Var `= Val :-
	( chat_value(Var,val(_))
	-> retractall_fact(chat_value(Var,val(_)))
	 ; true ),
	!,
	asserta_fact(chat_value(Var,val(Val))).

Var =: Val :-
	chat_value(Var,val(Val)).

%% Var `= Val :-
%%  ( recorded(Var,val(_),P), erase(P)
%% 	 ;	true), !,
%%  recordz(Var,val(Val),_).
%% 
%% %% This lokks very wrong to me... M.H.
%% Var =+ Val :-
%%  ( recorded(Var,val(Val0),P), erase(P)
%% 	; Val0 is 0), !,
%%    Val is Val0+1,
%%    recordz(Var,val(Val),_).
%% 
%% Var =: Val :-
%%    recorded(Var,val(Val),_).
