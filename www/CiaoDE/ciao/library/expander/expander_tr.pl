:- module(expander_tr,[expand_sentence/3,expand_clause/3],[]).
:- use_module(library('compiler/c_itf'),[defines_module/2]).
:- use_module(library(write)).
:- use_module(library(streams)).

:- data io_output/1.

expand_sentence(0,0,Module):-
	defines_module(Base,Module),
	atom_concat(Base,'_co.pl',F),
	open(F,write,IO),
	asserta_fact(io_output(IO)).

expand_sentence((:- add_clause_trans(expand_clause/3)),(:- add_clause_trans(expand_clause/3)),M):-
	io_output(IO),
	atom_concat(M,'_co',F),
	write(IO,(:- module(F,[],[]))), write(IO,'.\n').

expand_sentence((:- S),(:- S),_):-
	write_sentence((:- S)).

expand_sentence(end_of_file,end_of_file,_):-
	io_output(IO),
	write(IO,'\n').

expand_sentence(S,S,_).

write_sentence(S):-
	io_output(IO),	
	writeq(IO,S), write(IO,'.\n').

expand_clause(clause(0,0),clause(0,0),Module) :-
	defines_module(Base,Module),
	display(Base),nl,
	atom_concat(Base,'_co.pl',F),
	display(F),nl,
	open(F,append,IO),
	asserta_fact(io_output(IO)).

expand_clause(clause(Head,true),clause(Head,true),_):-
	io_output(IO),
	write_term(IO,(Head),[quoted(true)]), write_term(IO,'.\n',[]).
	
expand_clause(clause(Head,Body),clause(Head,Body),_):-
	io_output(IO),
	write_term(IO,(Head),[quoted(true)]), write(IO,' :- \n'), write_body(Body), write(IO,'.\n'). 

expand_clause(A,A,_):-
	io_output(IO),
	close(IO),
	display([A,'user_output']).


write_body(';'(A,B)):-
	write_body(A),
	io_output(IO),	
	tab(IO,8), write(IO,'; \n'),
	write_body(B), tab(IO,8), write(IO,')').

write_body('->'(A,B)):-
	io_output(IO),
	tab(IO,8), write(IO,'( \n'), 
	write_body(A), tab(IO,8), write(IO,' -> \n'),
	write_body(B).
	
write_body(','(A,B)) :- 
	io_output(IO),
	tab(IO,10), write_term(IO,A,[quoted(true)]), write(IO,', \n'),
	write_body(B).

write_body(B) :-
	io_output(IO),
	tab(IO,10), write_term(IO,B,[quoted(true)]), write(IO,'\n').
