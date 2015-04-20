:- module(check,[go/1,check/3,total/2,tot/1,tot/2],[iso]).

:- use_module(vrml).
:- use_module(io).
:- use_module(generator).
:- set_prolog_flag(write_strings,on).


go(Name) :-
	atom_concat('/home/clip/Systems/ciao/library.development/vrml/Test/',Name,Part0),
        atom_concat(Part0,'.wrl',Path),
        write('######################################################'),nl,nl,
	write(Path),nl,
	vrml_file_to_terms(Path,Terms),
	writeq(Terms),nl,nl,nl,nl.



check(_Type,[],_Max).
check(Type,[Name|More],Max) :-
	check_more(Type,Name,0,Max),
	check(Type,More,Max).

check_more(_,_,N,N).

%Print mode
check_more(p,Name,Nr,Max) :-
	!,
	create_path(Name,Nr,Part1),
	write('######################################################'),nl,nl,
	write(Part1),nl,
	vrml_file_to_terms(Part1,Terms),
	writeq(Terms),nl,nl,nl,nl,
	Nr_new is Nr + 1,
	check_more(p,Name,Nr_new,Max).
	
%Silent mode
check_more(s,Name,Nr,Max) :-
	!,
	create_path(Name,Nr,Part1),
	write(Part1),nl,
	vrml_file_to_terms(Part1,_Terms),
	Nr_new is Nr + 1,
	check_more(s,Name,Nr_new,Max).
	
create_path(Name,Nr,Path) :-
	name(Nr,Nummer),
	atom_codes(Atom,Nummer),
	atom_concat('/home/clip/Systems/ciao/library.development/vrml/Test/',Name,Part0),
	atom_concat(Part0,Atom,Part1),
        atom_concat(Part1,'.wrl',Path).
	
tot(Name) :-
	atom_concat('/home/clip/Systems/ciao/library.development/vrml/Test/',Name,Part0),
        atom_concat(Part0,'.wrl',Path),
        write('######################################################'),nl,nl,
	write(Path),nl,
	vrml_file_to_terms(Path,Terms),
	!,
	writeq(Terms),nl,nl,nl,
	generator(Terms,_VRML).

tot(Name,VRML) :-
	atom_concat('/home/clip/Systems/ciao/library.development/vrml/Test/',Name,Part0),
        atom_concat(Part0,'.wrl',Path),
        write('######################################################'),nl,nl,
	write(Path),nl,
	vrml_file_to_terms(Path,Terms),
	!,
	writeq(Terms),nl,nl,nl,
	generator(Terms,VRML),
	write_vrml_file('ut.wrl',VRML).


total([],_Max).
total([Name|More],Max) :-
	total_more(Name,0,Max),
	total(More,Max).

total_more(_,N,N).

%Print mode
total_more(Name,Nr,Max) :-
	!,
	create_path(Name,Nr,Part1),
	write(Part1),nl,
	vrml_file_to_terms(Part1,Terms),
	generator(Terms,_),
	Nr_new is Nr + 1,
	total_more(Name,Nr_new,Max).
