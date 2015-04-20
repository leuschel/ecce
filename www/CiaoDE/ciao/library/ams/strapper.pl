
:- module(strapper,[main/0],[]).
:- use_module(library(concurrency)). %,[eng_killothers/0]).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library('ams/socket_serve'),[serve_socket_multi/2]).
:- use_module(library(sockets)). 
:- use_module(library(use_url)).

%:- use_module(engine(internals),['$meta_call'/1]).
%	( '$meta_call'(Goal),
% instead of Mod:Goal (??)

:- redefining(debug/1). %also from io_aux
debug(M):- display(user_error,M), nl(user_error).
%debug(_).

main:-
	debug(entering),
	catch(boot(Answer,Port,Socket,Mod),Error,answer(Error)),
	start_listening(Answer,Port,Socket,Mod),
	debug(finished).

boot(Answer,Port,Socket,Mod):-
	read(Mod),
	read(Mid),
	read(Length),
	debug(read((Mod,Mid,Length))),
	( load_module(Mod,Mid)
	-> debug(loaded),
	   ( bind_socket(Port,Length,Socket)
	   -> Answer = 0
	    ; Answer = -2
	   )
	; Answer = -4
	).

load_module(class(Mod),Mid):- !,   % for classes
	use_module_url(Mod,Mid),
	use_module_url(objects_rt,
	               "http://www.clip.dia.fi.upm.es/objects_rt.pl").
load_module(Mod,Mid):-
	use_module_url(Mod,Mid).

answer(Answer):-
	display_term(Answer),
	flush_output.

start_listening(0,Port,Socket,Mod):- !,
	debug(connected),
	answer(Port),
	debug(responded(Port)),
	serve_socket_multi(Socket,listen(Mod)),
	debug(finished_listening).
start_listening(Answer,_Po,_So,_Mo):-
	debug(failed),
	answer(Answer),
	debug(responded(Answer)).

listen(Stream,Mod):-
	read(Stream,Request),
	debug(request(Request)),
	process(Request,Mod,Stream),
	( Request=end_of_file
	-> close(Stream)
	 ; listen(Stream,Mod)
	).

process(end_of_file,_M,_S):- !.
process(0,_Mod,_Stream):- !,
	fail.
process(1,_Mod,_Stream):- !,
	eng_killothers,
        eng_goal_id(Self),
        eng_kill(Self),
	abort,
	halt.
process(Goal,Mod,Stream):-
	( call_in_module(Mod,Goal),
	  Solution = Goal
	; Solution = 0
	),
	debug(solution(Solution)),
	current_output(OldOut),
	set_output(Stream),  % should not fail!
	display_term(Solution),
	flush_output,
	set_output(OldOut).

call_in_module(class(Mod),Goal):- !,
	catch(Mod:Goal,Error,call_in_objects(Error,Mod,Goal)).
call_in_module(Mod,Goal):-
	catch(Mod:Goal,Error,call_in_multifile(Error,Mod,Goal)).

call_in_objects(error(existence_error(procedure,F/A),F/A),Mod,Goal):-
	intercept(F,A,Goal), !,
	debug(recalling_objects(Mod:Goal)),
	catch(objects_rt:Goal,Error,call_in_multifile(Error,Mod,Goal)).
call_in_objects(Error,_M,_Goal):-
	throw(Error).

call_in_multifile(error(existence_error(procedure,F/A),F/A),Mod,Goal):-
	intercept(F,A,Goal), !,
	debug(recalling_multifile(Mod:Goal)),
	call_multifile(Goal).
call_in_multifile(Error,_M,_Goal):-
	throw(Error).

:- redefining(intercept/3). %also from exceptions
intercept(F,A,Goal):-
	functor(Goal,G,A),
	atom_concat('strapper:',G,F).

:- multifile call_multifile/1.
