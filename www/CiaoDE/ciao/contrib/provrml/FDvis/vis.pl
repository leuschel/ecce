% :- [ciaocompat].
%%:- use_module(dirs).
:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- use_module(library(system)).
:- use_module(library(basicprops)).
:- use_module(internal_types).
:- use_module(library(sockets)).
:- use_module(library(iso_byte_char)).
:- use_module(intermediate).
:- use_module(stone).
:- use_module(io).

server_name('elcano.dia.fi.upm.es').

server_start(Stream) :-
	write('\nPORT NUMBER:\n'),
	read(Port),
	write('\n******** START SERVER ********\n'),
	server_name(Server),
	connect_to_socket(Server,Port,Stream).
	
server_start(Stream, Host) :-
        write('\nPORT NUMBER:\n'),
        read(Port),
	write('\n******** START SERVER ********\n'),
	connect_to_socket(Host, Port, Stream).

server_start(Stream, Host, Port) :-
	write('\n******** START SERVER ********\n'),
        connect_to_socket(Host, Port, Stream),
	write('\nGot stream\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_visual(VRML) :-
	code_create(Terms),
%%      coord_system_create(labels(a,b,c),0,9, CoordTerms),
%%	append(C,CoordTerms,C1),
%%	append(C,CubeTerms, Terms),
	terms_to_vrml(Terms, VRML).
%%%%%	write_file('filename',VRML).

visualise([]).
visualise(ListOfFiles) :-
	write('starting stream\n'),
	server_name(Server),
	server_start(Stream, Server),
	write('Connection established\n'),
	visualise(ListOfFiles, Stream).

visualise([], Stream) :-
	write('Thats all folks\n'),
	server_call(Stream, close).

visualise([FileName|MoreFiles], Stream) :-
	server_call(Stream, file(FileName)),
write('To change picture write any character (non empty) and "."\n'),
	read(_),
	visualise(MoreFiles, Stream).

fil(VRML):-
	create_visual(VRML).

koll :-
	popen('../../VRML/CODE/vis',read, Stream),
	write('Lets start\n'),
	take(Stream, Port),
	write(Port),
	write('\n'),
	vis(Port).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
take(S, P) :-
	take(S, [], P).

take(_, [-1], null).

take(S, [0'@], Port):-
	get_code(S, C),
	take_rest(S, [64,C], Port).

take(S, _, Port) :-
	get_code(S, C),
	take(S, [C], Port).

take_rest(S, [64,80,79,82,84,64], Port) :-
	get_code(S, C1),
	get_code(S, C2),
	get_code(S, C3),
	get_code(S, C4),
	name(Port, [C1,C2,C3,C4]).


take_rest(S, Val, Port):-
	get_code(S, C),
	append(Val, [C], Rest),
	take_rest(S, Rest, Port).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vis(Port):-
	write('starting stream\n'),
	server_name(Server),
	server_start(Stream, Server, Port),
	write('Connection established\n'),
	vrml_file_to_terms('cone4.wrl', Terms1),
	write('1\n'),
	terms_to_vrml(Terms1, VRML1),
	server_call(Stream, code(VRML1)).

mer:-	read(_),
	vrml_file_to_terms('cone2.wrl', Terms2),
	write('1\n'),
	terms_to_vrml(Terms2, VRML2),
	server_call(Stream, code(VRML2)),
	read(_),	
	terms_to_vrml(Terms3, VRML3),
	vrml_file_to_terms('cone3.wrl', Terms3),
	server_call(Stream, code(VRML3)),
	read(_),
	vrml_file_to_terms('cone4.wrl', Terms4),
	terms_to_vrml(Terms4, VRML4),
	server_call(Stream, code(VRML4)),
	read(_).


server_call(Stream, close) :-
	socket_send(Stream,"close"),
	close(Stream).

server_call(Stream, code(Msg)) :-
	append("code",Msg,Send1),
	get_end_of_msg(End),
	append(Send1, End, Send),
	socket_send(Stream, Send).

server_call(Stream, file(Msg)) :-
	write( 'Nu \n' ),
	append("file",Msg,Send1),
	write( Send1), write('\n'),
	write( '    ska\n' ),
	append(Send1, "\n", Send),
	write( '        vi skicka \n' ),
	socket_send(Stream, Send).

get_end_of_msg("\n@#END@\n").


server_poll(Stream, Msg) :-
	stream_select([Stream],100:0,R),
	(  R \== []
        -> read_to_close(Stream, Msg)
	;
	   server_poll(Stream, Msg)
	).
	
server_poll_action(Stream) :-
	server_poll(Stream, Msg),
	message_action(Msg,Stat),
	(  Stat == true
	-> server_poll_action(Stream)
	;
	    true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
message_action(Msg,_) :-
	write('MESSAGE_ACTION'),
	prefix_extract(Msg,Code),
	add_header(Code, NewCode),
	vrml_to_terms( NewCode, Terms),
	write_file_term('gr.terms', Terms).
   
add_header(Code, NewCode) :-
	append("#VRML V2.0 utf8",Code, NewCode).



%%%%One could do it quite neet using Term =.. List directly and do the call directly afterwaards. Here we are more like this, doing checks.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action([restrict|Params], true) :-
	restrict(Params).

action([new_view|Params], true) :-
	new_view(Params).

action([calculate|Params], true) :-
	calculate(Params).

action([exit|_], false).
	

param_extract(List, Params) :-
	param_extract(List, Params, []).

param_extract([], [X], Tmp) :-
	name(X,Tmp).

param_extract([0'$|List], [X|Params], Tmp) :-
	name(X,Tmp),
	param_extract(List, Params, []).

param_extract([C|List], Params, Tmp) :-
	app(Tmp, C, Tmp1),
	param_extract(List, Params, Tmp1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%prefix_extract(+List, -Rest) 
%This predicate can be used typically to take away a prefix.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prefix_extract([], []).

prefix_extract([0'$|List], List).
		  
prefix_extract([_|List], Params) :-
	prefix_extract(List, Params).



      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  read_to_close(+Stream, -String)
%  reading a Stream to the end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
read_to_close(S, L) :-
	get0(S, C),
	read_to_close1(C, S, L).

read_to_close1(-1, _, []) :- 
	!.

read_to_close1(C, S, [C|L]) :-
        get0(S, C1),
	read_to_close1(C1, S, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wr([]):-!.
wr([C|S]):-
	put(C),
	wr(S).

write_string([],_).
write_string([C|Cs],S) :- put(S,C), write_string(Cs,S).




append([], X, X).
append([H|L1], X, [H|L2]) :-
	append(L1, X, L2).

app([], X, [X]).
app([H|L1], X, [H|L2]) :-
	app(L1, X, L2).

%% Concatenates two atoms
atom_concat_own(A,B,AB) :-
        atom_chars(A,AS),
	atom_chars(B,BS),
        append(AS,BS,ABS),
        atom_chars(AB,ABS).

%%%%%%%%%%%%%%%%
