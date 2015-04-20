:- module(amsbased_publish, [], []).

:- use_module(library('ams/amsrt'),[ams_service/1]).
:- use_module(library(filenames)).
:- use_module(library(read)).
:- use_module(library(sockets)).
:- use_module(library(system)).

:- multifile save_addr_actmod/1.

save_addr_actmod(a(Host,MyPort)) :-
        current_executable(ExePath),
        atom_codes(ExePath,EXEPATH),
        no_path_file_name(EXEPATH,EXEFILE),
        ( file_name_extension(EXEFILE,MyName,_)
	-> true
	 ; MyName = EXEFILE
	),
        get_pid(MyPid),
	ams_service(Port),
	connect_to_socket(Host,Port,Stream),
	current_output(OldOut),
	set_output(Stream),
        display_term(deamon(MyName,MyPid,MyPort)),
	flush_output,
	set_output(OldOut),
	current_input(OldIn),
	set_input(Stream),
	read(Stream,Answer),
	set_input(OldIn),
        close(Stream),
	!,
	( Answer = -1
	-> error('Someone''s already running as me!'),
	   fail
	 ; true
	).
save_addr_actmod(_Address) :-
	error('Cannot start up!'),
	fail.
