:- module(platformbased_publish, [], []).

:- use_module(library('actmods/actmodrt'),[remote_call/2]).
:- use_module(library(filenames)).
:- use_module(library(system)).

:- multifile save_addr_actmod/1.
:- multifile '$platform$addr'/1.
:- data '$platform$addr'/1.
:- multifile '$actmod$name'/1.
:- data '$actmod$name'/1.

save_addr_actmod([Address,Host,PORT]) :-
        atom_codes(PORT, Chs),
        number_codes(Port, Chs), !,
	current_executable(ExePath),
        atom_codes(ExePath, EXEPATH),
        no_path_file_name(EXEPATH, EXEFILE),
        ( file_name_extension(EXEFILE, MOD, _), ! ; MOD = EXEFILE ),
        atom_codes(Mod, MOD),
	connect_platform(Host,Port,Mod,Address).
save_addr_actmod([Address,Mod,Host,PORT]) :-
        atom_codes(PORT, Chs),
        number_codes(Port, Chs), !,
	connect_platform(Host,Port,Mod,Address).
save_addr_actmod(_) :-
        inform_user(['Bad number of arguments: [port] [name] platform_host platform_port']),
 	halt(1).

connect_platform(Host,Port,Mod,Address):-
        get_pid(Pid),
        Platform=a(Host,Port),
        ( remote_call(Platform,my_module_address(Mod,Address,Pid)) -> true
	; throw(unable_to_connect_platform(Host:Port)) ),
	asserta_fact('$actmod$name'(Mod)),
	displayq(asserta_fact('$actmod$name'(Mod))), nl,
	asserta_fact('$platform$addr'(Platform)).
