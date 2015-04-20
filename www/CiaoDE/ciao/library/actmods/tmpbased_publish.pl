:- module(tmpbased_publish, [], []).

:- use_module(library('actmods/tmpbased_common')).
:- use_module(library(filenames)).
:- use_module(library(streams)).
:- use_module(library(lists), [append/3]).
:- use_module(library(system)).

:- multifile save_addr_actmod/1.

save_addr_actmod([Address]) :- !,
        current_executable(ExePath),
        atom_codes(ExePath, EXEPATH),
        no_path_file_name(EXEPATH, EXEFILE),
        ( file_name_extension(EXEFILE, MOD, _), ! ; MOD = EXEFILE ),
        atom_codes(Mod, MOD),
        module_to_addressfile(Mod, AddrPath),
        ( file_exists(AddrPath) ->
	  catch(delete_file(AddrPath), error(A, B),
	  handler(A, B, AddrPath))
	; true ),
        get_pid(Pid),
        umask(OldUmask,0o077),
        open(AddrPath, write, ST),
        current_output(OldOut),
        set_output(ST),
        display_term(Address),
        display_term(pid(Pid)),
        set_output(OldOut),
        close(ST),
        umask(_, OldUmask).
save_addr_actmod(_) :-
        inform_user(['Bad number of arguments: either none or port number']),
 	halt(1).


handler(system_error, B, File) :-
	message( error , [ 'System Error in ', B, 
                          ' when working with file ' , File ] ).
handler(A, B, File) :-
	message( error , [ 'No message for ' , A , B, File ] ).
