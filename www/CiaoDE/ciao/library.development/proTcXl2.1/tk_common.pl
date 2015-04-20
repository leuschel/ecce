%
% ProTcl 1.1
%
%	Definitions common to all Prologs
%
% Author: Micha Meier
%
%
% sccsid("@(#)tk_common.pl	1.14          96/02/12").
% sccscr("@(#)  Copyright 1993 ECRC GmbH ").
%

tk(Opts) :-
    tk_init('', Opts),
    update.

tk_demo :-
    tk_init('$tk_library/demos/widget', []).

tk_init(File, Opts) :-
    tk_clear_options,
    tk_options(Opts, OptFile),
    (File = '', nonvar(OptFile) ->
	InFile = OptFile
    ;
	InFile = File
    ),
    tk_init(InFile),
    protcl_source(ProTcl),
    concat_atoms('global protcl_library; set protcl_library ', ProTcl, SP),
    tcl_eval(SP),
    tcl_eval('set argc [llength $argv]'),
%%    add_new_commands, %% This was just defining call_prolog
    (InFile = '' ->
     %% set_error_handler(153, tk_prompt/2)   %% MCL
    true
    ;
	concat_atoms('source ', InFile, InitFile),
	tcl_eval(InitFile),
	tk_main_loop
    ).

tk_options([], _).
tk_options([Opt|List], File) :-
    Opt =.. [Name|Args],
    (Args = [Val|_] ->
	true
    ;
	Val = ''
    ),
    (Name = file, var(File) ->
	File = Val
    ;
	true
    ),
    tk_option(Name, Val),
    tk_options(List, File).

tk_file(File, Options):- tk([file(File)|Options]).

update:- tcl_eval('if {[info commands update] == "update"} update').

tk_main_loop :-
    tk_do_one_event(0),
    (
        tk_num_main_windows(X), X > 0 ->
	tk_main_loop
    ;
	true
    ).


%
% Wait for the next Prolog event to occur, serve all other events
%

tk_next_event(List):- tk_next_event(16'1e, List).

tk_next_event(Mask, List) :-
    M is Mask /\ 16'fe,
    (tk_do_one_event(M, L) -> true; L = []),
    (
        tk_num_main_windows(0) ->
	List = ["exit"]
    ;
        (
            L = [] ->					% no Prolog event
            tk_next_event(Mask, List)
        ;
            List = L
        )
    ).


%
% Process all events currently present in the queue, return the first Prolog
% one if available, else fail
%

tk_get_event(List):- tk_get_event(16'1f, List).

tk_get_event(Mask, List) :-
    M is Mask \/ 1,				% don't wait
    tk_do_one_event(M, L),		% fails if no events
    (
        tk_num_main_windows(0) ->
	List = ["exit"]
    ;
        (
            L = [] ->				% no Prolog event
            tk_get_event(Mask, List)
        ;
            List = L
        )
    ).



 %%
 %% This just defines call_prolog, which we have disabled by now. MCL
 %%
 %% add_new_commands :-
 %%     tcl_eval_string(
 %% 	'proc call_prolog {goal} {
 %% 	    global var;
 %% 	    set res [prolog "tcl_call $goal X" tk];
 %% 	    if {$res != "success"} {return $res};
 %% 	    set list $var(X);
 %% 	    unset var(X);
 %% 	    foreach pair $list {
 %% 		set var([lindex $pair 0]) [lindex $pair 1];
 %% 	    };
 %% 	    return $res
 %% 	}'
 %%     , _).
