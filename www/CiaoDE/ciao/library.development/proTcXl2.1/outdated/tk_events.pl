%
% ECLiPSe version of the Prolog Tk interface
%
% Author: Micha Meier
% Date:   September 93
%

 %% HISTORY 
 %% 14-May-1996		Manuel Carro	
 %%    Starting port to SICStus

%
% sccsid("@(#)etk.pl	1.24          96/02/12").
% sccscr("@(#)  Copyright 1995 ECRC GmbH ").
%

 %% :- module_interface(tk).
 %% 
 %% :- export
 %% 	tcl/1,
 %% 	tcl/2,
 %% 	tcl/3,
 %% 	tcl_call/2,
 %% 	tcl_command_complete/1,
 %% 	tcl_cut_fail/1,
 %% 	tcl_eval/1,
 %% 	tcl_eval/2,
 %% 	tcl_eval_string/2,
 %% 	tcl_interp/1,
 %% 	tcl_source/0,
 %% 	tcl_string/2,
 %% 	tclsh/0,
 %% 	tk_demo/0,
 %% 	tk_file/2,
 %% 	tk/1,
 %% 	tk_debug/0,
 %% 	tk_debug_events/0,
 %% 	tk_do_one_event/1,
 %% 	tk_do_one_event/2,
 %% 	tk_file_handler/2,
 %% 	tk_main_loop/0,
 %% 	tk_next_event/1,
 %% 	tk_next_event/2,
 %% 	tk_num_main_windows/1,
 %% 	tk_get_event/1,
 %% 	tk_get_event/2,
 %% 	tk_load_libs/2,
 %% 	tk_wait_input/1,
 %% 	wish/0.

% macros
 %% :- export
 %% 	expand_tcl/2.
 %% :- op(200, fx, tcl).
 %% 
 %% :- define_macro(tcl/1, expand_tcl/2, [goal]).
 %% :- define_macro(tcl/2, expand_tcl/2, [goal]).
 %% :- define_macro(tcl/3, expand_tcl/2, [goal]).
 %% 
 %% :- begin_module(tk).
 %% 
 %% :- import
 %% 	bignum/1,
 %% 	compile_term_/2,
 %% 	sepia_toplevel_prompt/2,		% default 153 handler
 %% 	symbol_address/2
 %%     from sepia_kernel.

 %% MCL: Marked with ** those already present for SICStus

 %% :- make_local_array(saved_input).
 %% 
 %% :- external(tk_init/1).  /* ** */
 %% :- external(tk_clear_options/0). /* ** */
 %% :- external(tk_option/2). /* ** */
 %% :- external(tk_do_one_event/2).  /* Check */
 %% :- external(tcl_eval_string/2). /* ** */
 %% :- external(tcl_command_complete/1).
 %% :- external(tcl_cut_fail/1).
 %% :- external(tk_num_main_windows/1). /* ** */
 %% :- external(tk_fd_handler/3).
 %% :- external(tcl_interp/1).

% We assume that files may contain relative pathnames and the loading
% may be done from another directory. Therefore, the pathnames of the
% ProTcl files have to be absolute.

 %% :- compile(dirs).
 %% :- compile(tk_common).
 %% :- compile(tcl).

:- consult(foreign).
:- consult(tk_common).
:- consul(tcl).


 %% absolute_objects(ObjectString, Cwd, Arch, Objects, OL) :-
 %%     open(ObjectString, string, S),
 %%     get_chtab(0'., OldT),
 %%     set_chtab(0'., lower_case),
 %%     add_path(S, Cwd, Arch, Objects, OL),
 %%     set_chtab(0'., OldT).

 %% add_path(S, Cwd, Arch, Objects, OL) :-
 %%     read_token(S, File, Type),
 %%     (Type = end_of_file ->
 %% 	close(S),
 %% 	Objects = OL
 %%     ;
 %% 	concat_string([Cwd, "../", Arch, "/", File, " "], AbsFile),
 %% 	Objects = [AbsFile|NewO],
 %% 	add_path(S, Cwd, Arch, NewO, OL)
 %%     ).

 %% :-
 %%     getcwd(Cwd),
 %%     get_flag(hostarch, Arch),
 %%     get_flag(object_suffix, O),
 %%     concat_string(["../", Arch, "/dirs"], Dirs),
 %%     get_flag(installation_directory, Inst),
 %%     concat_atom([Inst, "/lib_graphic/tk"], ProTcl),
 %%     compile(Dirs),
 %%     set_error_handler(211, fail/0),
 %%     xlibsw(XLib),
 %%     (symbol_address(p_tk_init, _) ->
 %% 	% it is statically linked
 %% 	LOAD = (Files = "" -> true; concat_string([Files, " ", XLib], Load), load(Load))
 %%     ;
 %%     O == "so" ->
 %% 	concat_string([Cwd, "../", Arch, "/tk.so"], Load),
 %% 	LOAD = (Files = "" -> load(Load); load(Files))
 %%     ;
 %% %    O == "o" ->
 %% 	tcl_library(TclLib),
 %% 	tk_library(TkLib),
 %% 	protcl_objects(ObjectAtom),
 %% 	atom_string(ObjectAtom, ObjectString),
 %% 	absolute_objects(ObjectString, Cwd, Arch, Objects, OL),
 %% 	OL = [Files,
 %% 	    " -L", Cwd, "../", Arch,
 %% 	    " -L", TclLib,
 %% 	    " -L", TkLib,
 %% 	    " -linit -ltk4.0 -ltcl7.4 ", XLib, " -lm"],
 %% 	LOAD = (concat_string(Objects, Load), load(Load))
 %%     ),
 %%     reset_error_handler(211),
 %%     compile_term([(
 %% tk_load_libs(Files, Module) :-
 %% 	LOAD,
 %% 	external(tk_init/1, p_tk_init),
 %% 	external(tk_clear_options/0, p_tk_clear_options),
 %% 	external(tk_option/2, p_tk_option),
 %% 	external(tk_do_one_event/2, p_tk_do_one_event),
 %% 	external(tcl_eval_string/2, p_tcl_eval_string),
 %% 	external(tcl_command_complete/1, p_tcl_command_complete),
 %% 	external(tcl_cut_fail/1, p_tcl_cut_fail),
 %% 	external(tcl_interp/1, p_tcl_interp),
 %% 	external(tk_num_main_windows/1, p_tk_num_main_windows),
 %% 	external(tk_fd_handler/3, p_tk_fd_handler)
 %%     ),
 %% protcl_source(ProTcl)
 %%     ]).

 %% :-
 %% 	(current_module(tkext) ->
 %% 	    true			% loading will be done from tkext
 %% 	;
 %% 	    tk_load_libs("", '')
 %% 	).

 %% Already in TK
 %% tcl_eval(S, R) :-
 %%     (atom(S); string(S)),
 %%     !,
 %%     tcl_eval_string(S, R).
 %% tcl_eval(List, R) :-
 %%     concat_string(List, S),
 %%     tcl_eval_string(S, R).
 %% 
 %% tk(Opts) :-
 %%     tk_init('', Opts).

% For compatibility with other systems

tk_do_one_event(Mask) :-
    tk_do_one_event(Mask, L),
    (L = [] ->
	true
    ;
	error(333, L)
    ).

tk_file_handler(Stream, What) :-
    get_stream_info(Stream, fd, Fd),
    get_stream(Stream, NS),
    tk_fd_handler(Fd, NS, What).

% Wait for input from a given stream and process Tk events in the meantime
tk_wait_input(Stream) :-
    (tk_num_main_windows(0) ->
	true					% no events if no windows
    ;
	get_stream_info(Stream, fd, Fd),
	get_stream(Stream, NS),
	tk_fd_handler(Fd, NS, on),
	tk_loop(NS),
	tk_fd_handler(Fd, NS, off)
    ).

tk_loop(Stream) :-
    tk_do_one_event(0, L),
    (tk_num_main_windows(0) ->
	error(333, ["exit"]),			% we are done
	reset_error_handler(153)
    ;
    atom(L) ->				% no Prolog event
	tk_loop(Stream)
    ;
    L = ["file", Stream] ->
	true					% there is something to read
    ;
	error(333, L),				% invoke the handler
	tk_loop(Stream)
    ).


% The new handler for the prompt event - until there is data to read,
%  we serve Tk events. Unfortunately this does not help when we block
%  in a read which is not issued from the top-level loop.
tk_prompt(_, M) :-
    sepia_toplevel_prompt(_, M),
    tk_wait_input(toplevel_input).


% A wish-like facility to allow typing Tcl commands directly
wish :-
    wish([]).

tclsh :-
    wish([nodisplay]).

wish(Opts) :-
    (tcl_eval('') ->		% see if we have an interpreter
	true
    ;
	tk(Opts)
    ),
 %% This code set the error handler 333 for the tk events mechanism.
 %% We are not handling errors by now. MCL.
 %%     (tk_num_main_windows(0) ->
 %% 	true
 %%     ;
 %% 	set_error_handler(333, handle_exit/2)
 %%     ),
    get_prompt(toplevel_input, P, S),
    set_prompt(toplevel_input, '', S),
    block(wish_line(S, ""), Tag, rest(Tag, P, S)),
    set_prompt(toplevel_input, P, S),
    tk_file_handler(toplevel_input, off).

wish_line(S, Partial) :-
    (Partial = "" ->
	printf(S, "%% %b", [])
    ;
	true				% no prompt if continuation
    ),
    tk_wait_input(toplevel_input),
    read_string(toplevel_input, "\n", _, String),
    !,
    (String = "" ->
	NewPartial = Partial
    ;
	concat_string([Partial, "\n", String], NewCmd),
	(tcl_command_complete(NewCmd) ->
	    NewPartial = "",
	    (tcl_eval(NewCmd, Res) ->
		printf(toplevel_output, "%Dw\n%b", [Res])
	    ;
		true
	    )
	;
	    NewPartial = NewCmd
	)
    ),
    (tcl_eval('') ->		% do we still have an interpreter?
	wish_line(S, NewPartial)
    ;
	true
    ).
wish_line(_, _) :-
    tcl exit.

rest(Tag, P, S) :-
    set_prompt(toplevel_input, P, S),
    tk_file_handler(toplevel_input, off),
    tcl_eval(exit),
    exit_block(Tag).

tcl_call(String, Res) :-
    open(String, string, s),
    readvar(s, Goal, Vars),       % parse and return variables
    close(s),
    (Goal = Module:G ->
	call(G, Module)
    ;
	call(Goal, eclipse)
    ),
    return_vars(Vars, Res).

% convert [N|V] to [N, V] because pairs do not exist in Tcl
return_vars([], []).
    return_vars([[Name|Value]|L], [[Name, Value]|LR]) :-
    return_vars(L, LR).

%% :- tool(tcl_source/0, tcl_source/1).

tcl_source(Module) :-
    getcwd(Cwd),
    concat_string(['source ', Cwd, '##'], Cmd),
    compile_term_(tcl_source(File) :- tcl(Cmd, [File]), Module).

%% :- pragma(nodebug).

tk_debug :-
    set_error_handler(252, pending_events/0),
    get_stream(debug_input, S),
    setval(saved_input, S).

pending_events :-
    tcl_eval_string(update, _) -> true; true.

/*
 * This works, but does not really give much more than 'b' and ^D

:- debug_macro(0'], "@tk_debug_events").

tk_debug_events :-
    printf(debug_output, "handling Tk events %b", []),
    getval(saved_input, S),
    tk_wait_input(S),
    get_prompt(S, P, OutS),
    set_prompt(S, P, null),
    read_string(S, "\n", _, _),		% to ignore user input
    set_prompt(S, P, OutS).
*/

handle_exit(N, ["exit"]) :-
    !,
    (tcl exit -> true; true),
    reset_error_handler(N).
handle_exit(N, E) :-
    error(default(N), E).

% Initialize the saved debug input in case that the ']' macro is used
% without tk_debug
:- get_stream(debug_input, S), setval(saved_input, S).

% :- untraceable tk_debug/0.
