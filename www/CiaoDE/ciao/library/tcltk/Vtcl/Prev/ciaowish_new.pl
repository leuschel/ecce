%%------------------------------------------------------------------------
%%
%% CIAOWISH main executable
%%
%% ANGEL FERNANDEZ PINEDA
%%
%% NovJanuary - 1998
%%
%%------------------------------------------------------------------------

:- use_module(library('tcltk/wish')).

:- use_module(engine(internals),[module_concat/3,term_to_meta/2]).
:- use_module(library(compiler)). % for use_module/1

:- use_module(library(concurrency)).

%%------------------------------------------------------------------------

main([]) :-
	wish:wish,
	initialize_ciaowish,
	current_input(From),
	eng_call(daemon,create,create,ID),
	process_input(From),
	eng_kill(ID),
	wish:destructor.

%%------------------------------------------------------------------------
%% CIAOWISH initialization
%%------------------------------------------------------------------------

initialize_ciaowish :-
	core(String),
	wish:send_code_string(String),
	fail.

initialize_ciaowish.

%%------------------------------------------------------------------------
%% Send standard input to wish
%%------------------------------------------------------------------------

process_input(From) :-
	catch(getline(From,CodeLine),_,fail),
	wish:send_code_string(CodeLine),
	!,
	process_input(From).

process_input(_).

%%------------------------------------------------------------------------

getline_after(-1,[]) :- !,fail. % EOF
getline_after(10,[]) :- !.
getline_after(13, R) :- !, % Return, delete if at end of line
        get_code(C),
        getline_after(C, Cs),
        ( Cs = [] ->
              R = []
        ; R = [13|Cs]
        ).
getline_after(C, [C|Cs]) :-
        get_code(C1),
        getline_after(C1, Cs).

getline(Stream, Line) :-
        current_input(OldIn),
        set_input(Stream),
	get_code(C),
        getline_after(C,Line),
        set_input(OldIn).

%%------------------------------------------------------------------------
%% TERM Reader daemon.
%%------------------------------------------------------------------------

daemon :-
	catch(process_tcltk_calls,_,fail),
	daemon.

daemon.

process_tcltk_calls :-
	wish:receive_term(T),
	process_term(T),
	nl.

process_term(Term) :-
	display(' - RECEIVED TERM: '),
	display(Term),
	display(' -'),nl.
%	catch(call(Goal),_,fail).
	
process_term(_).

%%------------------------------------------------------------------------
%% Initial code.
%%------------------------------------------------------------------------

core("set prolog_variables(_) none").

core("proc prolog {goal} {").
core("uplevel 0 [prolog_term prolog_goal($goal)] ").
core("}").
