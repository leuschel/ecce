%
% SICStus version of ProTcl
%
% Author: Micha Meier
% Date:   September 93
%

%
% sccsid("@(#)stk.pl	1.6          94/09/19").
% sccscr("@(#)  Copyright 1993 ECRC GmbH ").
%

:- module(tk, [
	tcl_eval/1,
	tcl_eval/2,
	tk_demo/0,
	tcl_test/0,
	tk_test/0,
	tk_file/2,
	tk/1,
	tk_do_one_event/1,
	tk_num_main_windows/1,
	tk_main_loop/0]).

tk(Opts) :-
    tk_init('', Opts),
    update.

append([], L, L).
append([X|L1], L2, [X|L3]):- append(L1, L2, L3).

:- compile(tk_common).
:- compile(foreign).
