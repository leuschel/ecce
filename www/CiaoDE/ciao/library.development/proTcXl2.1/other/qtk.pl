%
% Quintus version of ProTcl
%
% Author: Micha Meier
% Date:   September 93
%

%
% sccsid("@(#)qtk.pl	1.5          94/09/11").
% sccscr("@(#)  Copyright 1993 ECRC GmbH ").
%

:- module(tk, [
	tcl_eval/1,
	tcl_eval/2,
	tk_demo/0,
	tcl_test/0,
	tk_test/0,
	tk_file/2,
%	tk/1,
	tk_do_one_event/1,
	tk_num_main_windows/1,
	tk_main_loop/0]).

:- compile(tk_common).
:- compile(foreign).
