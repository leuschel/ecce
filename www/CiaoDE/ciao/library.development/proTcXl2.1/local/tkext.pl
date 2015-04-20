%
% ECLiPSe version of the Prolog Tk interface with a third-party extension
%
% Author: Micha Meier
% Date:   September 93
%

%
% sccsid("@(#)tkext.pl	1.2          94/06/16").
% sccscr("@(#)  Copyright 1993 ECRC GmbH ").
%

:- current_module(tk) ->
	printf(error, "%n*** The tkext library cannot be loaded after tk%n%n%b", []),
	abort
    ;
	true.

:- module_interface(tkext).

:- export
	tk_load/1.

:- begin_module(tkext).

:- tool(tk_load/1, tk_load_body/2).

:- import
	lib/2
    from sepia_kernel.

tk_load_body(Files, Mod) :-
    lib(tk),
    tk_load_libs(Files, Mod),
    lib(tk, Mod).
