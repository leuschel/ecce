
%
% ProTcl 1.0
%
%	Definitions common to Quintus-like foreign language interface
%
% Author: Micha Meier
% Date:   September 93
%

%
% sccsid("@(#)foreign.pl	1.8          96/01/02").
% sccscr("@(#)  Copyright 1993 ECRC GmbH ").
%

:- ensure_loaded(string_utils).
:- ensure_loaded(dirs).

foreign_file('tk.o',
	[tk_init,
	 tk_option]).
foreign_file('protcl.o',
	[tk_clear_options,
	 tcl_eval_string,
	 tk_num_main_windows,
         tk_do_one_event_atom,
	 input_ready]).

foreign(tk_init, c, tk_init(+string)).
foreign(tk_clear_options, c, tk_clear_options).
foreign(tk_option, c, tk_option(+string, +string)).
foreign(tcl_eval_string, c, tcl_eval_string(+string, -string, [-integer])).
foreign(tk_num_main_windows, c, tk_num_main_windows([-integer])).
 %% This returns in one atom the whole Tcl event list
foreign(tk_do_one_event_atom, c, tk_do_one_event_atom(+integer, [-string])).
foreign(input_ready, c, input_ready(+integer, [-integer])).

:-
        tcl_library(TclLib),
        tk_library(TkLib),
        protcl_source(Pro),
        xlibsw(XLib),
        concat_atoms('-L', TclLib, TC),
        concat_atoms('-L', TkLib, TK),
        concat_atoms(Pro, '/libinit.a', Init),
        concat_atoms('-L', XLib, XL),
        load_foreign_files(
                              ['tk.o', 
                               'protcl.o'], 
                              [Init,
                               TC, 
                               TK,
                               '-ltk4.0 -ltcl7.4 -lm', 
                               XLib]
                          ).
