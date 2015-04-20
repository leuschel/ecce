%%---------------------------------------------------------------------
%%
%% RADIOBUTTON CLASS
%%
%%---------------------------------------------------------------------

:- class(radiobutton_class).
:- inherit_class(library('tcltk/examples/interface/widget_class')).


:- data        variable/1.
:- inheritable variable/1.

variable('').

:- export(set_variable/1).

set_variable(Variable) :-
        atom(Variable),
        set_fact(variable(Variable)),
        notify_changes.

:- export([get_variable/1]).

get_variable(Variable) :-
        variable(Variable).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(radiobutton).

%creation_options([' -command ',sqb(C)," "|Other]) :-
%creation_options([' -command ',C," "|Other]) :-
creation_options([' ',min(variable),V|Other]) :-
        variable(V),
        inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

radiobutton_class.
radiobutton_class(Owner) :-
        radiobutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
