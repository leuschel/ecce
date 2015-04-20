:- module('more_specific.none',['more_specific.none:more_specific_transformation'/1]).

:- set_prolog_flag(single_var_warnings,off).

%:- dynamic more_specific_transformation/1.

/* instantiates goals during the unfolding process to more specific versions */

'more_specific.none:more_specific_transformation'(Goal).


