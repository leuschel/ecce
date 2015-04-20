
:- module(do,[do/0],[ ]).

:- use_module(slots,[conc/3]).

do:- conc([],[arg],_Z), fail.
