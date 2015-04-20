:- module(clock,[clock/1],
	[assertions, basicmodes, regtypes, foreign_interface]).

:- true pred clock(go(T)):: int + (foreign,returns(T)).
:- impl_defined([clock/1]).

:- extra_compiler_opts(['-O2']).
:- extra_compiler_opts('LINUXi86',['-ffast-math']).
:- use_foreign_library([m]).
