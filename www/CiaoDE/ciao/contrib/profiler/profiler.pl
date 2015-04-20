% The Profile package

:- discontiguous cost_center/2.
:- multifile cost_center/2.
%:- data cost_center/2.

:- load_compilation_module(library('profiler/profiler_tr')).

:- use_module(library('profiler/profiler_rt')).

:- add_sentence_trans(profiler_def/3).

:- op(1150, fx, [profile,noprofile]).
