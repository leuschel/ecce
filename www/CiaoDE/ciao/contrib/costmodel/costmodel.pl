:- load_compilation_module(library('costmodel/costmodel_tr')).

:- use_module(library('costmodel/costmodel_rt')).

:- add_sentence_trans(costmodel_def/3).

:- op(1050, xfy, ['->']).

:- op(1150, fx, [costmodel]).

:- op(1050, xfy, ['::']).

:- op(1050, xfy, ['=>']).
