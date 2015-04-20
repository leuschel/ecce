:- module(file,[shut_down/0,p/2]).
:- use_module(library('ams/amsrt')).
:- multifile '$ams$actmod'/3,'$ams$app'/1.
:- initialization(start_up("http://clip.dia.fi.upm.es/~clip/file.pl")).
:- on_abort(shut_down).
:- data my_sid/1.

start_up(Mid):-
    '$ams$app'(App),
    '$ams$actmod'(file,Mode,Host),
    absolute_file_name(App,FilePath),
    atom_concat('clip.dia.fi.upm.es',FilePath,Suffix1),
    atom_concat('file://',Suffix1,Aid),
    ams_startup(Host,Aid,file,Mid,Mode,Sid),
    asserta_fact(my_sid(Sid)).

shut_down:-
    my_sid(Sid),
    ams_shutdown(Sid).

p(_3535,_3536) :-
    my_sid(Sid),
    ams_call(Sid,p(_3535,_3536)).
