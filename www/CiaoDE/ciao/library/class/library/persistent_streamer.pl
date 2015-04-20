
:- interface(persistent_streamer).

:- export(flush/0).
:- export(write_term/1).
:- export(current_term/1).
:- export(next_term/0).
:- export(start_writing/0).
:- export(end_writing/0).
:- export(start_reading/0).
:- export(end_reading/0).
