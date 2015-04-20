:- module(intended_effort,[intended_effort/5,total_amos/1],[assertions,functions]).

:- use_module(library(aggregates),[findall/3]).

:- comment(intended_effort(Project,WP,Task,Partner,MM), "@var{Partner}
intends to devote @var{MM} man moths to @var{Task} in @var{WP} of
@var{Project}").

:- discontiguous intended_effort/5.

:- include(intended_effort_upm).
:- include(intended_effort_bristol).
:- include(intended_effort_soton).
:- include(intended_effort_ruc).

:- check pred intended_effort(A,B,C,D,E) => ground([A,B,C,D,E]).
