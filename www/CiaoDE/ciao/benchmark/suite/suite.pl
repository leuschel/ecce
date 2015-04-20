:- module(suite, [main/0]).

% :- use_module(size).
:- use_module(speed).
:- use_module(fib).
:- use_module(bn).
:- use_module('wave2-upd-arrays').
:- use_module('wave2-original').
:- use_module(witt).

:- use_module(library(write)).
:- use_module(library(prolog_sys)).

main:-
        queens,
        fib,
        bignums,
        wave,
        wave_orig,
        do_witt.
%        statistics.


 %% knights:-
 %%         line,
 %%         write('knights tour.  Should fail.'), nl,
 %%         knights(4, _Board).
 %% knights.


 %% size:-
 %%        line,
 %%        write('Memory operations.  Should give info about mem. and gc'), nl,
 %%        mem.

queens:- speed.


 %% trie:- 
 %%         line,
 %%         write('Construct a trie and search some words in it'), nl,
 %%         consulta([variable, speed, queen, var],['knights_2.pl',
 %%         mundo, 'robot.pl', 'size.pl', 'trie.pl', 'wumpus.pl', 'speed.pl'],
 %%         Donde),
 %%         write(donde = Donde), nl.

 %% wumpus:-
 %%         line,
 %%         write('Solve a wumpus world'), nl,
 %%         w(mundo),
 %%         nl.

fib:- do_fib.
        
bignums:- do_bignums.

witt:- do_witt.

 %% guardians:-
 %%         line,
 %%         write('Solving the guardians and locks problem...'), nl,
 %%         guardians(1000,200,Unlocked),
 %%         write('Unlocked cells: '),
 %%         write(Unlocked),
 %%         write('.'), nl.

 %% jugs:-
 %%         line,
 %%         write('Solving the jugs problem...'), nl,
 %%         solve_jugs(Solution),
 %%         write('Solution: '),
 %%         write(Solution),
 %%         write('.'), nl,
 %%         line.


 %% multi(0, _What).
 %% multi(N, What):-
 %%         N > 0,
 %%         launch_goal(What),
 %%         N1 is N - 1,
 %%         multi(N1, What).



line:- write(
'***************************************************************************'
), nl.
