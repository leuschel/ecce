

tree1(a,0).
tree1(R,Cnt) :- Cnt>0,C1 is Cnt - 1,
	 tree1(X,C1), R = f(Y, Y), Y=f(X).

/* specializing tree1 may lead to tree2 by unfolding equalities: */

tree2(a,0).
tree2(f(f(X), f(X)),Cnt) :- Cnt>0,C1 is Cnt - 1,
	 tree2(X,C1).



t1 :- t1(250).
t1(C) :- time(tree1(_R,C)).
t2 :- t2(250).
t2(C) :- time(tree2(_R,C)).

time(Goal,Time) :-
	 statistics(runtime,[Global1,_]),
	 call(Goal),
	 statistics(runtime,[Global2,_TimeSinceLastStat]),
	 Time is Global2 - Global1.

time(Goal) :-time(Goal,Time),
	print('Time for goal is: '),print(Time), print(' ms'),nl,
	statistics.
	

/* [limpopo:~/Prolog] mal% sicstus -l sharing_test.pl
% compiling /Users/mal/Prolog/sharing_test.pl...
% compiled /Users/mal/Prolog/sharing_test.pl in module user, 0 msec 2496 bytes
SICStus 3.10.1 (powerpc-darwin-6.4): Fri Apr 11 19:08:02 CEST 2003
Licensed to ecs.soton.ac.uk
| ?- t1.
Time for goal is: 0 ms
memory (total)       1712096 bytes
   program space     1607804 bytes:    1190332 in use,    417472 free
   global stack        43784 bytes:       7352 in use,     36432 free
   local stack         16724 bytes:       4196 in use,     12528 free
   trail stack         22408 bytes:       1280 in use,     21128 free
   control stack       21376 bytes:        248 in use,     21128 free
       0.000 sec. for 2 global, 0 local, and 0 control space overflows
       0.000 sec. for 0 garbage collections which collected 0 bytes
       0.000 sec. for 0 atom garbage collections which collected 0 bytes
       0.050 sec. runtime
       1.460 sec. elapsed time
yes
| ?- halt.
[limpopo:~/Prolog] mal% sicstus -l sharing_test.pl
% compiling /Users/mal/Prolog/sharing_test.pl...
% compiled /Users/mal/Prolog/sharing_test.pl in module user, 10 msec 2496 bytes
SICStus 3.10.1 (powerpc-darwin-6.4): Fri Apr 11 19:08:02 CEST 2003
Licensed to ecs.soton.ac.uk
| ?- t2.
Time for goal is: 0 ms
memory (total)       1712096 bytes
   program space     1607804 bytes:    1190332 in use,    417472 free
   global stack        43784 bytes:       9352 in use,     34432 free
   local stack         16724 bytes:        196 in use,     16528 free
   trail stack         21908 bytes:        280 in use,     21628 free
   control stack       21876 bytes:        248 in use,     21628 free
       0.000 sec. for 2 global, 0 local, and 0 control space overflows
       0.000 sec. for 0 garbage collections which collected 0 bytes
       0.000 sec. for 0 atom garbage collections which collected 0 bytes
       0.050 sec. runtime
       1.200 sec. elapsed time
yes
*/