time(Goal,Time) :- 
	statistics(runtime,[Global1,_]),
	call(Goal),
	statistics(runtime,[Global2,_TimeSinceLastStat]),
	Time is Global2 - Global1.

time(Goal) :-
	time(Goal,Time),
	print('Time for goal: '),print(Goal),
	print(' is: '),print(Time), print(' ms'),nl.

add_new_time_log_entry(NameOfEntry,LogFile) :-
        open_log(LogFile,Stream),
        nl(Stream),
        print(Stream,NameOfEntry),
        close(Stream).
       

time_log(Goal,LogFile) :-
        open_log(LogFile,Stream),
        time(Goal,Time),
        format(Stream,"\t",[]),
        print(Stream,Time),
        close(Stream).

open_log(LogFile,Stream) :-
        (open(LogFile,append,Stream,[])
         -> true
         ;  open(LogFile,write,Stream,[])).
