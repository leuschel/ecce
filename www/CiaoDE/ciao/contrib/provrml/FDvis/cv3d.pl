:- module(cv3d,
	[init_state/3,
	 show_state/2,
	 show_state/1,
	 final_state/1,
	 list_of_vars_names/6,
	 list_of_vars_names/5]).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(isomodes)).
:- include(library(types)).

:- use_module(library(lists)).

:- comment(title,"Interface to CV3D").

:- comment(module,"This library provides the @concept{user interface} of
   the @concept{finite domain visualisation}.").

%:- use_module(library(internal_types)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred list_of_vars_names(+N, -Vars, -Names, +Min, +Max, +Base) 
	:: int * list(fdvar) * list(atm) * int * int * atm

        # "Creates a list of @var{N} variables and a list of
	  @var{N} names for that vars, where each
	  name is @var{Base} concat with a value starting in 0. @var{Min}
	  and @var{Max} are the initial domain limits of each variable.
          @var{Base} is the first part of the name. @var{N} is the number of
	  variables given.".
%%%%%%%%%%%%%%%%
list_of_vars_names(N, Vars, Names, Min, Max, Base):-
	create_vars(N, Min, Max, Vars),
	give_var_names(Vars, Names, 0,_, Base).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred list_of_vars_names(N, Vars, Names, Min, Max) 
	# 
	"It is the same as @pred{list_of_vars/6} except that the first part 
	  of the name for each variable is @tt{'Var'}.".
%%%%%%%%%%%%%%%%
list_of_vars_names(N, Vars, Names, Min, Max):-
	create_vars(N, Min, Max, Vars),
	give_var_names(Vars, Names, 0,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- comment(doinclude,create_vars/4).
:- pred create_vars(+N,+Min,+Max,-Vars)
	:: integer * integer * integer * list(fdvar)
        # 
	"Creates a list of @var{N} variables. Each one has as domain 
	 limits @var{Min} and @var{Max}.". 

create_vars(0, _Min, _Max, []).

create_vars(N, Min, Max, [V|Vs]):-
	N > 0,
	V in Min..Max,
	N1 is N - 1,
	create_vars(N1, Min, Max, Vs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- comment(doinclude,give_var_names/5).
:- pred give_var_names(+Vars,-Names,+Initial,-Total,+Base)
	:: list(fdvar) * list(atm) * integer * integer * atm
        # "Creates a list of names @var{Names} for the variables in 
	  @var{Vars}, starting each name with @var{Base}. @var{Initial} is a
	  number given to each variable, and @var{Total} is the number of
	  names created.".
%%%%%%%%%%%%%%%%
give_var_names([], [], I, I,_).

give_var_names([_One|R], [Name|Names], I, F, Base):-
	atom_concat(Base, I, Name),
	N is I + 1,
	give_var_names(R, Names, N, F,Base).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- comment(doinclude,give_var_names/4).
:- pred give_var_names(Vars,Names,Initial,Total)
	# "See @pred{give_var_names/5}. In this case each variable starts with 
          'var'.".
%%%%%%%%%%%%%%%%
give_var_names([], [], I, I).

give_var_names([_One|R], [Name|Names],I,F):-
	atom_concat('Var_', I, Name),
	N is I + 1,
	give_var_names(R, Names, N, F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred init_state(+Vars,+Names,-State) 
	:: list(fdvar) * list(atm) * state 

        # "That is the first predicate you must call to create the
	  visualisation windows and its buttons and to initialise the
	  @var{State} properly. It also takes care of cleaning
	  possible garbage from previous calls. @var{Names} are the
	  @concept{names of the variables} in @var{Vars}. State is a
	  returned struct you need for every call to @pred{show_state/2} or
	  @pred{show_state/1} and @pred{final_state/1}".
%%%%%%%%%%%%%%%%
init_state(Vars, Names, State):-
	retractall(misc:current_coord(_)),
	retractall(event:'$noback'(_)),
	create_main_env(I),
	state_interp(State,I),
	state_vars(State,Vars),
	state_names(State,Names),
        get_minmax(State),
	set_tcl_var(I,alldef,no),
	set_tcl_var(I,refresh,off),
	set_tcl_var(I,stop,off),
	set_tcl_var(I,draw,off),
	tk_next_event(I,_Ev),
	disable_b(I,'.buttons.start'),
	enable_b(I,'.buttons.stop'),
	enable_b(I,'.buttons.draw'),
	enable_b(I,'.buttons.ignore'),
	enable_b(I,'.buttons.add'),
	((get_tcl_var(I,p2d,set);get_tcl_var(I,allv,set);get_tcl_var(I,allven,set))->
	    enable_b(I,'.buttons.reset'),
	    enable_b(I,'.buttons.accept')
	;
	    true
	),
	at_least_two(I,Num),
	(Num>1->
	    enable_b(I,'.buttons.refresh')
	;
	    true
	),
	visualize_vh(State),
% An~adir aqui el predicado que llama a la nueva visualizacion si reset no le
% afecta. Si no an~adirlo despues de 'visualize_allv(State)'
	protect('$no_back'(reset)),
	visualize_2dp(State),
	visualize_allv(State,[]),
	visualize_allven(State,[]),
	set_tcl_var(I,mode,'"Waiting"'),
	top_level(State,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred show_state(+State,+Name)
	:: state * atm

	# "Sets a breakpoint which checks the execution mode ('Stopped',
	  'Draw', 'Ignore') and, if needed, shows the state of the
	  variables in the active windows. It also disables buttons in
	  order to avoid inconsistent states. @var{State} is the variable
	  returned by @pred{init_state/3}.  @var{Name} is an identifier for
	  the breakpoint, displayed in the status line of the main window,
	  in order to know where you are during the execution. ".
%%%%%%%%%%%%%%%%
show_state(State,Name):-
	state_interp(State,I),
	atom_concat('\"',Name,Id1),
	atom_concat(Id1,'\"',Id),
	set_tcl_var(I,state,Id),update(I),
	show_state(State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred show_state(State) 
	# "See @pred{show_state/2}.".
%%%%%%%%%%%%%%%%
show_state(State):-
   	state_interp(State,I),
	(get_tcl_var(I,alldef,no)->
	    (definite_variables(State)->
		set_tcl_var(I,state,'"All variables are definite"'),
		disable_b(I,'.buttons.pause')
	    ;
	        true
	    )
	;
	    true
	),
	(get_tcl_var(I,stop,on)->
	    set_tcl_var(I,stop,off),
	    disable_b(I,'.buttons.pause'),
	    enable_b(I,'.buttons.stop'),
	    enable_b(I,'.buttons.draw'),
	    enable_b(I,'.buttons.ignore'),
	    enable_b(I,'.buttons.add'),
	    ((get_tcl_var(I,p2d,set);get_tcl_var(I,allv,set);get_tcl_var(I,allven,set))->
		enable_b(I,'.buttons.reset'),
		enable_b(I,'.buttons.accept')
	    ;
	        true
	    ),
	    at_least_two(I,Num),
	    (Num>1->
		enable_b(I,'.buttons.refresh')
	    ;
	        true
	    ),
	    visualize_vh(State),
% An~adir aqui el predicado que llama a la nueva visualizacion si reset no le
% afecta. Si no an~adirlo despues de 'visualize_allv(State)'
	    protect('$no_back'(reset)),
	    visualize_2dp(State),
	    visualize_allv(State,[]),
	    visualize_allven(State,[]),
	    top_level(State,0),
	    disable_b(I,'.buttons.stop'),
	    disable_b(I,'.buttons.draw'),
	    disable_b(I,'.buttons.ignore'),
	    disable_b(I,'.buttons.add'),
	    disable_b(I,'.buttons.reset'),
	    disable_b(I,'.buttons.accept'),
	    disable_b(I,'.buttons.refresh')
	;
	    (get_tcl_var(I,draw,on)->
		visualize_vh(State),
		visualize_2dp(State),
		visualize_allv(State,[]),
		visualize_allven(State,[])
% An~adir aqui el predicado que llama a la nueva visualizacion
	    ;
	        true
	    )
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred final_state(+State)
	:: state

        # "Shows the last state of the variables (usually the solution of
           the problem). Upon exit all windows are closed and all internal
           values deleted. @var{State} is the variable returned by
           @pred{init_state/3}. ".
%%%%%%%%%%%%%%%%
final_state(State):-
	state_interp(State,I),
	set_tcl_var(I,state,'"final_state"'),
	enable_b(I,'.buttons.add'),
	((get_tcl_var(I,p2d,set);get_tcl_var(I,allv,set);get_tcl_var(I,allven,set))->
	    enable_b(I,'.buttons.reset')
	;
	    true
	),
	at_least_two(I,Num),
	(Num>1->
	    enable_b(I,'.buttons.refresh')
	;
	    true
	),
	visualize_vh(State),
	protect('$no_back'(reset)),
	visualize_2dp(State),
	visualize_allv(State,[]),
	visualize_allven(State,[]),
	top_level(State,0),
	retractall(misc:current_coord(_)),
	retractall(event:'$noback'(_)),
	tcl_delete(I).
