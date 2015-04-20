:- module(misc,
	[get_coord/3,delta_coord/4,scale_coord/3,save_coord/1,retrieve_coord/1,
	atom_concat/3,index_list/3,
	identif_tr/2, identif_st/3,
	state_interp/2,state_vars/2,state_names/2,state_dom/2,domain_range/3,
	create_crd/2,get_crd/2,set_crd/2,
	protect/1,protect_2/1,
	try_const/2,do_constraint/2,get_constraint/7,
	definite_variables/1,at_least_two/2,
	list_of_vars_names/6,list_of_vars_names/5,
	current_coord/1]).

:- [ciaocompat].

%:- use_module(dirs).

:- include(library(assertions)).

:- comment(title,"Miscellaneous predicates (@tt{misc})").

:- comment(module,"This module provides different utility predicates for
   managing object coordinates and internal structures, searching, and
   miscellaneous constraint related predicates.").

:- include(library(basicmodes)).
:- include(library(types)).
% :- include(library('types/type_ops')).
% :- use_module(library('types/basictypes')).
:- include(library('isomodes')).

:- use_module(library(basicprops)).

:- use_module(library(clpfd)).
:- include(clpfd_ops).
:- use_module(library(lists)).
:- use_module(internal_types).
:- use_module(library(system)).

:- use_module(library(event)).
:- use_module(library(tcltk_general)).
:- use_module(library(updatable)).
:- use_module(library(var_hist)).
:- use_module(library(plot_2d)).
:- use_module(library(allv)).
:- use_module(library(allven)).
:- use_module(library(fd_general)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gestion de las coordenadas

%:- comment(doinclude,get_coord/3).
:- pred get_coord(+Coord, -X, -Y)
        :: point * integer * integer
        # "Provides the X-value and Y-value of the coordinate".
:- pred get_coord(-Coord, +X, +Y)
        :: point * integer * integer
        # "Provides a coordinate with X-value and a Y-value".
% Asigna o devueve unas coordenadas
get_coord(coord(X, Y), X, Y).

%:- comment(doinclude,delta_coord/4).
:- pred delta_coord(+Coord1, -Coord2, +DeltaX, +DeltaY)
        :: point * point * integer * integer
        # "Increases a coordinate @var{Coord1} adding @var{DeltaX} to X-value
	  and @var{DeltaY} to Y-value.".  
% Incrementa el valor de una coordenada DeltaX y DeltaY
delta_coord(coord(X, Y), coord(X1, Y1), DeltaX, DeltaY):-
	X1 is X + DeltaX,
	Y1 is Y + DeltaY.

%:- comment(doinclude,scale_coord/3).
:- pred scale_coord(+Coord1, -Coord2, +Scale)
        :: point * point * integer
        # "Scales @var{Coord1} with a factor @var{Scale} given the result 
	  in @var{Coord2}.".
% Escala unas coordenadas un factor Scale
scale_coord(coord(X, Y), coord(Xs, Ys), Scale):-
	Xs is X * Scale,
	Ys is Y * Scale.

%:- comment(doinclude,save_coord/1).
:- pred save_coord(+Coord)
	:: point
        # "Asserts @var{Coord} in the data base as the fact 
	  @pred{current_coord/1}.".
% Graba las coordenadas
save_coord(C):-
	assert(current_coord(C)).

%:- comment(doinclude,retrieve_coord/1).
:- pred retrieve_coord(-Coord)
	:: point
        # "Retracts an atom asserted as @pred{current_coord/1}.".
% Lee las coordenadas
retrieve_coord(C):-
	retract(current_coord(C)),
	retractall(current_coord(C)).  % Safety -- in case execution break

%:- comment(doinclude,atom_concat/3).
:- pred atom_concat(+Atom1, +Atom2, -AtomRes)
	:: atm * atm * atm
        # "Concat @var{Atom1} with @var{Atom2} giving the result in 
	  @var{AtomRes}.".
% Concatena dos atomos A y B
atom_concat(A,B,AB) :-
        name(A,AS),
        name(B,BS),
        append(AS,BS,ABS),
        name(AB,ABS).

%:- comment(doinclude,identif_tr/2).
:- pred identif_tr(+Coord,-Id)
	:: point * atm
        # "Creates an identifier @var{Id} for a TCL/TK widget using its
	  coordinate @var{Coord}.". 
% Crea un identificador para cada cuadro de Variable History en funcion de su
% coordenada
identif_tr(C,Id):-
      get_coord(C,X,Y),
      atom_concat('.r',X,Id1),
      atom_concat(Id1,'_',Id2),
      atom_concat(Id2,Y,Id).

%:- comment(doinclude,identif_st/3).
:- pred identif_st(+Var,+Value,-Id)
	:: atm * integer * atm
        # "Creates an identifier @var{Id} for a TCL/TK rectangle using the 
	  name of var @var{Var} and the value @var{Value}.".
% Crea un identificador Id con el que te puedes referir a los rectangulos,
% dada la variable y el valor que simboliza dicho rectangulo
identif_st(Var,Value,Id):-
	atom_concat('''',Var,Id1),
	atom_concat(Id1,'@',Id2),
	atom_concat(Id2,Value,Id3),
	atom_concat(Id3,'''',Id).
%	atom_concat(Var,Value,Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Asigna y cambia el estado del problema
%:- comment(doinclude,state_interp/2).
:- pred state_interp(+State,-Interp)
	:: state * interp
        # "Returns the @concept{TCL/TK interpreter} PROLOG is working with.".
:- pred state_interp(-State,+Interp)
	:: state * interp
        # "Assigns the TCL/TK interpreter to the structure @var{State}.".
state_interp(info(I,_Vars,_Names,_Dom,_C),I).

%:- comment(doinclude,state_vars/2).
:- pred state_vars(+State,-Vars)
	:: state * list(fdvar)
        # "Returns the @concept{Finite Domain variables} we are inspecting.".
:- pred state_vars(-State,+Vars)
	:: state * list(fdvar)
        # "Stores in the @var{State} the Finite Domain variables we will inspect.".
state_vars(info(_I,Vars,_Names,_Dom,_C),Vars).

%:- comment(doinclude,state_names/2).
:- pred state_names(+State,-Names)
	:: state * list(atm)
        # "Returns the names of the variables we are inspecting.".
:- pred state_names(-State,+Names)
	:: state * list(atm)
        # "Stores in the @var{State} the names of the variables we will inspect.".
state_names(info(_I,_Vars,Names,_Dom,_C),Names).

%:- comment(doinclude,state_dom/2).
:- pred state_dom(+State,-Domain)
	:: state * list(limit)
        # "Returns the bounds for each of the Finite Domain variables we
	  are inspecting.".
:- pred state_dom(-State,+Vars)
	:: state * list(limit)
        # "Stores in the @var{State} the limits for each of the Finite Domain variables we 
	  will inspect.".
state_dom(info(_I,_Vars,_Names,Dom,_C),Dom).

%:- comment(doinclude,domain_range/3).
:- pred domain_range(+Limit,-Min,-Max)
	:: limit * integer * integer

        # "Returns the minimum domain value @var{Min} and the maximum domain
          value @var{Max} from the initial domain of a variable.".

:- pred domain_range(-Limit,+Min,+Max)
	:: limit * integer * integer
        # "Assigns the lower and the upper bounds of a domain limits.".
domain_range(min_max(Min,Max),Min,Max).

%:- comment(doinclude,create_crd/2).
:- pred create_crd(+State,+Coord)
	:: state * point
        # "Associates a coordinate given by @var{Coord} to @var{State} using backtrackable destructive assignement.".
create_crd(info(_I,_Vars,_Names,_Dom,Var),C):-create_updatable(Var,C).

%:- comment(doinclude,get_crd/2).
:- pred get_crd(+State,-Coord)
	:: state * point
        # "Returns the last coordinate saved in @var{State}. Fail if you did
	  not call @pred{create_crd/2} before.".
get_crd(info(_I,_Vars,_Names,_Dom,Var),C):-get_updatable(Var,C).

%:- comment(doinclude,set_crd/2).
:- pred set_crd(+State,+Coord)
	:: state * point

        # "Changes coordinate value in @var{State}. Fail if
          @pred{create_crd/2} was not called before.".

set_crd(info(_I,_Vars,_Names,_Dom,Var),C):-update_updatable(Var,C).

%:- comment(doinclude,protect/1).
:- pred protect(+Action)
	:: action

        # "Protects code from being backtracked over by creating a
	  choicepoint. It is used to control backtracking when you press
	  @concept{reset} button.".

% Evita que se haga backtracking cuando se pulsa reset
protect(_H).
protect(H):-
	(call(H)->
	    retract(H),
	    protect(H)
	;
	    fail).

%:- comment(doinclude,protect_2/1).
:- pred protect_2(+Action)
	:: action2
        # "It is the same as protect, but it is used when you press commit.".
% Evita que se haga backtracking cuando se pulsa commit
protect_2(_H).
protect_2(event:'$no_back_ac'(H)):-
	(call(event:'$no_back_ac'(_))->
	    retractall(event:'$no_back_ac'(_)),
	    assert(event:'$no_back_ac'(H)),
	    protect_2(event:'$no_back_ac'(H))
	;
	    fail).

%:- comment(doinclude,try_const/2).
:- pred try_const(+Constraint,-Result)
	:: constraint * atm
        # "Executes @var{Constraint} and returns 'yes' in @var{Result} denoting it success. In case of failure it returns 'no'". 
% Se asegura de que la restriccion puesta no llega a un estado inconsistente
try_const(R,yes):-
	call(R),!.
try_const(_,no).


:- comment(doinclude,put_back/2). 
:- pred put_back(+L1,-L2) 
	:: string * string
        # "Adds neccessary backslashes in order to show the input user
constraint correctly. L1 is the string given by TCL/TK and L2 is the string
that Prolog shows in the status label of the main window. That is needed to be done due to the escape sequences used in both languages.".

put_back([],[]).
put_back([92|Rest],[92,92|Rest1]):- put_back(Rest,Rest1).
put_back([X|Rest],[X|Rest1]):- X\==92,put_back(Rest,Rest1).

%:- comment(doinclude,do_constraint/2).
:- pred do_constraint(+State,+Constraint)
	:: state * constraint

        # "Executes a @var{Constraint} given by the user as input and shows
	  the effects of its execution in all active windows if success. If
	  failure, it displays the message 'Failed contraint'.".

do_constraint(_State,[]).
do_constraint(State,Res):-
	state_interp(State,I),
	name(String,Res),
	mktemp('/tmp/traceXXXXXX',File),
	open(File,write,St),
	write_term(St,String,[]),
	close(St),
	open(File,read,St2),
	read_term(St2,T,[variable_names(Lv)]),
        unify_vars(State,T,Lv),
	(valid_constraint(T)->
	   state_vars(State,Vars),
	   active_values(Vars,Oldvalues),
	   real_values(Vars,Oldvalues2),
	   try_const(T,Valid),
	   (Valid=yes->
	      enable_b(I,'.buttons.reset'),
	      put_back(Res,Res2),
	      name(KK,Res2),
	      set_tcl_var(I,state,KK),
	      set_tcl_var(I,refresh,on),
	      visualize_vh(State),
	      visualize_2dp(State),
	      visualize_allv(State,Oldvalues),
	      visualize_allven(State,Oldvalues2)	      
	   ;
	      set_tcl_var(I,state,'"Failed constraint"')
	   )
	;
	   set_tcl_var(I,state,'"Failed command"')
	),
	close(St2),
	delete_file(File).

:- comment(doinclude,unify_vars/3).
:- pred unify_vars(+State,+Term,?Names)
	:: state * term * list(atm=var)

	# "Unifies each variable in @var{Term} with a Finite Domain
	  variable of the problem we are inspecting when an input user
	  constraint is given.".

unify_vars(_State,_T,[]).
unify_vars(State,T,[Name=Var|Lv]):-
	state_vars(State,Vars),
	state_names(State,Names),
	index_list(Name,Names,Pos),
	index_list(Var1,Vars,Pos),
	Var=Var1,
	unify_vars(State,T,Lv).

:- comment(doinclude,valid_constraint/1).
:- pred valid_constraint(+Term)
	:: constraint

        # "Checks if @var{Term} is a valid constraint expansion, preventing
	  errors in user input constraints.".

valid_constraint(T):-
	(predicate_property(T,interpreted),!;predicate_property(T,compiled)).

%:- comment(doinclude,get_constraint/7).
:- pred get_constraint(+Interp,+Id,+Vars,+Names,-Var,-Value,+Status)
	:: interp * atm * list(fdvar) * list(atm) * fdvar * integer * string

        # "Returns the Finite Domain variable (@var{Var}) and the value
	  (@var{Value}) associated to the TCL/TK object identifier
	  (@var{Id}), in order to make an user input constraint like
	  ""@var{Var} is not equal to @var{Value}"". @var{Id} has the form
	  @var{Var}@@@var{Value}.".

% Operacion contraria a identif: obtiene el valor (Value) y la variable (Var) 
% representada por un identificador Id. Vars y Names son la lista de Variables
% y la lista de nombres de las variables.
get_constraint(I,Id,Vars,Names,Var,Value,Status):-
	name(Id,L),
	split_id(L,Namevar,Value),
	atom_concat('constraint(',Namevar,Ms1),
	atom_concat(Ms1,',',Ms2),
	atom_concat(Ms2,Value,Ms3),
	atom_concat(Ms3,')',Ms),
	set_tcl_var(I,Status,Ms), 
	index_list(Namevar,Names,Pos),
	index_list(Var,Vars,Pos).

:- comment(doinclude,split_id/3).
:- pred split_id(+String,-Name,-Value)
	:: string * atm * integer
        # "Generates the name of a Finite Domain variable and a value from 
	  a string that represents a identifier.".
split_id(L,Name,Value):-
	append(C,[64|Resto],L),
	name(Name,C),
	name(Value,Resto).

%:- comment(doinclude,index_list/3).
:- pred index_list(+Element,+List,-Pos)
	:: any * list(any) * integer
        # "Searches @var{Element} in @var{List}. If @var{Element} is found, 
	  @var{Pos} represent the position of @var{Element} in @var{List}.".
:- pred index_list(-Element,+List,+Pos)
	:: any * any * integer
        # "Returns the element in @var{List} which is at @var{Pos} position.".
% Busca en la Lista el elemento E y devuelve la posicion Pos en la que aparece
index_list(E,Lista,Pos):- index_list(E,Lista,1,Pos).
index_list(E,[E|_Resto],Pos,Pos).
index_list(E,[_Y|Resto],Posact,Pos):-
	P1 is Posact+1,
	index_list(E,Resto,P1,Pos).

%:- comment(doinclude,definite_variables/1).
:- pred definite_variables(+State)
	:: state

        # "Checks the definiteness of the Finite Domain variables we are
	  inspecting.".

% Indica si ya hemos asignado valores a todas las variables del problema
definite_variables(State):- 
	state_vars(State,Vars),
	state_interp(State,I), 
	is_definite_variables(I,Vars).

:- comment(doinclude,is_definite_variables/2).
:- pred is_definite_variables(+Interp,+Vars)
	:: interp * list(fdvar)
        # "See @pred{definite_variables/1}. It is called from it.".
is_definite_variables(I,[]):- set_tcl_var(I,alldef,yes).
is_definite_variables(I,[V|RestV]):-
	integer(V),
	is_definite_variables(I,RestV).

%:- comment(doinclude,at_least_two/2).
:- pred at_least_two(+Interp,-Num)
	:: interp * integer
        # "Provides the number of visualizations currently active in @var{Num}.".
at_least_two(I,Num):-
	Num1 is 0,
	(get_tcl_var(I,p2d,set)-> 
	    Num2 is Num1+1
	;
	    Num2 is Num1),
	(get_tcl_var(I,vh,set)->
	    Num3 is Num2+1
	;
	    Num3 is Num2),
	(get_tcl_var(I,allv,set)->
	    Num4 is Num3+1
	;
	    Num4 is Num3),
	(get_tcl_var(I,allven,set)->
	    Num is Num4+1
	;
	    Num is Num4).
