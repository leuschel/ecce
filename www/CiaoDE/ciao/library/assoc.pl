
:- module(assoc,
	[ empty/1,
	  get_value/3,
	  put_value/5,
	  add_value/4,
          update_value/5,
	  one_key_list_to_table/2,
	  map/3,
	  foldl/4
%	  tabla_tiempos/5,
	],
	[assertions, hiord, regtypes]).

%:- use_module(library(prolog_sys)).
%:- use_module(library(between)).

:- use_module(library(hiordlib),
	[
	    map/3,
	    foldl/4
	]).

:- use_module(library(lists),
	[
	    length/2
	]).

:- comment(title,"Association beetwen key and value").

:- comment(author,"The Clip Group (Pablo Chico)").

:- comment(module,"This library provides facilities to do the association
   beetwen key and value. When there are few elements the association struct
   is a list of pairs, but when there are many elements, it's a avl tree."). 


%if change_tree(N) then when assoc consists of N or more elements, 
						%list --> avl_tree
change_tree(45).

:- regtype assoc_table(Assoc) # "@var{Assoc} is a associations beetwen keys and values.".

:- pred empty(Assoc) : assoc_table
   # "@var{Assoc} is an empty @tt{assoc_table}.".

empty(assoc_table([],0)).

:- regtype key(K)   # "@var{K} is a valid key in a @tt{assoc_table}.".
:- regtype value(V) # "@var{V} is a valid value in a @tt{assoc_table}.".

:- pred get_value(Assoc,K,V) : assoc_table * key * value
   # "@var{V} is the value associted to
   the key @var{K} in the assoc_table @var{Assoc}.".

get_value(assoc_table(Assoc,N),K,V) :- 
	change_tree(N_Limit),
	(
	    N > N_Limit ->
	    get_value_avl(Assoc,K,V)
	;
	    get_value_list(Assoc,K,V)
	).

%get_value if assoc is a list
get_value_list([K-V|_Rest],K,V) :- !.
get_value_list([K1-_|Rest],K2,V) :- 
	K2 @> K1, 
	get_value_list(Rest,K2,V).

%get_value if assoc is a avl_tree
get_value_avl(avl(K,V,L,R,_), Key, Val) :-
	compare(Rel, Key, K),
	get_value_avl_cmp(Rel, Key, Val, V, L, R).

get_value_avl_cmp(<, Key, Val, _, L, _) :- get_value_avl(L, Key, Val).
get_value_avl_cmp(=, _, Val, Val, _, _).
get_value_avl_cmp(>, Key, Val, _, _, R) :- get_value_avl(R, Key, Val).

:- regtype member(M) # "@var{M} is no or yes(@tt{value}).".

:- pred put_value(Assoc1,K,Assoc2,V,Member) : assoc_table * key * assoc_table * value * member
   # "Insert in @var{Assoc1} the value @var{V} associted to
   the key @var{K} and the result is @var{Assoc2}. If 
   the key @var{K} doesn't belong to the @var{Assoc1} then
   @var{Member} unifies with no. If it belong, @var{Assoc}
   is the result of modify the pair @var{K}-OldVar by @var{K}-@var{V} and
   @var{Member} is yes(OldVar).".


put_value(assoc_table(Assoc_Old,N_Old),K,assoc_table(Assoc_New,N_New),V,Member) :- 
	change_tree(N_Limit),
	compare(Rel,N_Old,N_Limit),
	put_value_cmp(Rel,Assoc_Old,K,Assoc_New,V,Member),
	new_N(Member,N_Old,N_New).

put_value_cmp(<,Assoc_Old,K,Assoc_New,V,Member) :-
	put_value_list(Assoc_Old,K,Assoc_New,V,Member).

put_value_cmp(=,Assoc_Old,K,Assoc_New,V,Member) :-
	put_value_list(Assoc_Old,K,Assoc_Temp,V,Member),
	(
	    Member == no ->
	    one_key_list_to_table_avl(Assoc_Temp,nil,Assoc_New)
	;
	    Assoc_New = Assoc_Temp
	).

put_value_cmp(>,Assoc_Old,K,Assoc_New,V,Member) :-
	put_value_avl(Assoc_Old,K,Assoc_New,V,Member,_).


%put_value if assoc is a list
put_value_list([],K,[K-V],V,no).
put_value_list([K1-V1|Rest],K2,Result,V2,Member) :- 
	compare(Rel,K1,K2),
	put_value_list_cmp(Rel,[K1-V1|Rest],K2,Result,V2,Member).

put_value_list_cmp(<,[K1-V1|Rest1],K2,[K1-V1|Rest2],NewVal,Member) :- 
	put_value_list(Rest1,K2,Rest2,NewVal,Member).  
put_value_list_cmp(=,[K-OldVal|Rest],K,[K-NewVal|Rest],NewVal,yes(OldVal)).
put_value_list_cmp(>,[K1-V1|Rest],K2,[K2-NewVal,K1-V1|Rest],NewVal,no).

%put_value if assoc is a avl_tree
put_value_avl(nil,K,avl(K,V,nil,nil,0),V,no,yes).
put_value_avl(avl(K,V,L,R,H), Key, Result, Val, Member, Eq) :-
	compare(Rel, Key, K),
	put_value_avl_cmp(Rel, avl(K,V,L,R,H), Key, Result, Val, Member, Eq).

put_value_avl_cmp(<, avl(K,V,L,R,H), Key, Result, Val, Member, Eq1) :- 
	put_value_avl(L, Key, ABB, Val, Member, Eq2),
	New_H is H - 1,
	new_FE_5(Eq2,New_H,avl(K,V,ABB,R,H),Result,Eq1).
put_value_avl_cmp(=, avl(K,V,L,R,H), _, avl(K,Val,L,R,H), Val, yes(V),no).
put_value_avl_cmp(>, avl(K,V,L,R,H), Key, Result, Val, Member, Eq1) :- 
	put_value_avl(R, Key, ABB, Val, Member, Eq2),
	New_H is H + 1,
	new_FE_5(Eq2,New_H,avl(K,V,L,ABB,H),Result,Eq1).

%Figure out FE
new_FE_5(no,_,AVL,AVL,no).
new_FE_5(yes,Site,ABB,AVL,Eq) :- new_FE_4(Site,ABB,AVL,Eq).

new_FE_4(0,avl(K,V,L,R,_),avl(K,V,L,R,0),no).
new_FE_4(-1,avl(K,V,L,R,_),avl(K,V,L,R,-1),yes).
new_FE_4(1,avl(K,V,L,R,_),avl(K,V,L,R,1),yes).
new_FE_4(-2,avl(K,V,avl(LK,LV,LL,LR,LH),R,_),AVL,no) :- 
	(
	    LH == 1 ->
	    ROT = rdd
	;
	    ROT = rsd
	),
	rotation(ROT,avl(K,V,avl(LK,LV,LL,LR,LH),R,-2),AVL).
new_FE_4(2,avl(K,V,L,avl(RK,RV,RL,RR,RH),_),AVL,no) :- 
	(
	    RH == -1 ->
	    ROT = rdi
	;
	    ROT = rsi
	),
	rotation(ROT,avl(K,V,L,avl(RK,RV,RL,RR,RH),2),AVL).

rotation(rsi,avl(K,V,L,avl(RK,RV,RL,RR,_),_),avl(RK,RV,avl(K,V,L,RL,0),RR,0)).
rotation(rsd,avl(K,V,avl(LK,LV,LL,LR,_),R,_),avl(LK,LV,LL,avl(K,V,LR,R,0),0)).
rotation(rdi,avl(K,V,L,avl(RK,RV,avl(RLK,RLV,RLL,RLR,HRL),RR,_),_),
	 avl(RLK,RLV,avl(K,V,L,RLL,HL),avl(RK,RV,RLR,RR,HR),0)) :-
         height(HRL,HL,HR).
rotation(rdd,avl(K,V,avl(LK,LV,LL,avl(LRK,LRV,LRL,LRR,HLR),_),R,_),
	 avl(LRK,LRV,avl(LK,LV,LL,LRL,HL),avl(K,V,LRR,R,HR),0)) :-
         height(HLR,HL,HR).

height(0,0,0).
height(-1,0,1).
height(1,-1,0).

%Figure out number of elements
new_N(no,N_Old,N_New) :- N_New is N_Old + 1.
new_N(yes(_),N,N).


:- pred add_value(Assoc1,K,Assoc2,V) : assoc_table * key * assoc_table * value
   # "Similar to @pred{put_value/5} but for the case in which @var{Key} must 
   not appear in @var{Assoc1} (Member is known to be `no'). 
   Otherwise we throw an error.".

add_value(OldAssoc,K,NewAssoc,Val) :- 
        put_value(OldAssoc,K,NewAssoc,Val,no) ->
        true
; 
	error("add_value: key already appears in table").


:- pred update_value(Assoc1,K,Assoc2,V,OldVar) : assoc_table * key * assoc_table * value * value
   # "Similar to @pred{put_value/5} but for the case in which @var{Key} must 
   appear in @var{Assoc1} (Member is known to be yes(@var{OldVar}). 
   Otherwise we throw an error.".

update_value(OldAssoc,K,NewAssoc,Val,OldVal) :- 
        put_value(OldAssoc,K,NewAssoc,Val,yes(OldVal)) ->
	true
;     
	error("update_value: key does not appear in table").


:- pred map(Assoc1,Pred,Assoc2) 
   # "Applies @var{Pred} with arity 3 to each value of the assoc_table
   @var{Assoc1} obtaining the new assoc_table @var{Assoc2}
   in which only the values can have changed.".

:- meta_predicate map(?, pred(3), ?).

map(assoc_table(Assoc,N),Pred,assoc_table(Result,N)) :- 
	change_tree(N_Limit),
	(
	    N > N_Limit ->
	    map_avl(Assoc,Pred,Result)
	;
	    hiordlib:map(Assoc,(_(K-V,K-R) :- Pred(K,V,R)),Result)
	).

map_avl(nil,_,nil).
map_avl(avl(K,Old_Val,Old_L,Old_R,H),Pred,avl(K,New_Val,New_L,New_R,H)) :- 
	map_avl(Old_L,Pred,New_L),
	Pred(K,Old_Val,New_Val), 
	map_avl(Old_R,Pred,New_R).

:- pred foldl(Assoc,DS,Pred,NDS) 
   # "Applies @var{Pred} with arity 4 to each value of the assoc_table
   @var{Assoc}. If @var{Pred} is satisfied, it updates
   the data-structure DS. Otherwise it fails.".

:- meta_predicate foldl(?, ?, pred(4), ?).

foldl(assoc_table(Assoc,N),DS,Pred,NDS) :- 
	change_tree(N_Limit),
	(
	    N > N_Limit ->
	    foldl_avl(Assoc,DS,Pred,NDS)
	;
	    hiordlib:foldl(Assoc,DS,(_(K-V,DS0,NDS0) :- Pred(K,V,DS0,NDS0)),NDS)
	).

foldl_avl(nil,DS,_,DS).
foldl_avl(avl(K,V,L,R,_),DS,Pred,NDS) :- 
	foldl_avl(L,DS,Pred,DS0),
	Pred(K,V,DS0,DS1),
	foldl_avl(R,DS1,Pred,NDS).

:- regtype pairs(P) # "@var{P} is a ordered list of elements of the form
   @tt{key}-@tt{value}.".

:- pred one_key_list_to_table(L,Assoc) : pairs * assoc_table
   # "Transforms @var{L} into @var{Assoc} where each pair of @var{L} will be
   a association in @var{Assoc}.".

one_key_list_to_table(Pairs,assoc_table(Assoc,Long)) :- 
	change_tree(N_Limit),
	length(Pairs,Long),
	(
	    Long > N_Limit ->
	    one_key_list_to_table_avl(Pairs, nil, Assoc)
	;
	    Assoc = Pairs
	).

one_key_list_to_table_avl([], AVL, AVL).
one_key_list_to_table_avl([K-V|Pairs], AVL_Old, AVL_New) :-
	put_value_avl(AVL_Old, K, AVL_Temp, V, _, _),
	one_key_list_to_table_avl(Pairs, AVL_Temp, AVL_New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%                   CALCULO DE N_LIMIT                        %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%crear_lista(L,1,L).
%crear_lista(L1,N,L2) :- 
%	N > 1,
%	N2 is N - 1,
%	L_Temp = [N2-N2|L1],
%	crear_lista(L_Temp,N2,L2).

%acceder_lista(_,0) :- !.
%acceder_lista(List,N) :-
%	get_value_list(List,N,N),
%	N2 is N - 1,
%	acceder_lista(List,N2).
%
%acceder_AVL(_,0) :- !.
%acceder_AVL(AVL,N) :-
%	get_value_avl(AVL,N,N),
%	N2 is N - 1,
%	acceder_AVL(AVL,N2).

%insertar_nesimo_lista(N,List_Ori) :-
	%le insertamos el elemento n-esimo
%	put_value_list(List_Ori,N,List,N,_),
	%accedemos independientemente a los n elementos de la lista
%	acceder_lista(List,N).

%insertar_nesimo_avl(N,List_Ori) :-
	%ahora insertamos el n-esimo elemento pero trabajando con arboles
%	one_key_list_to_table_avl(List_Ori,nil,AVL_Ori),
	%le insertamos el elemento n-esimo
%	put_value_avl(AVL_Ori,N,AVL,N,_,_),
	%accedemos independientemente a los n elementos de la lista
%	acceder_AVL(AVL,N).

%recorrer_N(Tiempo,N,N_Fin,Repeat,Resul,Llevo) :- 
%	crear_lista([],N,Lista_Ori),
%	statistics(Tiempo,[_,_]),
	%construimos una lista de asociacion de n - 1 elementos
%	between(1,Repeat,N_Lista),
%	insertar_nesimo_lista(N,Lista_Ori),
%	(
%	    N_Lista < Repeat ->
%	    fail
%	;
%	    statistics(Tiempo,[_,T_Lista])
%	),
%	between(1,Repeat,N_Avl),
%	insertar_nesimo_avl(N,Lista_Ori),
%	(
%	    N_Avl < Repeat ->
%	    fail
%	;
%	    statistics(Tiempo,[_,T_Avl])
%	),
%	Llevo2 = [N:T_Lista-T_Avl|Llevo],
%	(
%	    N == N_Fin ->
%	    Resul = Llevo2
%	;
%	    N1 is N + 1,
%	    recorrer_N(Tiempo,N1,N_Fin,Repeat,Resul,Llevo2)
%	).	
	
%tabla_tiempos(Tiempo,N_Ini,N_Fin,Repeat,Resul) :-
%	recorrer_N(Tiempo,N_Ini,N_Fin,Repeat,Resul,[]).
