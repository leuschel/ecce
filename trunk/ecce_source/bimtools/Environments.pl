
/* ---------------- */
/* Environments.pro */
/* ---------------- */

/* Standard Environment Manipulation tools */

/* ===================================================== */

:- include( '../multi_meta' ).

/* ===================================================== */



ecce_type(st_lup_binding,term('/',[st_lup_key,st_lup_val])).
ecce_type(st_lup_key,nonvar).
ecce_type(st_lup_val,nonvar).
ecce_type(st_lup_env,list(st_lup_binding)).



pre_condition(store(Env,Key,_Value,_NewEnv)) :-
        term_is_of_type(Env,st_lup_env),
        term_is_of_type(Key,st_lup_key).
post_condition(store(_Env,_Key,_Value,NewEnv)) :-
        term_is_of_type(NewEnv,st_lup_env).

store([],Key,Value,[Key/Value]).
store([Key/Value2|T],Key,Value,[Key/Value|T]).
store([Key2/Value2|T],Key,Value,[Key2/Value2|BT]) :-
   Key \== Key2,
   store(T,Key,Value,BT).


   
pre_condition(lookup(Key,Env,_Value)) :-
        term_is_of_type(Env,st_lup_env),
        term_is_of_type(Key,st_lup_key).
post_condition(lookup(_Key,_Env,_Value)).
        
lookup(Key,[Key/Value|T],Value).
lookup(Key,[Key2/Value2|T],Value) :-
   Key \== Key2,
   lookup(Key,T,Value).
