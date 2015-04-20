/* -------- */
/* HASH.PRO */
/* -------- */

ecce_type(hashkey,term(0,[])).
ecce_type(hashkey,term(1,[])).
ecce_type(hashkey,term(2,[])).
ecce_type(hashkey,term(3,[])).
ecce_type(hashkey,term(4,[])).
ecce_type(hashkey,term(5,[])).
ecce_type(hashkey,term(6,[])).
ecce_type(hashkey,term(7,[])).
ecce_type(hashkey,term(8,[])).
ecce_type(hashkey,term(9,[])).

ecce_type(hashtable,term(hash,[any,any,any,any,any,
			  any,any,any,any,any ])).


get_hash_key(Int,Hash) :-
	Hash is Int mod 10.

create_hash_table(StdEl,
	hash(StdEl,StdEl,StdEl,StdEl,StdEl,StdEl,StdEl,StdEl,StdEl,StdEl)).

get_hashed_element(0,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T0).
get_hashed_element(1,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T1).
get_hashed_element(2,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T2).
get_hashed_element(3,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T3).
get_hashed_element(4,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T4).
get_hashed_element(5,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T5).
get_hashed_element(6,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T6).
get_hashed_element(7,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T7).
get_hashed_element(8,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T8).
get_hashed_element(9,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),T9).


set_hashed_element(0,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(NewEl,T1,T2,T3,T4,T5,T6,T7,T8,T9)).
set_hashed_element(1,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,NewEl,T2,T3,T4,T5,T6,T7,T8,T9)).
set_hashed_element(2,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,T1,NewEl,T3,T4,T5,T6,T7,T8,T9)).
set_hashed_element(3,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,T1,T2,NewEl,T4,T5,T6,T7,T8,T9)).
set_hashed_element(4,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,T1,T2,T3,NewEl,T5,T6,T7,T8,T9)).
set_hashed_element(5,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,T1,T2,T3,T4,NewEl,T6,T7,T8,T9)).
set_hashed_element(6,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,T1,T2,T3,T4,T5,NewEl,T7,T8,T9)).
set_hashed_element(7,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,T1,T2,T3,T4,T5,T6,NewEl,T8,T9)).
set_hashed_element(8,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,T1,T2,T3,T4,T5,T6,T7,NewEl,T9)).
set_hashed_element(9,NewEl,hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9),
	         hash(T0,T1,T2,T3,T4,T5,T6,T7,T8,NewEl)).
