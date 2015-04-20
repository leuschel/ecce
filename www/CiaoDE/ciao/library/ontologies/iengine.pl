/*get_slots,get_class_instances,get_class_subclasses,get_class_superclasses,get_instance_types,get_slot_domain,get_slot_facets,get_slot_type,get_slot_value,get_slot_values,get_slot_values_in_detail,individual_p,instance_of_p,get_frame_details,member_slot_value_p,member_facet_value_p,get_facet_value,get_facet_values*/
/* Implementacion de las primitivas OKBC en Prolog de Edimburgo */
/* Version: 0.5 */
/* Author: Juan Pablo Perez Aldea */
/* Date: 12/3/2001 */
/* Comments: Faltan todavia unas tres operaciones.*/




/****************************************************************/
/* Primitivas OKBC							    */
/****************************************************************/

get_slots(Frame, L) :- 	find_slots(Frame, [], L). 

get_class_instances(C, L) :-	class(C),   
				find_instances(C, [], L). 

get_class_subclasses(C, direct, L) :- 	class(C),  
					find_subclasses(C, [], L).  


get_class_subclasses(C, taxonomic, L) :- 	class(C), 
						get_class_subclasses(C, direct, X), 
						get_list_subclasses(X, X, L), 
						!.  

get_class_superclasses(C, direct, L) :-		class(C), 
						find_superclasses(C, [], L).  			

get_class_superclasses(C, taxonomic, L) :-	class(C), 
						get_class_superclasses(C, direct, X), 
						get_list_superclasses(X, X, L), 
						!.  


get_instance_types(Instance, L) :- find_instance_types(Instance, [], L). 

get_slot_domain(Slot,L) :-	find_slot_domains(Slot, [], S), 
					recorre2(S, S, L). 

get_slot_facets(Slot, L) :- 	find_facets(Slot, [], L). 

get_slot_type(Frame, Slot, own):- own_slot_of(Slot, Frame). 

get_slot_type(Frame, Slot, template):- template_slot_of(Slot, Frame). 

get_slot_value(Frame, Slot, X) :-	slot_of(Slot, Frame), 
					find_values(valor, Slot, [], [X|Xs]), 
					unique([X|Xs]). 
					 
get_slot_value(Frame, Slot, cardinality_violation) :-	slot_of(Slot, Frame), 
					find_values(valor, Slot, [], L), 
					not(unique(L)),  
					not(empty(L)). 

get_slot_values(Frame, Slot, L) :-	slot_of(Slot, Frame), 
					find_values(valor, Slot, [], L). 

get_slot_values_in_detail(Frame, Slot, L) :-	get_facet_values(Frame, Slot, valor, L1), 
						convierte_d(L1, [], L2),
						find_values_of_slot(Slot, L2, L3),  
						convierte_nd(L3, [], L). 

individual_p(X) :- 	not(class(X)). 

instance_of_p(X, C) :- 	instance_of(X, C). 

get_frame_details(Frame, L) :- 	class(Frame), 
				get_class_superclasses(Frame, Sup, taxonomic), 
				get_class_subclasses(Frame, Sub, taxonomic), 
				get_slots(Frame, Sl), 
				introduce(Sl, [], X),  
				introduce(Sub, X, Y),  
				introduce(Sup, Y, Z), 
				introduce([clase], Z, L).  

get_frame_details(Frame, L) :- 	instance_of(Frame, Y), 
				get_slots(Frame, Sl), 
				introduce(Sl, [], X),  
				introduce([instancia], X, L).  

member_slot_value_p(Frame, Slot, Value) :-	slot_of(Slot, Frame), 
						find_values(valor, Slot, [], L), 
						member(Value, L).  	



member_facet_value_p(Frame, Slot, Facet, Value) :-	slot_of(Slot, Frame), 
							find_values(Facet, Slot, [], L), 
							member(Value, L).  

get_facet_value(Frame, Slot, Facet, X) :-	slot_of(Slot, Frame), 
						find_values(Facet, Slot, [], [X|Xs]), 
						unique([X|Xs]). 
					 
get_facet_value(Frame, Slot, Facet, cardinality_violation) :-	slot_of(Slot, Frame), 
								find_values(Facet, Slot, [], L), 
								not(unique(L)),  
								not(empty(L)). 

get_facet_values(Frame, Slot, Facet, L) :- 	slot_of(Slot, Frame), 
						find_values(Facet, Slot, [], L).  



/******************************************************************/
/* 		Predicados 'find_'				  */
/******************************************************************/


find_instances(C, I, L) :- 	instance_of(X, C),   
				not(member(X, I)),  
				find_instances(C, [X|I], L), 
				!.  
find_instances(_, I, I).        

find_instance_types(C, I, L) :- type_of(X, C),   
				not(member(X, I)),  
				find_instances(C, [X|I], L), 
				!.  
find_instance_types(_, I, I).        


find_subclasses(C, I, L) :- 	subclass_of(X, C),   
				not(member(X, I)),  
				find_subclasses(C, [X|I], L), 
				!.  
find_subclasses(_, I, I).        

find_superclasses(C, I, L) :- 	superclass_of(X, C),   
				not(member(X, I)),  
				find_superclasses(C, [X|I], L), 
				!.  
find_superclasses(_, I, I).        


find_values(F, S, I, L) :- 	value_of_facet_of(X, F, S),   
				not(member(X, I)),  
				find_values(F, S, [X|I], L), 
				!.  
find_values(_, _, I, I).        

find_facets(S, I, L) :-	facet_of(X, S), 
			not(member(X, I)), 
			find_facets(S, [X|I], L), 
			!. 
find_facets(_, I, I). 
			
find_slots(F, I, L) :-  slot_of(X, F),   
			not(member(X, I)),  
			find_slots(F, [X|I], L), 
			!.  
find_slots(_, I, I). 

find_slot_domains(S, I, L) :- 	slot_of(S, X),   
				not(member(X, I)),  
				find_slot_domains(S, [X|I], L), 
				!.  
find_slot_domains(_, I, I).        

find_values_of_slot(S, I, L) :- 	value_of_facet_of(X, valor, S),   
					not(member(X, I)),  
					find_subclasses(S, [X|I], L), 
					!.  
find_values_of_slot(_, I, I). 

/******************************************************************/
/* 		Predicados 'get_list'				  */
/******************************************************************/	


get_list_subclasses([], Y, Y). 

 	
get_list_subclasses([X|Xs], Lista_Inicial, L) :-	get_class_subclasses(X, taxonomic, Y), 
							aplana(Y, Lista_Inicial, Z), 
							get_list_subclasses(Xs, Z, L). 

get_list_superclasses([], Y, Y). 

 	
get_list_superclasses([X|Xs], Lista_Inicial, L) :-	get_class_superclasses(X, taxonomic, Y), 
							aplana(Y, Lista_Inicial, Z), 
							get_list_superclasses(Xs, Z, L). 



/********************************************************************/


/*  OSCAR:::    slot_of(X,Y) :- own_slot_of(X,Y).         */
/* 		slot_of(X,Y) :- template_slot_of(X,Y).    */

slot_of(X,Y) :- own_slot_of(X,Y).          
slot_of(X,Y) :- template_slot_of(X,Y).      


/******************************************************************/
/* 		Predicados auxiliares				  */
/******************************************************************/

not(Goal) :- call(Goal), !, fail. 
not(_).  

member(X, [X|_]).   
member(X, [_|Ys]) :- member(X, Ys). 	

member_super(X, L) :- 	get_class_superclasses(X, taxonomic, S), 
			recorre(S, L). 

recorre([X|_], L) :-	member(X,L).
recorre([_|Xs], L) :-	recorre(Xs, L).

recorre2([], S, S).
recorre2([X|Xs], S, L) :- 	not(member_super(X, S)), 
				recorre2(Xs, S, L).
recorre2([X|Xs], S, L) :- 	member_super(X, S), 
				quitar(X, S, R), 
				recorre2(Xs, R, L).

quitar(X, L, R) :- quitar(X, L, [], R). 
quitar(X, [X|Xs], L, R) :-	reverse(L, L2), 
				append(L2, Xs, R).  
quitar(X, [Y|Ys], L, R) :- quitar(X, Ys, [Y|L], R).

aplana([], Ys, Ys). 
aplana([X|Xs], Ys, [X|Zs]) :- aplana(Xs, Ys, Zs).

introduce(X, L, [X|L]). 

convierte_d([], L, L).
convierte_nd([], L, L).
/*convierte_d([X|Xs], Ys, L) :- convierte_d(Xs, [tupla(X,true,true)|Ys], L).*/ 

append([], Ys, Ys) :-  list(Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).  

list([]). 
list([_|Y]) :- list(Y). 

reverse(Xs, Ys) :- reverse(Xs, [], Ys). 
reverse([], Ys, Ys). 
reverse([X|Xs], Acc, Ys) :- reverse(Xs, [X|Acc], Ys). 

unique([_]).

empty([]). 