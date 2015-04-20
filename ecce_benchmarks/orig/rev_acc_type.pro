/* file: rev_acc_type.pro */

rev([],_A,_A).
rev([_H|_T],_Acc,_Res) :-
	is_a_list(_Acc),
	rev(_T,[_H|_Acc],_Res).


is_a_list([]).
is_a_list([_H|_T]) :- is_a_list(_T).
