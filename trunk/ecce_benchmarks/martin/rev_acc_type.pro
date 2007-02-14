/* file: rev_acc_type.pro */

revacc:rev([],_A,_A).
revacc:rev([_H|_T],_Acc,_Res) :-
	revacc:is_list(_Acc),
	revacc:rev(_T,[_H|_Acc],_Res).


revacc:is_list([]).
revacc:is_list([_H|_T]) :- revacc:is_list(_T).
