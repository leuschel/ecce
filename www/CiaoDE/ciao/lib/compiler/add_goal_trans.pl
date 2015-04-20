:- meta_predicate add_goal_trans(+, spec).

add_goal_trans(M, S) :-
        term_to_meta(T/A, S),
        atom(T),
        functor(Tr, T, A),
        ( A = 3 -> arg(3, Tr, M) ; true),
        assertz_fact(goal_trans(M,Tr)).

del_goal_trans(M) :-
        retractall_fact(goal_trans(M,_)).
