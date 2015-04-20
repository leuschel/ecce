
% This has to be in user.pl (PBC)
% goal_expansion(write_term(T,L), c_itf, user:ciao_write_term(T,L)).

ciao_write_term(T, L) :- sicstus_wt_ops(L, S, []), write_term(T, S).

sicstus_wt_ops([]) --> [].
sicstus_wt_ops([O|Os]) -->
        sicstus_wt_op(O),
        sicstus_wt_ops(Os).

sicstus_wt_op(priority(_)) --> !, [].
sicstus_wt_op(ignore_ops(ops)) --> !, [ignore_ops(true)].
sicstus_wt_op(Op) --> [Op].

%% Output predicates are built-in in SICStus3
