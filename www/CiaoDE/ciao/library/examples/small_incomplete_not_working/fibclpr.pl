fib(A, B) :-
        solve_generic_1(lt, 1, A, -1),
        wrap_term(C, A-1),
        fib(C, D),
        wrap_term(E, A-2),
        fib(E, F),
        solve_generic_3(eq, 0, F, -1, B, 1, D, -1).
fib(0, 0).
fib(1, 1).
