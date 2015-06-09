go([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1, E1, F1]) :-
	sum_32_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1, E1, F1).
	
'$MEMOCALLS'(X).
sum_32_8(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1, E1) :-
	sum_31_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1, E1).
sum_32_8(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_31_8(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1) :-
	sum_30_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1).
sum_31_8(0, 0, 0, 0, 0, A, B, C, D, E, 0, 0, 0, F, 0, 0, 0, G, H, I, J, K, 0, 0, 0, L, M, N, 0, O, 0) :-
	sum_26_8(A, B, C, D, E, 0, 0, 0, F, 0, 0, 0, G, H, I, J, K, 0, 0, 0, L, M, N, 0, O, 0).
sum_31_7(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1) :-
	sum_30_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1).
sum_31_7(0, 0, 0, 0, 0, A, B, C, D, E, 0, 0, 0, F, 0, 0, 0, G, H, I, J, K, 0, 0, 0, L, M, N, 0, O, 0) :-
	sum_26_7(A, B, C, D, E, 0, 0, 0, F, 0, 0, 0, G, H, I, J, K, 0, 0, 0, L, M, N, 0, O, 0).
sum_30_8(1, A, B, C, 1, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1) :-
	sum_29_7(A, B, C, 1, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1).
sum_30_8(0, 0, 0, 0, A, B, C, D, E, F, G, H, I, J, 0, 0, K, L, M, N, O, P, 0, 0, Q, R, S, T, U, V) :-
	sum_26_8(A, B, C, D, E, F, G, H, I, J, 0, 0, K, L, M, N, O, P, 0, 0, Q, R, S, T, U, V).
sum_30_7(1, A, B, C, 1, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1) :-
	sum_29_6(A, B, C, 1, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1).
sum_30_7(0, 0, 0, 0, A, B, C, D, E, F, G, H, I, J, 0, 0, K, L, M, N, O, P, 0, 0, Q, R, S, T, U, V) :-
	sum_26_7(A, B, C, D, E, F, G, H, I, J, 0, 0, K, L, M, N, O, P, 0, 0, Q, R, S, T, U, V).
sum_30_6(1, A, B, C, 1, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1) :-
	sum_29_5(A, B, C, 1, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1).
sum_30_6(0, 0, 0, 0, A, B, C, D, E, F, G, H, I, J, 0, 0, K, L, M, N, O, P, 0, 0, Q, R, S, T, U, V) :-
	sum_26_6(A, B, C, D, E, F, G, H, I, J, 0, 0, K, L, M, N, O, P, 0, 0, Q, R, S, T, U, V).
sum_29_8(1, A, B, 1, 1, C, D, 1, 1, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X) :-
	sum_28_7(A, B, 1, 1, C, D, 1, 1, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X).
sum_29_8(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 0, T, U, V, W, X, Y) :-
	sum_26_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 0, T, U, V, W, X, Y).
sum_29_7(1, A, B, 1, 1, C, D, 1, 1, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X) :-
	sum_28_6(A, B, 1, 1, C, D, 1, 1, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X).
sum_29_7(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 0, T, U, V, W, X, Y) :-
	sum_26_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 0, T, U, V, W, X, Y).
sum_29_6(1, A, B, 1, 1, C, D, 1, 1, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X) :-
	sum_28_5(A, B, 1, 1, C, D, 1, 1, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X).
sum_29_6(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 0, T, U, V, W, X, Y) :-
	sum_26_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 0, T, U, V, W, X, Y).
sum_29_5(1, A, B, 1, 1, C, D, 1, 1, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X) :-
	sum_28_4(A, B, 1, 1, C, D, 1, 1, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X).
sum_29_5(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 0, T, U, V, W, X, Y) :-
	sum_26_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 0, T, U, V, W, X, Y).
sum_28_8(1, A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_27_7(A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_28_8(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_28_7(1, A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_27_6(A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_28_7(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_28_6(1, A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_27_5(A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_28_6(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_28_5(1, A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_27_4(A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_28_5(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_28_4(1, A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_27_3(A, 1, 1, 1, B, 1, 1, 1, C, 1, 1, 1, D, 1, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_28_4(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_27_8(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) :-
	fail.
sum_27_8(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_27_7(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) :-
	fail.
sum_27_7(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_27_6(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) :-
	fail.
sum_27_6(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_27_5(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) :-
	fail.
sum_27_5(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_27_4(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) :-
	fail.
sum_27_4(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_27_3(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) :-
	fail.
sum_27_3(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) :-
	sum_26_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z).
sum_26_8(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) :-
	sum_25_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y).
sum_26_8(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N) :-
	sum_22_8(A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N).
sum_26_7(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) :-
	sum_25_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y).
sum_26_7(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N) :-
	sum_22_7(A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N).
sum_26_6(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) :-
	sum_25_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y).
sum_26_6(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N) :-
	sum_22_6(A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N).
sum_26_5(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) :-
	sum_25_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y).
sum_26_5(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N) :-
	sum_22_5(A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N).
sum_26_4(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) :-
	sum_25_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y).
sum_26_4(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N) :-
	sum_22_4(A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N).
sum_26_3(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) :-
	sum_25_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y).
sum_26_3(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N) :-
	sum_22_3(A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N).
sum_26_2(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) :-
	sum_25_1(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y).
sum_26_2(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N) :-
	sum_22_2(A, B, C, D, E, F, 0, 0, 0, 0, G, H, I, J, 0, 0, 0, 0, K, L, M, N).
sum_25_8(1, A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) :-
	sum_24_7(A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W).
sum_25_8(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T) :-
	sum_22_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T).
sum_25_7(1, A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) :-
	sum_24_6(A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W).
sum_25_7(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T) :-
	sum_22_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T).
sum_25_6(1, A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) :-
	sum_24_5(A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W).
sum_25_6(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T) :-
	sum_22_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T).
sum_25_5(1, A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) :-
	sum_24_4(A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W).
sum_25_5(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T) :-
	sum_22_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T).
sum_25_4(1, A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) :-
	sum_24_3(A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W).
sum_25_4(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T) :-
	sum_22_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T).
sum_25_3(1, A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) :-
	sum_24_2(A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W).
sum_25_3(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T) :-
	sum_22_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T).
sum_25_2(1, A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) :-
	sum_24_1(A, B, 1, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W).
sum_25_2(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T) :-
	sum_22_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 0, P, 0, Q, R, S, T).
sum_25_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_24_8(1, A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S) :-
	sum_23_7(A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S).
sum_24_8(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_24_7(1, A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S) :-
	sum_23_6(A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S).
sum_24_7(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_24_6(1, A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S) :-
	sum_23_5(A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S).
sum_24_6(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_24_5(1, A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S) :-
	sum_23_4(A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S).
sum_24_5(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_24_4(1, A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S) :-
	sum_23_3(A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S).
sum_24_4(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_24_3(1, A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S) :-
	sum_23_2(A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S).
sum_24_3(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_24_2(1, A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S) :-
	sum_23_1(A, 1, B, C, D, 1, E, F, G, 1, H, 1, I, J, K, L, M, N, O, P, Q, R, S).
sum_24_2(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_24_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_23_8(1, 1, A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K) :-
	sum_21_6(A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K).
sum_23_8(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_23_7(1, 1, A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K) :-
	sum_21_5(A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K).
sum_23_7(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_23_6(1, 1, A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K) :-
	sum_21_4(A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K).
sum_23_6(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_23_5(1, 1, A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K) :-
	sum_21_3(A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K).
sum_23_5(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_23_4(1, 1, A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K) :-
	sum_21_2(A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K).
sum_23_4(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_23_3(1, 1, A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K) :-
	sum_21_1(A, B, C, 1, D, E, F, 1, 1, 1, 1, 1, G, H, I, 1, 1, 1, J, 1, K).
sum_23_3(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_23_2(1, 1, _, _, _, 1, _, _, _, 1, 1, 1, 1, 1, _, _, _, 1, 1, 1, _, 1, _) :-
	fail.
sum_23_2(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) :-
	sum_22_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V).
sum_23_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_22_8(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) :-
	sum_21_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U).
sum_22_8(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L) :-
	sum_18_8(A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L).
sum_22_7(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) :-
	sum_21_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U).
sum_22_7(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L) :-
	sum_18_7(A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L).
sum_22_6(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) :-
	sum_21_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U).
sum_22_6(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L) :-
	sum_18_6(A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L).
sum_22_5(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) :-
	sum_21_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U).
sum_22_5(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L) :-
	sum_18_5(A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L).
sum_22_4(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) :-
	sum_21_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U).
sum_22_4(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L) :-
	sum_18_4(A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L).
sum_22_3(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) :-
	sum_21_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U).
sum_22_3(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L) :-
	sum_18_3(A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L).
sum_22_2(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) :-
	sum_21_1(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U).
sum_22_2(0, 0, 0, 0, A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L) :-
	sum_18_2(A, B, C, D, E, F, 0, 0, G, H, I, 0, J, 0, 0, 0, K, L).
sum_22_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_21_8(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) :-
	sum_20_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T).
sum_21_8(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P) :-
	sum_18_8(A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P).
sum_21_7(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) :-
	sum_20_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T).
sum_21_7(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P) :-
	sum_18_7(A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P).
sum_21_6(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) :-
	sum_20_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T).
sum_21_6(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P) :-
	sum_18_6(A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P).
sum_21_5(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) :-
	sum_20_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T).
sum_21_5(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P) :-
	sum_18_5(A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P).
sum_21_4(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) :-
	sum_20_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T).
sum_21_4(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P) :-
	sum_18_4(A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P).
sum_21_3(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) :-
	sum_20_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T).
sum_21_3(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P) :-
	sum_18_3(A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P).
sum_21_2(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) :-
	sum_20_1(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T).
sum_21_2(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P) :-
	sum_18_2(A, B, C, D, E, F, G, H, I, J, K, 0, L, M, N, 0, O, P).
sum_21_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_20_8(1, A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_19_7(A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_20_8(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_20_7(1, A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_19_6(A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_20_7(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_20_6(1, A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_19_5(A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_20_6(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_20_5(1, A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_19_4(A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_20_5(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_20_4(1, A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_19_3(A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_20_4(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_20_3(1, A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_19_2(A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_20_3(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_20_2(1, A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_19_1(A, 1, 1, B, C, D, E, 1, F, G, H, I, J, K, L, M, N, O, P).
sum_20_2(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_20_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_19_8(1, 1, 1, A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1) :-
	sum_16_5(A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1).
sum_19_8(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_19_7(1, 1, 1, A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1) :-
	sum_16_4(A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1).
sum_19_7(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_19_6(1, 1, 1, A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1) :-
	sum_16_3(A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1).
sum_19_6(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_19_5(1, 1, 1, A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1) :-
	sum_16_2(A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1).
sum_19_5(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_19_4(1, 1, 1, A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1) :-
	sum_16_1(A, B, C, D, 1, 1, 1, 1, E, F, G, H, 1, 1, 1, 1).
sum_19_4(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_19_3(1, 1, 1, _, _, _, _, 1, 1, 1, 1, _, _, _, _, 1, 1, 1, 1) :-
	fail.
sum_19_3(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_19_2(1, 1, 1, _, _, _, _, 1, 1, 1, 1, _, _, _, _, 1, 1, 1, 1) :-
	fail.
sum_19_2(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) :-
	sum_18_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).
sum_19_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_18_8(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) :-
	sum_17_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q).
sum_18_8(0, 0, 0, 0, 0, 0, 0, 0, A, B, C, D, E, F, G, H, 0, 0) :-
	sum_10_8(A, B, C, D, E, F, G, H, 0, 0).
sum_18_7(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) :-
	sum_17_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q).
sum_18_7(0, 0, 0, 0, 0, 0, 0, 0, A, B, C, D, E, F, G, H, 0, 0) :-
	sum_10_7(A, B, C, D, E, F, G, H, 0, 0).
sum_18_6(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) :-
	sum_17_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q).
sum_18_6(0, 0, 0, 0, 0, 0, 0, 0, A, B, C, D, E, F, G, H, 0, 0) :-
	sum_10_6(A, B, C, D, E, F, G, H, 0, 0).
sum_18_5(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) :-
	sum_17_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q).
sum_18_5(0, 0, 0, 0, 0, 0, 0, 0, A, B, C, D, E, F, G, H, 0, 0) :-
	sum_10_5(A, B, C, D, E, F, G, H, 0, 0).
sum_18_4(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) :-
	sum_17_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q).
sum_18_4(0, 0, 0, 0, 0, 0, 0, 0, A, B, C, D, E, F, G, H, 0, 0) :-
	sum_10_4(A, B, C, D, E, F, G, H, 0, 0).
sum_18_3(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) :-
	sum_17_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q).
sum_18_3(0, 0, 0, 0, 0, 0, 0, 0, A, B, C, D, E, F, G, H, 0, 0) :-
	sum_10_3(A, B, C, D, E, F, G, H, 0, 0).
sum_18_2(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) :-
	sum_17_1(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q).
sum_18_2(0, 0, 0, 0, 0, 0, 0, 0, A, B, C, D, E, F, G, H, 0, 0) :-
	sum_10_2(A, B, C, D, E, F, G, H, 0, 0).
sum_18_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_17_8(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_16_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).
sum_17_8(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, 0) :-
	sum_14_8(A, B, C, D, E, F, G, H, I, J, K, L, M, 0).
sum_17_7(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_16_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).
sum_17_7(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, 0) :-
	sum_14_7(A, B, C, D, E, F, G, H, I, J, K, L, M, 0).
sum_17_6(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_16_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).
sum_17_6(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, 0) :-
	sum_14_6(A, B, C, D, E, F, G, H, I, J, K, L, M, 0).
sum_17_5(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_16_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).
sum_17_5(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, 0) :-
	sum_14_5(A, B, C, D, E, F, G, H, I, J, K, L, M, 0).
sum_17_4(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_16_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).
sum_17_4(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, 0) :-
	sum_14_4(A, B, C, D, E, F, G, H, I, J, K, L, M, 0).
sum_17_3(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_16_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).
sum_17_3(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, 0) :-
	sum_14_3(A, B, C, D, E, F, G, H, I, J, K, L, M, 0).
sum_17_2(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) :-
	sum_16_1(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).
sum_17_2(0, 0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, 0) :-
	sum_14_2(A, B, C, D, E, F, G, H, I, J, K, L, M, 0).
sum_17_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_16_8(1, A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_15_7(A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_8(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_7(1, A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_15_6(A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_7(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_6(1, A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_15_5(A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_6(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_5(1, A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_15_4(A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_5(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_4(1, A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_15_3(A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_4(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_3(1, A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_15_2(A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_3(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_2(1, A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_15_1(A, 1, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_2(0, 0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_16_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_15_8(1, 1, 1, A, B, 1, 1, 1, C, 1, D, E, F, 1, 1) :-
	sum_12_5(A, B, 1, 1, 1, C, 1, D, E, F, 1, 1).
sum_15_8(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_8(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_15_7(1, 1, 1, A, B, 1, 1, 1, C, 1, D, E, F, 1, 1) :-
	sum_12_4(A, B, 1, 1, 1, C, 1, D, E, F, 1, 1).
sum_15_7(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_7(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_15_6(1, 1, 1, A, B, 1, 1, 1, C, 1, D, E, F, 1, 1) :-
	sum_12_3(A, B, 1, 1, 1, C, 1, D, E, F, 1, 1).
sum_15_6(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_6(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_15_5(1, 1, 1, A, B, 1, 1, 1, C, 1, D, E, F, 1, 1) :-
	sum_12_2(A, B, 1, 1, 1, C, 1, D, E, F, 1, 1).
sum_15_5(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_5(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_15_4(1, 1, 1, A, B, 1, 1, 1, C, 1, D, E, F, 1, 1) :-
	sum_12_1(A, B, 1, 1, 1, C, 1, D, E, F, 1, 1).
sum_15_4(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_4(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_15_3(1, 1, 1, _, _, 1, 1, 1, _, 1, _, _, _, 1, 1) :-
	fail.
sum_15_3(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_3(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_15_2(1, 1, 1, _, _, 1, 1, 1, _, 1, _, _, _, 1, 1) :-
	fail.
sum_15_2(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N) :-
	sum_14_2(A, B, C, D, E, F, G, H, I, J, K, L, M, N).
sum_15_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_14_8(1, A, B, C, D, E, F, G, H, I, J, K, L, M) :-
	sum_13_7(A, B, C, D, E, F, G, H, I, J, K, L, M).
sum_14_8(0, 0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_8(A, B, C, D, E, F, G, H, I, J, K, L).
sum_14_7(1, A, B, C, D, E, F, G, H, I, J, K, L, M) :-
	sum_13_6(A, B, C, D, E, F, G, H, I, J, K, L, M).
sum_14_7(0, 0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_7(A, B, C, D, E, F, G, H, I, J, K, L).
sum_14_6(1, A, B, C, D, E, F, G, H, I, J, K, L, M) :-
	sum_13_5(A, B, C, D, E, F, G, H, I, J, K, L, M).
sum_14_6(0, 0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_6(A, B, C, D, E, F, G, H, I, J, K, L).
sum_14_5(1, A, B, C, D, E, F, G, H, I, J, K, L, M) :-
	sum_13_4(A, B, C, D, E, F, G, H, I, J, K, L, M).
sum_14_5(0, 0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_5(A, B, C, D, E, F, G, H, I, J, K, L).
sum_14_4(1, A, B, C, D, E, F, G, H, I, J, K, L, M) :-
	sum_13_3(A, B, C, D, E, F, G, H, I, J, K, L, M).
sum_14_4(0, 0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_4(A, B, C, D, E, F, G, H, I, J, K, L).
sum_14_3(1, A, B, C, D, E, F, G, H, I, J, K, L, M) :-
	sum_13_2(A, B, C, D, E, F, G, H, I, J, K, L, M).
sum_14_3(0, 0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_3(A, B, C, D, E, F, G, H, I, J, K, L).
sum_14_2(1, A, B, C, D, E, F, G, H, I, J, K, L, M) :-
	sum_13_1(A, B, C, D, E, F, G, H, I, J, K, L, M).
sum_14_2(0, 0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_2(A, B, C, D, E, F, G, H, I, J, K, L).
sum_14_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_13_8(1, A, B, 1, C, D, E, 1, F, G, H, 1, I) :-
	sum_12_7(A, B, 1, C, D, E, 1, F, G, H, 1, I).
sum_13_8(0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_8(A, B, C, D, E, F, G, H, I, J, K, L).
sum_13_7(1, A, B, 1, C, D, E, 1, F, G, H, 1, I) :-
	sum_12_6(A, B, 1, C, D, E, 1, F, G, H, 1, I).
sum_13_7(0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_7(A, B, C, D, E, F, G, H, I, J, K, L).
sum_13_6(1, A, B, 1, C, D, E, 1, F, G, H, 1, I) :-
	sum_12_5(A, B, 1, C, D, E, 1, F, G, H, 1, I).
sum_13_6(0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_6(A, B, C, D, E, F, G, H, I, J, K, L).
sum_13_5(1, A, B, 1, C, D, E, 1, F, G, H, 1, I) :-
	sum_12_4(A, B, 1, C, D, E, 1, F, G, H, 1, I).
sum_13_5(0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_5(A, B, C, D, E, F, G, H, I, J, K, L).
sum_13_4(1, A, B, 1, C, D, E, 1, F, G, H, 1, I) :-
	sum_12_3(A, B, 1, C, D, E, 1, F, G, H, 1, I).
sum_13_4(0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_4(A, B, C, D, E, F, G, H, I, J, K, L).
sum_13_3(1, A, B, 1, C, D, E, 1, F, G, H, 1, I) :-
	sum_12_2(A, B, 1, C, D, E, 1, F, G, H, 1, I).
sum_13_3(0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_3(A, B, C, D, E, F, G, H, I, J, K, L).
sum_13_2(1, A, B, 1, C, D, E, 1, F, G, H, 1, I) :-
	sum_12_1(A, B, 1, C, D, E, 1, F, G, H, 1, I).
sum_13_2(0, A, B, C, D, E, F, G, H, I, J, K, L) :-
	sum_12_2(A, B, C, D, E, F, G, H, I, J, K, L).
sum_13_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_12_8(1, A, B, C, D, E, F, G, H, I, J, K) :-
	sum_11_7(A, B, C, D, E, F, G, H, I, J, K).
sum_12_8(0, 0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_8(A, B, C, D, E, F, G, H, I, J).
sum_12_7(1, A, B, C, D, E, F, G, H, I, J, K) :-
	sum_11_6(A, B, C, D, E, F, G, H, I, J, K).
sum_12_7(0, 0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_7(A, B, C, D, E, F, G, H, I, J).
sum_12_6(1, A, B, C, D, E, F, G, H, I, J, K) :-
	sum_11_5(A, B, C, D, E, F, G, H, I, J, K).
sum_12_6(0, 0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_6(A, B, C, D, E, F, G, H, I, J).
sum_12_5(1, A, B, C, D, E, F, G, H, I, J, K) :-
	sum_11_4(A, B, C, D, E, F, G, H, I, J, K).
sum_12_5(0, 0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_5(A, B, C, D, E, F, G, H, I, J).
sum_12_4(1, A, B, C, D, E, F, G, H, I, J, K) :-
	sum_11_3(A, B, C, D, E, F, G, H, I, J, K).
sum_12_4(0, 0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_4(A, B, C, D, E, F, G, H, I, J).
sum_12_3(1, A, B, C, D, E, F, G, H, I, J, K) :-
	sum_11_2(A, B, C, D, E, F, G, H, I, J, K).
sum_12_3(0, 0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_3(A, B, C, D, E, F, G, H, I, J).
sum_12_2(1, A, B, C, D, E, F, G, H, I, J, K) :-
	sum_11_1(A, B, C, D, E, F, G, H, I, J, K).
sum_12_2(0, 0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_2(A, B, C, D, E, F, G, H, I, J).
sum_12_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_11_8(1, 1, A, B, C, D, E, 1, F, 1, G) :-
	sum_9_6(A, B, C, D, E, 1, F, 1, G).
sum_11_8(0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_8(A, B, C, D, E, F, G, H, I, J).
sum_11_7(1, 1, A, B, C, D, E, 1, F, 1, G) :-
	sum_9_5(A, B, C, D, E, 1, F, 1, G).
sum_11_7(0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_7(A, B, C, D, E, F, G, H, I, J).
sum_11_6(1, 1, A, B, C, D, E, 1, F, 1, G) :-
	sum_9_4(A, B, C, D, E, 1, F, 1, G).
sum_11_6(0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_6(A, B, C, D, E, F, G, H, I, J).
sum_11_5(1, 1, A, B, C, D, E, 1, F, 1, G) :-
	sum_9_3(A, B, C, D, E, 1, F, 1, G).
sum_11_5(0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_5(A, B, C, D, E, F, G, H, I, J).
sum_11_4(1, 1, A, B, C, D, E, 1, F, 1, G) :-
	sum_9_2(A, B, C, D, E, 1, F, 1, G).
sum_11_4(0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_4(A, B, C, D, E, F, G, H, I, J).
sum_11_3(1, 1, A, B, C, D, E, 1, F, 1, G) :-
	sum_9_1(A, B, C, D, E, 1, F, 1, G).
sum_11_3(0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_3(A, B, C, D, E, F, G, H, I, J).
sum_11_2(1, 1, _, _, _, _, _, 1, _, 1, _) :-
	fail.
sum_11_2(0, A, B, C, D, E, F, G, H, I, J) :-
	sum_10_2(A, B, C, D, E, F, G, H, I, J).
sum_11_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_10_8(1, A, B, C, D, E, F, G, H, I) :-
	sum_9_7(A, B, C, D, E, F, G, H, I).
sum_10_8(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_10_7(1, A, B, C, D, E, F, G, H, I) :-
	sum_9_6(A, B, C, D, E, F, G, H, I).
sum_10_7(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_10_6(1, A, B, C, D, E, F, G, H, I) :-
	sum_9_5(A, B, C, D, E, F, G, H, I).
sum_10_6(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_10_5(1, A, B, C, D, E, F, G, H, I) :-
	sum_9_4(A, B, C, D, E, F, G, H, I).
sum_10_5(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_10_4(1, A, B, C, D, E, F, G, H, I) :-
	sum_9_3(A, B, C, D, E, F, G, H, I).
sum_10_4(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_10_3(1, A, B, C, D, E, F, G, H, I) :-
	sum_9_2(A, B, C, D, E, F, G, H, I).
sum_10_3(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_10_2(1, A, B, C, D, E, F, G, H, I) :-
	sum_9_1(A, B, C, D, E, F, G, H, I).
sum_10_2(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_10_1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_9_8(1, A, B, C, D, E, F, G, H) :-
	sum_8_7(A, B, C, D, E, F, G, H).
sum_9_8(0, 0, 0, _, _, _, 0, _, 0).
sum_9_7(1, A, B, C, D, E, F, G, H) :-
	sum_8_6(A, B, C, D, E, F, G, H).
sum_9_7(0, 0, 0, _, _, _, 0, _, 0).
sum_9_6(1, A, B, C, D, E, F, G, H) :-
	sum_8_5(A, B, C, D, E, F, G, H).
sum_9_6(0, 0, 0, A, B, C, 0, D, 0) :-
	sum_6_6(A, B, C, 0, D, 0).
sum_9_5(1, A, B, C, D, E, F, G, H) :-
	sum_8_4(A, B, C, D, E, F, G, H).
sum_9_5(0, 0, 0, A, B, C, 0, D, 0) :-
	sum_6_5(A, B, C, 0, D, 0).
sum_9_4(1, A, B, C, D, E, F, G, H) :-
	sum_8_3(A, B, C, D, E, F, G, H).
sum_9_4(0, 0, 0, A, B, C, 0, D, 0) :-
	sum_6_4(A, B, C, 0, D, 0).
sum_9_3(1, A, B, C, D, E, F, G, H) :-
	sum_8_2(A, B, C, D, E, F, G, H).
sum_9_3(0, 0, 0, A, B, C, 0, D, 0) :-
	sum_6_3(A, B, C, 0, D, 0).
sum_9_2(1, A, B, C, D, E, F, G, H) :-
	sum_8_1(A, B, C, D, E, F, G, H).
sum_9_2(0, 0, 0, A, B, C, 0, D, 0) :-
	sum_6_2(A, B, C, 0, D, 0).
sum_9_1(0, 0, 0, 0, 0, 0, 0, 0, 0).
sum_8_8(1, A, 1, B, C, D, E, F) :-
	sum_7_7(A, 1, B, C, D, E, F).
sum_8_8(0, 0, _, _, _, _, _, _).
sum_8_7(1, A, 1, B, C, D, E, F) :-
	sum_7_6(A, 1, B, C, D, E, F).
sum_8_7(0, 0, _, _, _, _, _, _).
sum_8_6(1, A, 1, B, C, D, E, F) :-
	sum_7_5(A, 1, B, C, D, E, F).
sum_8_6(0, 0, A, B, C, D, E, F) :-
	sum_6_6(A, B, C, D, E, F).
sum_8_5(1, A, 1, B, C, D, E, F) :-
	sum_7_4(A, 1, B, C, D, E, F).
sum_8_5(0, 0, A, B, C, D, E, F) :-
	sum_6_5(A, B, C, D, E, F).
sum_8_4(1, A, 1, B, C, D, E, F) :-
	sum_7_3(A, 1, B, C, D, E, F).
sum_8_4(0, 0, A, B, C, D, E, F) :-
	sum_6_4(A, B, C, D, E, F).
sum_8_3(1, A, 1, B, C, D, E, F) :-
	sum_7_2(A, 1, B, C, D, E, F).
sum_8_3(0, 0, A, B, C, D, E, F) :-
	sum_6_3(A, B, C, D, E, F).
sum_8_2(1, A, 1, B, C, D, E, F) :-
	sum_7_1(A, 1, B, C, D, E, F).
sum_8_2(0, 0, A, B, C, D, E, F) :-
	sum_6_2(A, B, C, D, E, F).
sum_8_1(0, 0, 0, 0, 0, 0, 0, 0).
sum_7_8(_, _, _, _, _, _, _).
sum_7_7(1, 1, 1, 1, 1, A, B) :-
	sum_2_2(A, B).
sum_7_7(0, _, _, _, _, _, _).
sum_7_6(1, 1, 1, 1, 1, A, B) :-
	sum_2_1(A, B).
sum_7_6(0, A, B, C, D, E, F) :-
	sum_6_6(A, B, C, D, E, F).
sum_7_5(1, 1, 1, 1, 1, _, _) :-
	fail.
sum_7_5(0, A, B, C, D, E, F) :-
	sum_6_5(A, B, C, D, E, F).
sum_7_4(1, 1, 1, 1, 1, _, _) :-
	fail.
sum_7_4(0, A, B, C, D, E, F) :-
	sum_6_4(A, B, C, D, E, F).
sum_7_3(1, 1, 1, 1, 1, _, _) :-
	fail.
sum_7_3(0, A, B, C, D, E, F) :-
	sum_6_3(A, B, C, D, E, F).
sum_7_2(1, 1, 1, 1, 1, _, _) :-
	fail.
sum_7_2(0, A, B, C, D, E, F) :-
	sum_6_2(A, B, C, D, E, F).
sum_7_1(0, 0, 0, 0, 0, 0, 0).
sum_6_7(_, _, _, _, _, _).
sum_6_6(1, A, B, C, D, E) :-
	sum_5_5(A, B, C, D, E).
sum_6_6(0, 0, _, _, _, _).
sum_6_5(1, A, B, C, D, E) :-
	sum_5_4(A, B, C, D, E).
sum_6_5(0, 0, _, _, _, _).
sum_6_4(1, A, B, C, D, E) :-
	sum_5_3(A, B, C, D, E).
sum_6_4(0, 0, A, B, C, D) :-
	sum_4_4(A, B, C, D).
sum_6_3(1, A, B, C, D, E) :-
	sum_5_2(A, B, C, D, E).
sum_6_3(0, 0, A, B, C, D) :-
	sum_4_3(A, B, C, D).
sum_6_2(1, A, B, C, D, E) :-
	sum_5_1(A, B, C, D, E).
sum_6_2(0, 0, A, B, C, D) :-
	sum_4_2(A, B, C, D).
sum_6_1(0, 0, 0, 0, 0, 0).
sum_5_6(_, _, _, _, _).
sum_5_5(1, 1, A, B, C) :-
	sum_3_3(A, B, C).
sum_5_5(0, _, _, _, _).
sum_5_4(1, 1, A, B, C) :-
	sum_3_2(A, B, C).
sum_5_4(0, A, B, C, D) :-
	sum_4_4(A, B, C, D).
sum_5_3(1, 1, A, B, C) :-
	sum_3_1(A, B, C).
sum_5_3(0, A, B, C, D) :-
	sum_4_3(A, B, C, D).
sum_5_2(1, 1, _, _, _) :-
	fail.
sum_5_2(0, A, B, C, D) :-
	sum_4_2(A, B, C, D).
sum_5_1(0, 0, 0, 0, 0).
sum_4_5(_, _, _, _).
sum_4_4(1, A, B, C) :-
	sum_3_3(A, B, C).
sum_4_4(0, 0, _, _).
sum_4_3(1, A, B, C) :-
	sum_3_2(A, B, C).
sum_4_3(0, 0, _, _).
sum_4_2(1, A, B, C) :-
	sum_3_1(A, B, C).
sum_4_2(0, 0, A, B) :-
	sum_2_2(A, B).
sum_4_1(0, 0, 0, 0).
sum_3_4(_, _, _).
sum_3_3(1, A, B) :-
	sum_2_2(A, B).
sum_3_3(0, _, _).
sum_3_2(1, A, B) :-
	sum_2_1(A, B).
sum_3_2(0, A, B) :-
	sum_2_2(A, B).
sum_3_1(0, 0, 0).
sum_2_3(_, _).
sum_2_2(1, A) :-
	sum_1_1(A).
sum_2_2(0, 0).
sum_2_1(0, 0).
sum_1_2(_).
sum_1_1(0).
