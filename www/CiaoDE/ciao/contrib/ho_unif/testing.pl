:- module(testing,
	[
	    example1/0,
	    example2/0,
	    example3/0,
 	    example4/0,
 	    example5/0,
 	    example6/0,
 	    example7/0,
 	    example8/0,
 	    example9/0,
 	    example10/0,
 	    example11/0,
 	    example12/0,
 	    example13/0,
 	    example14/0,
 	    example15/0,
 	    example16/0
	],
	[]).

:- use_module(lambda_terms, [new_ref/2, display_ref/1,get_ref/2]).

:- use_module(hnorm,[hnorm/2]).

:- use_module(pattern_unify, [pattern_unify/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 1
%   unify  \x.x  with  \x.(M x)
%   result should be  M -> \x.x

example1 :-
	term1(T1,[]),
	term2(T2,[M]),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2),
	hnorm(T2,T3),
	show_terms_after_norm(T1,T3),
	display('M = '), display_ref(M), nl.

term1(Ref,[]) :-
	new_ref(dB(1),Ref1),
	new_ref(lam(1, Ref1),Ref).

term2(Ref,[M]) :-
	new_ref(dB(1),Ref1),
	new_ref(var('M',1),M),
	new_ref(app(M, [Ref1]), Ref2),
	new_ref(lam(1, Ref2), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 2
%   unify  \x.(c x)  with  \x.(c (N x))
%   result should be  N -> \x.x

example2 :-
	term3(T1),
	term4(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term3(Ref) :-
	new_ref(dB(1),Ref1),
	new_ref(const('c',1),C),
	new_ref(app(C, [Ref1]), Ref2),
	new_ref(lam(1, Ref2), Ref).

term4(Ref) :-
	new_ref(dB(1),Ref1),
	new_ref(const('c',1),C),
	new_ref(var('N',1),N),
	new_ref(app(N, [Ref1]), Ref2),
	new_ref(app(C, [Ref2]), Ref3),
	new_ref(lam(1, Ref3), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 3
%   unify  \x\y.(c y x)  with  N
%   result should be  N -> \x\y.(c y x)

example3 :-
	term5(T1),
	term6(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term5(Ref) :-
	new_ref(var('N',1), Ref).

term6(Ref) :-
	new_ref(dB(1),Ref1),
	new_ref(dB(2),Ref2),
	new_ref(const('c',1),C),
	new_ref(app(C, [Ref1,Ref2]), Ref3),
	new_ref(lam(2, Ref3), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 4
%   unify  \x\y.(c x y)  with  \x.(c (N x))
%   result should be  N -> \x.x

example4 :-
	term7(T1),
	term8(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term7(Ref) :-
	new_ref(dB(1),Ref1),
	new_ref(dB(2),Ref2),
	new_ref(const('c',1),C),
	new_ref(app(C, [Ref2,Ref1]), Ref3),
	new_ref(lam(2, Ref3), Ref).

term8(Ref) :-
	new_ref(dB(1),Ref1),
	new_ref(var('N',1),N),
	new_ref(app(N, [Ref1]), Ref2),
	new_ref(const('c',1),C),
	new_ref(app(C, [Ref2]), Ref3),
	new_ref(lam(1, Ref3), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 5
%   unify  (X a b)  with  (Y b c)
%   result should be  X -> \x.\y.(Z x y)  and  Y -> \x\y.(Z a x)

example5 :-
	term9(T1),
	term10(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term9(Ref) :-
	new_ref(var('X',1),X),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(app(X, [A,B]), Ref).

term10(Ref) :-
	new_ref(var('Y',2),Y),
	new_ref(const('b',3),B),
	new_ref(const('c',4),C),
	new_ref(app(Y, [B,C]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 6
%   unify  (X a b)  with  (c (Y b c))
%   result should be  X -> \x\y.(c (Z x y))  and  Y -> \x\y.(Z a x)

example6 :-
	term11(T1),
	term12(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term11(Ref) :-
	new_ref(var('X',1),X),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(app(X, [A,B]), Ref).

term12(Ref) :-
	new_ref(var('Y',2),Y),
	new_ref(const('b',3),B),
	new_ref(const('c',1),C1),
	new_ref(const('c',3),C3),
	new_ref(app(Y, [B,C3]), Ref1),
	new_ref(app(C1, [Ref1]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 7
%   unify  (c (X a b) (X b d))  with  (c (Y b c) (b d))
%   result should be X -> \x\y.(x y)  and  Y -> \x\y.(a x)

example7 :-
	term13(T1),
	term14(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term13(Ref) :-
	new_ref(var('X',1),X),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B1),
	new_ref(const('b',3),B2),
	new_ref(const('c',1),C),
	new_ref(const('d',2),D),
	new_ref(app(X, [B1,D]), Ref1),
	new_ref(app(X, [A,B2]), Ref2),
	new_ref(app(C, [Ref2,Ref1]), Ref).

term14(Ref) :-
	new_ref(var('Y',2),Y),
	new_ref(const('b',3),B),
	new_ref(const('c',1),C1),
	new_ref(const('c',3),C3),
	new_ref(const('d',2),D),
	new_ref(app(B, [D]), Ref1),
	new_ref(app(Y, [B,C3]), Ref2),
	new_ref(app(C1, [Ref2,Ref1]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 8
%   unify  \x.(c (X a b) (X x d)) with 
%          \x.(c (Y b c) (x d))
%   result should be  X -> \x\y.(x y)  and  Y -> \x\y.(a x)

example8 :-
	term15(T1),
	term16(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term15(Ref) :-
	new_ref(var('X',1),X),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(const('c',1),C),
	new_ref(const('d',3),D),
	new_ref(dB(1),Ref1),
	new_ref(app(X, [A,B]), Ref2),
	new_ref(app(X, [Ref1,D]), Ref3),
	new_ref(app(C, [Ref2,Ref3]), Ref4),
	new_ref(lam(1,Ref4), Ref).

term16(Ref) :-
	new_ref(var('Y',2),Y),
	new_ref(const('b',3),B),
	new_ref(const('c',1),C1),
	new_ref(const('c',3),C3),
	new_ref(const('d',3),D),
	new_ref(dB(1),Ref1),
	new_ref(app(Y, [B,C3]), Ref2),
	new_ref(app(Ref1, [D]), Ref3),
	new_ref(app(C1, [Ref2,Ref3]), Ref4),
	new_ref(lam(1,Ref4), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 9
%   unify  (X a b c)  with  (X c b a)
%   result should be  X -> \x\y\z.(W(1) y)

example9 :-
	new_ref(var('X',1),X),
	term17(X,T1),
	term18(X,T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term17(X, Ref) :-
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(const('c',3),C),
	new_ref(app(X, [A,B,C]), Ref).

term18(X, Ref) :-
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(const('c',3),C),
	new_ref(app(X, [C,B,A]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 10
%   unify  (X a b)  with  (c (X b c))
%   Failure due to occurs check

example10 :-
	new_ref(var('X',1),X),
	term19(X,T1),
	term20(X,T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term19(X, Ref) :-
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(app(X, [A,B]), Ref).

term20(X, Ref) :-
	new_ref(const('b',3),B),
	new_ref(const('c',1),C1),
	new_ref(const('c',3),C3),
	new_ref(app(X, [B,C3]), Ref1),
	new_ref(app(C1, [Ref1]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 11
%   unify  (X a b)  with  (Y b c)
%   result should be  X -> \x\y.(Z y)  and  Y -> \x\y.(Z x)

example11 :-
	term21(T1),
	term22(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term21(Ref) :-
	new_ref(var('X',1),X),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(app(X, [A,B]), Ref).

term22(Ref) :-
	new_ref(var('Y',1),Y),
	new_ref(const('b',3),B),
	new_ref(const('c',3),C),
	new_ref(app(Y, [B,C]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 12
%   unify  (X a b c)  with  (Y c)
%   result should be  X -> \x\y\z.(W x z)  and  Y -> \x.(W a x)

example12 :-
	term23(T1),
	term24(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term23(Ref) :-
	new_ref(var('X',1),X),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(const('c',3),C),
	new_ref(app(X, [A,B,C]), Ref).

term24(Ref) :-
	new_ref(var('Y',2),Y),
	new_ref(const('c',3),C),
	new_ref(app(Y, [C]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 13
%   unify  X a b  with  (a (Y b c))
%   result should be  X -> \x\y.(x (W x y))  and  Y -> \x\y.(H a x)

example13 :-
	term25(T1),
	term26(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term25(Ref) :-
	new_ref(var('X',1),X),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(app(X, [A,B]), Ref).

term26(Ref) :-
	new_ref(var('Y',2),Y),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(const('c',3),C),
	new_ref(app(Y, [B,C]), Ref1),
	new_ref(app(A, [Ref1]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 14
%   unify  (X a b)  with  (d (Y b c))
%   Failure due to occurs check

example14 :-
	term27(T1),
	term28(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term27(Ref) :-
	new_ref(var('X',1),X),
	new_ref(const('a',2),A),
	new_ref(const('b',3),B),
	new_ref(app(X, [A,B]), Ref).

term28(Ref) :-
	new_ref(var('Y',2),Y),
	new_ref(const('b',3),B),
	new_ref(const('c',3),C),
	new_ref(const('d',3),D),
	new_ref(app(Y, [B,C]), Ref1),
	new_ref(app(D, [Ref1]), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 15
%   unify  a  with  a

example15 :-
	term29(T1),
	term30(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term29(Ref) :-
	new_ref(const('a',1), Ref).

term30(Ref) :-
	new_ref(const('a',1), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXAMPLE 16
%   unify  \x.(a (x b))  with  \x.(a (x b))

example16 :-
	term31(T1),
	term32(T2),
	show_terms_before(T1,T2),
	pattern_unify(T1,T2),
	show_terms_after(T1,T2).

term31(Ref) :-
	new_ref(const('a',1), A),
	new_ref(const('b',1), B),
	new_ref(dB(1),Ref1),
	new_ref(app(A,[Ref1,B]),Ref2),
	new_ref(lam(1,Ref2), Ref).

term32(Ref) :-
	new_ref(const('a',1), A),
	new_ref(const('b',1), B),
	new_ref(dB(1),Ref1),
	new_ref(app(A,[Ref1,B]),Ref2),
	new_ref(lam(1,Ref2), Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_terms_before(T1, T2) :-
	display('Before unification:'), nl,
	display('     T1: '), display_ref(T1), nl,
	display('     T2: '), display_ref(T2), nl.

show_terms_after(T1, T2) :-
	display('After unification:'), nl,
	display('     T1: '), display_ref(T1), nl,
	display('     T2: '), display_ref(T2), nl.

show_terms_after_norm(T1, T2) :-
	display('After unification and normalization:'), nl,
	display('     T1: '), display_ref(T1), nl,
	display('     T2: '), display_ref(T2), nl.



