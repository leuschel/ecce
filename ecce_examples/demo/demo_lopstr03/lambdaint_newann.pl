:- include('~/cvs_root/cogen2/logen_source/logen').
%:- mode l_eval(i,i,o).

:- residual l_eval/3.
:- filter l_eval(static,type(list(struct('/',[static,dynamic]))),dynamic):l_eval.
logen(l_eval, l_eval([],_E,[])).
logen(l_eval, l_eval([H|T],E,[EH|ET])) :-
        logen(unfold, eval(H,E,EH)),
        logen(unfold, l_eval(T,E,ET)).
%:- mode eval(i,i,o).

:- residual eval/3.
:- filter eval(static,type(list(struct('/',[static,dynamic]))),dynamic):eval.
logen(eval, eval(cst(C),_Env,constr(C,[]))).
logen(eval, eval(constr(C,Args),Env,constr(C,EArgs))) :-
        logen(unfold, l_eval(Args,Env,EArgs)).
logen(eval, eval(var(VKey),Env,Val)) :-
        logen(unfold, lookup(VKey,Env,Val)).
logen(eval, eval(plus(X,Y),Env,constr(XY,[]))) :-
        logen(unfold, eval(X,Env,constr(VX,[]))),
        logen(unfold, eval(Y,Env,constr(VY,[]))),
        logen(rescall, XY is VX+VY).
logen(eval, eval(minus(X,Y),Env,constr(XY,[]))) :-
        logen(unfold, eval(X,Env,constr(VX,[]))),
        logen(unfold, eval(Y,Env,constr(VY,[]))),
        logen(rescall, XY is VX-VY).
logen(eval, eval(times(X,Y),Env,constr(XY,[]))) :-
        logen(unfold, eval(X,Env,constr(VX,[]))),
        logen(unfold, eval(Y,Env,constr(VY,[]))),
        logen(rescall, XY is VX*VY).
logen(eval, eval(eq(X,Y),Env,constr(BoolRes,[]))) :-
        logen(unfold, eval(X,Env,VX)),
        logen(unfold, eval(Y,Env,VY)),
        resif(logen(rescall,(VX=VY)), logen(rescall,BoolRes=true), logen(rescall,BoolRes=false)).
logen(eval, eval(let(VKey,VExpr,InExpr),Env,Result)) :-
        logen(unfold, eval(VExpr,Env,VVal)),
        logen(unfold, store(Env,VKey,VVal,InEnv)),
        logen(unfold, eval(InExpr,InEnv,Result)).
logen(eval, eval(if(Test,Then,Else),Env,Res)) :-
        logen(unfold, eval(Test,Env,TestRes)),
        resif(logen(rescall,TestRes=constr(true,[])), 
             hide_nf(logen(unfold, eval(Then,Env,Res))), 
             hide_nf(logen(unfold, eval(Else,Env,Res)))).
logen(eval, eval(lambda(X,Expr),_Env,lambda(X,Expr))).
logen(eval, eval(apply(Arg,F),Env,Res)) :-
        logen(unfold, eval(F,Env,FVal)),
        logen(unfold, rename(FVal,Env,lambda(X,Expr))),
        logen(unfold, eval(Arg,Env,ArgVal)),
        logen(unfold, store(Env,X,ArgVal,NewEnv)),
        logen(memo, eval(Expr,NewEnv,Res)).
logen(eval, eval(fun(F),_,FunDef)) :-
        logen(unfold, function(F,FunDef)).
logen(eval, eval(print(X),_,constr(true,[]))) :-
        logen(rescall, print(X)),
        logen(rescall, nl).
%logen(eval, eval(X,_,_)) :- logen(rescall, print(uncovered(X))),
%        logen(rescall, nl).

:- residual rename/3.
:- filter rename(dynamic,dynamic,dynamic):rename.
logen(rename, rename(Expr,_Env,RenExpr)) :-
        logen(call,RenExpr=Expr).

:- residual function/2.
:- filter function(dynamic,dynamic):function.
logen(function, function(fib,
		 lambda(x,if(eq(var(x),cst(0)),
					 cst(1),
					 if(eq(var(x),cst(1)),
						cst(1),
						plus(apply(minus(var(x),cst(1)),fun(fib)),
							 apply(minus(var(x),cst(2)),fun(fib)))))))).
%:- mode store(i,i,i,o).
:- residual store/4.
:- filter store(dynamic,static,static,dynamic):store.
logen(store, store([],Key,Value,[Key/Value])).
logen(store, store([Key/_Value2|T],Key,Value,[Key/Value|T])).
logen(store, store([Key2/Value2|T],Key,Value,[Key2/Value2|BT])) :-
        logen(call, Key\==Key2),
        logen(unfold, store(T,Key,Value,BT)).
        
%:- mode lookup(i,i,o).
:- residual lookup/3.
:- filter lookup(statiic,dynamic,dynamic):lookup.
logen(lookup, lookup(Key,[Key/Value|_T],Value)).
logen(lookup, lookup(Key,[Key2/_Value2|T],Value)) :-
        logen(rescall, Key\==Key2),
        logen(unfold, lookup(Key,T,Value)).

:- residual fib/2.
:- filter fib(dynamic,dynamic):fib.
logen(fib, fib(X,FibX)) :-
        logen(unfold, store([],x,X,Env)),
        logen(unfold, eval(apply(cst(X),fun(fib)),Env,constr(FibX,_))).

:- residual bench/2.
:- filter bench(dynamic,dynamic):bench.
logen(bench, bench(Low,Up)) :-
        logen(rescall, Low>Up),
        logen(rescall, print('Done')),
        logen(rescall, nl).
logen(bench, bench(Low,Up)) :-
        logen(rescall, Low=<Up),
        logen(unfold, fib(Low,Res)),
        logen(rescall, !),
        logen(rescall, print(fib(Low))),
        logen(rescall, print(' == ')),
        logen(rescall, print(Res)),
        logen(rescall, nl),
        logen(rescall, L1 is Low+1),
        logen(memo, bench(L1,Up)).
:- bench(20,26).
