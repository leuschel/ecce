/* Store(OldEnv, VariableName, NewValue, NewEnv) */
store([],Key,Value,[Key/Value]) :- 
   print(assigning_undefined_var(Key,Value)),nl.
store([Key/_Value2|T],Key,Value,[Key/Value|T]).
store([Key2/Value2|T],Key,Value,[Key2/Value2|BT]) :-
   Key \== Key2, store(T,Key,Value,BT).

/* lookup(VariableName, Env, CurrentValue) */
lookup(Key,[],_) :- print(lookup_var_not_found_error(Key)),nl,fail.
lookup(Key,[Key/Value|_T],Value).
lookup(Key,[Key2/_Value2|T],Value) :-
   Key \== Key2,lookup(Key,T,Value).

def(Env,Key,[Key/undefined|Env]).



eval(X,_Env,Res) :- number(X),Res=X.
eval('$'(X),Env,Res) :- lookup(X,Env,Res).
eval('+'(X,Y),Env,Res) :- eval(X,Env,RX), eval(Y,Env,RY), Res is RX+RY.

eint(':='(X,V),In,Out) :- eval(V,In,Res),store(In,X,Res,Out).
eint('def'(X),In,Out) :- def(In,X,Out).
eint((X;Y),In,Out) :- eint(X,In,I2), eint(Y,I2,Out).


eint(if(BE,S1,S2),In,Out) :-
      eval_be(BE,In,Res),
      (Res=true -> eint(S1,In,Out) ; eint(S2,In,Out)).

eval_be('='(X,Y),Env,Res) :- eval(X,Env,RX), eval(Y,Env,RY),
      (RX=RY -> Res=true ; Res=false), print(eq(RX,RY,Res)),nl.
eval_be('<'(X,Y),Env,Res) :- eval(X,Env,RX), eval(Y,Env,RY),
      (RX<RY -> Res=true ; Res=false).

test2(X,R) :- eint( ('def'(x) ;
                   ':='(x,'$'(input)) ;
                    def(z) ;
                    ':='(x, '$'(x)+1) ;
                    ':='(z ,'$'(x)+('$'(x)+2))
                    ) , [input/X],R).

%test3(R) :- eint( (def x; x:= 1; def z; if($x=1, z:=1, z:=2)), [],R).
%test4(X,R) :- eint( (def x; x:= X; def z; if($x=1, z:=1, z:=2)), [],R).

eval([HS|String],_,Res) :- name(Res,[HS|String]).
eint(println(S),E,E) :- eval(S,E,ES),print(ES),nl.
eint(while(BE,S),In,Out) :- 
      eval_be(BE,In,Res),
      ((Res=true -> eint(';'(S,while(BE,S)),In,Out)) ; In=Out).
eint(X,E,O) :- print('**** uncovered :'), print(X),nl, print(env(E)),nl, print(out(O)),nl,
  fail.

ewhile(BE,S,In,Out) :- 
 eval_be(BE,In,Res),
      ((Res=true)
        -> (eint(S,In,In2), ewhile(BE,S,In2,Out) )
        ; In=Out).

%test5(R) :- eint( (def x; x:= 10; def z; z:= 0; while(($z)<($x),(println($z);z:=($z+1))) ; println("done")), [],R).
% alternate def:
% eint(while(BE,S),In,Out) :- eint(if(BE,';'(S,while(BE,S)),skip),In,Out).


p(s,X,R) :- !,R=X.
p(_,X,s(X)).