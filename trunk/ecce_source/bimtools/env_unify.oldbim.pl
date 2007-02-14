


env_unify(Term1,Term2,Environment,EnvAfterSub) :-
	unnumbervars(struct(group,[Term1,Term2,Environment]),
			struct(group,[T12,T12,EnvAfterSub]) ),
	numbervars(EnvAfterSub,1,_VarIndex).


env_unify(Term1,Term2,Env1,EnvAfterSub1,Env2,EnvAfterSub2) :-
	unnumbervars(struct(group,[Term1,Env1]),
			struct(group,[T12,EnvAfterSub1]) ),
	unnumbervars(struct(group,[Term2,Env2]),
			struct(group,[T12,EnvAfterSub2]) ),
	numbervars(gr(EnvAfterSub1,EnvAfterSub2),1,_VarIndex).


unify_test(Term1,Term2) :-
	unnumbervars(struct(group,[Term1,Term2]),
			 struct(group,[T12,T12]) ).

unify_rename_test(Term1,Term2) :-
	unnumbervars(Term1,T12),
	unnumbervars(Term2,T12).

instance_of(X,Y) :-
	unnumbervars(Y,X).



/* ?-unnumbervars(struct(f,['$VAR'(1),'$VAR'(1)]),X).
  X = struct(f,[_45,_45]) */

unnumbervars(G,NG) :-
	mng(G,NG,[],Sub).


mng('$VAR'(N),X,[],[sub(N,X)]).
mng('$VAR'(N),X,[sub(N,X)|T],[sub(N,X)|T]).
mng('$VAR'(N),X,[sub(M,Y)|T],[sub(M,Y)|T1]) :-
	not(N=M),
	mng('$VAR'(N),X,T,T1).
mng(struct(F,Args),struct(F,IArgs),InSub,OutSub) :-
	l_mng(Args,IArgs,InSub,OutSub).

l_mng([],[],Sub,Sub).
l_mng([H|T],[IH|IT],InSub,OutSub) :-
	mng(H,IH,InSub,IntSub),
	l_mng(T,IT,IntSub,OutSub).

/* l_solve(Rules,G,I,O) :- print(solve(G,I,O)),nl,fail. */
l_solve(Rules,[],Env,Env).
l_solve(Rules,[H|T],InEnv,OutEnv) :-
	member(struct(clause,[Head|Body]),Rules),
	env_unify(H,Head,struct([],[]),
		     struct([],[]),
		     struct(env,[Head|Body]),
		     struct(env,[ResolvedHead|ResolvedBody])),
	l_solve(Rules,ResolvedBody,ResolvedHead,OutHead),
	env_unify(H,OutHead,struct(env,[InEnv|T]),
		     struct(env,[IntEnv|ResolvedT]),
		     struct([],[]),
		     struct([],[])),
	l_solve(Rules,ResolvedT,IntEnv,OutEnv).


nrev_prog([struct(clause,[struct(nrev,[struct([],[]),struct([],[])]) ]),
	   struct(clause,[struct(nrev,
				[struct(.,['$VAR'(1),'$VAR'(2)]),'$VAR'(3)]),
				struct(nrev,['$VAR'(2),'$VAR'(4)]),
				struct(append,['$VAR'(4),struct(.,['$VAR'(1),
						struct([],[])]),'$VAR'(3)]) ]),
	   struct(clause,[struct(append,[struct([],[]),'$VAR'(1),'$VAR'(1)]) ]),
	   struct(clause,[struct(append,
				[struct(.,['$VAR'(1),'$VAR'(2)]),'$VAR'(3),
					struct(.,['$VAR'(1),'$VAR'(4)])]),
				struct(append,['$VAR'(2),'$VAR'(3),'$VAR'(4)]) ])
	]).

test_l_nrev(GroundList,Result) :-
	nrev_prog(Progr),
	l_solve(Progr,[struct(nrev,[GroundList,'$VAR'(0)])],'$VAR'(0),Result).

t(L) :-	term_compile(L,GL),
	test_l_nrev(GL,Result),
	print(Result),nl,
	term_decompile(Result,Res),
	print(Res),nl.


term_compile(NonGround,Ground) :-
	copy(NonGround,CNG),
	structify(CNG,Ground),
	numbervars(Ground,1,_VarIndex).

l_term_compile(NonGroundLs,GroundLs) :-
	copy(NonGroundLs,CNGLs),
	l_structify(CNGLs,GroundLs),
	numbervars(GroundLs,1,_Varindex).


structify(X,X) :- var(X),!.
structify(Y,struct(F,SArgs)) :-
	Y =..[F|Args],
	l_structify(Args,SArgs).
l_structify([],[]).
l_structify([H|T],[SH|ST]) :-
	structify(H,SH),
	l_structify(T,ST).

term_decompile(Ground,NonGround) :-
	unnumbervars(Ground,NGS),
	destructify(NGS,NonGround).

l_term_decompile(Ground,NonGround) :-
	unnumbervars(Ground,NGS),
	l_destructify(NGS,NonGround).



destructify(X,X) :- var(X),!.
destructify(struct(F,SArgs),Y) :-
	l_destructify(SArgs,Args),
	Y =..[F|Args].
l_destructify([],[]).
l_destructify([H|T],[DSH|DST]) :-
	destructify(H,DSH),
	l_destructify(T,DST).

member(X,[X|_T]).
member(X,[_Y|T]) :-
	member(X,T).
