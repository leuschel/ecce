/* csp.pl */:- op(900,xfy,->).:- op(900,xfy,//).trans(X->P,X,P).trans(P//Q,X,PT//PQ) :- trans(P,X,PT), trans(Q,X,PQ).	trans(sync(P,Q,X),X,sync(PT,QT,X)) :- trans(P,X,PT), trans(Q,X,QT).	trans(sync(P,Q,E),X,sync(PT,Q,E)) :- trans(P,X,PT), X\== E.	trans(sync(P,Q,E),X,sync(P,QT,E)) :- trans(Q,X,QT), X\== E.	trans(P+_Q,X,PT) :- trans(P,X,PT).trans(_P+Q,X,PQ) :- trans(Q,X,PQ).trans(agent(A),X,AT) :-	agent(A,ADef),	trans(ADef,X,AT).agent(p,(a->agent(p))).agent(r,((a->agent(r))//(a->agent(r)))).agent(memo,((b->stop) + (a->sync(  (b->(a->stop)) , agent(memo), b )))).	trace(_E,[]).trace(E,[X|T]) :-	trans(E,X,ET),	trace(ET,T).unsafe(E) :- trans(E,c,_).unsafe(E) :-	trans(E,_,ET),	unsafe(ET).