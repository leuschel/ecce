
foo(X,Y,Z,W) :- eq(X,X2),
  app(X2,Y,XY), len(XY,L),
  snd(Z,Zsnd), fst((L,Zsnd),W).

len([],zero).
len([_|R],succ(RL)) :- len(R,RL).

app([],Y,Y).
app([H|R],Y,[H|RY]) :- app(R,Y,RY).

fst((X,_),X).
snd((_,Y),Y).

eq(X,X).


  
foo2(X,Y,Z,W) :- eq(X,X2),
  app(X2,Y,XY), len(XY,L),
  snd2(Z,Zsnd), fst((L,Zsnd),W).
  
snd2((_,Y),b).