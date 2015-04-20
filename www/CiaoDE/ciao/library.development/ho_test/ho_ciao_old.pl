
apply(PredName,Args) :- 
	Call =.. [PredName|Args],
	call(Call).

defpred(L) :-
	L=[(apply(PredName,_) :- _)|_],
	!,
	new_name(PredName),
	def_known_pred(L,PredName).
defpred(L) :-
	def_known_pred(L,_).

def_known_pred([],_).
def_known_pred([(apply(PredName,Args) :- Body)|Rest],PredName) :- 
	defclause(PredName,Args,Body),
	def_known_pred(Rest,PredName).

defclause(PredName,Args,Body) :- 
	Head =.. [PredName|Args],
	assert( (Head :- Body) ).

new_name(Name) :- 
	inc(counter(N)), 
	name(N,NS),
	append("dynamic_",NS,NameS),
	name(Name,NameS).

:- dynamic counter/1.
counter(0).

inc(counter(X)) :- 
	retract(counter(X)),
	NX is X+1,
	assert(counter(NX)).
