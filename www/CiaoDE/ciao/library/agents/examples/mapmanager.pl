%%--------------------------------------------------------
%% A Map Manager Agent.
%%-----------------------------------------------------------

:- agent(mapmanager).

:- include('agents/protocols/ask/messages').

:- use_protocol('agents/protocols/ask',asker).
:- use_protocol('agents/protocols/ask',answer).

:- activities([manage_map]).
:- tasks([update_map/0,answer_ask/0]).

:- data map/2.

main :-
	register_service([manage_map]),
	set_fact(map([],[]),
	add_task(update_map,true),
	add_task(answer_ask,true).

update_map :-
	Messg = message${type => inform},
	receive(co,Messg).

when inform${sender => co, content => landmark(L,P)} do
	map(Landmarks,Positions),
	( member(L,Landmarks) ->
	  update(Positions,L,P)
	;
	  update(Landmarks,L),
	  update(Positions,L,P)
	).
	
answer_ask :-
	Messg = message${type => ask},
	receive(X,Messg).

when ask${sender => X, content => position_landmark(L)} do
	map(Landmarks,Positons),
	( member(L,Landmarks), member((L,P),Positions) ->
	  send(X,message${type => inform, content => position_landmark(L,P)})
	;
	  send(X,message${type => inform, content => dont_know(L)})
	).

when ask${sender => X, content => landmarks} do
	compute_landmarks_quality(Quality),
	send(X,message${type => inform, content => landmarks(Quality)}).