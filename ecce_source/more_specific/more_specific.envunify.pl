:- module( 'more_specific.envunify' , [ 'more_specific.envunify:more_specific_transformation'/1 ] ).
/* file: more_specific.envunify.pro */

%:- dynamic more_specific_transformation/1.

/* instantiates goals during the unfolding process to more specific versions */

'more_specific.envunify:more_specific_transformation'([]).
'more_specific.envunify:more_specific_transformation'([H|T]) :-
	mst_instantiate_atom(H),!,
	envunify_msv(H,T),
	'more_specific.envunify:more_specific_transformation'(T).

mst_instantiate_atom(Atom) :-
	mst_matching_heads(Atom,Heads),!,
	((Heads = [])
	 -> true  /* do nothing, dead literals will be detected anyway, and the atom could be a built-in or a negative literal */
	;  (mst_msg_of_list(Heads,MsgOfHeads),
	    Atom=MsgOfHeads)
	).

mst_matching_heads(Atom,Heads) :-
	bd_findall(Atom,claus(Nr,Atom,Body),Heads).

mst_msg_of_list([Atom1|Tail],MSG) :-
	mst_l_msg(Atom1,Tail,MSG).
mst_l_msg(Atom,[],Atom).
mst_l_msg(Atom,[Atom2|T],MSG) :-
	msg(Atom,Atom2,IntMSG),!,
	mst_l_msg(IntMSG,T,MSG).


envunify_msv(H,T) :-
	nonvar(H),
	H = get_binding(X,Y,Z),!,
	((member(get_binding(X1,Y1,Z1),T), X1==X, Y1==Y) 
	 -> (Z = Z1,print(get_binding(X1,Y1,Z1)),nl)
	 ;  true
	).
envunify_msv(H,T).
