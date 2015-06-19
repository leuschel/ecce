:- module( mst , _ ).

:- use_module( library( aggregates ) ).

:- use_module( '../bimtools' ).


mst_msg_of_list([Atom1|Tail],MSG) :-
	mst_l_msg(Atom1,Tail,MSG).
mst_l_msg(Atom,[],Atom).
mst_l_msg(Atom,[Atom2|T],MSG) :- 
	msg(Atom,Atom2,IntMSG),!,
	mst_l_msg(IntMSG,T,MSG).


mst_instantiate_atom(Atom) :- %print(mst_instantiate_atom(Atom)),nl,
	mst_matching_heads(Atom,Heads),!, %print(heads(Heads)),nl,
	((Heads = [])
	 -> true  /* do nothing, dead literals will be detected anyway, and the atom could be a built-in or a negative literal */
	;  (mst_msg_of_list(Heads,MsgOfHeads),
	    Atom=MsgOfHeads)
	).

mst_matching_heads(Atom,Heads) :- %print(findall(Atom,claus(_Nr,Atom,_Body),Heads)),nl,
	findall(Atom,(claus(_Nr,Atom,_Body), \+(is_inf(Atom))),Heads).
	%print(done_findall(Atom,claus(_Nr,Atom,_Body),Heads)),nl.
