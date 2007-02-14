:- module( 'partition.common' , _ ).


get_block([],[],_InBlockSoFar,[],[],[],[]).
get_block([H|T],[HN|TN],InBlockSoFar,InBlock,NotInBlock,
			InSelNr,NotInSelNr) :-
	((pp_cll(sharing(InBlockSoFar,[H]))
	 )
	 -> (InBlock = [H|InBlock2],
	     InSelNr = [HN|InSelNr2],
	     get_block(T,TN,[H|InBlockSoFar],InBlock2,NotInBlock,
			    InSelNr2,NotInSelNr))
	 ;  (NotInBlock = [H|T],
	     NotInSelNr = [HN|TN],
	     InBlock = [],
	     InSelNr = [])
	).


