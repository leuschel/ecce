:- module( xtchecks_msg , [ print_xtcheck_error/1 ] , [ assertions , regtypes ] ).

:- use_package( library( 'ciaopp/api/ciaopp_api' ) ).

:- comment( bug , "1.- error_message has to be used when printing message
                  (instead of doing it ad-hoc). A new string version
                  of pretty and assertions_write predicates have to be
                  implemented.").

:- comment( bug , "2.- api_internals should be used instead of
                   pretty_print (It converts 'module:pred' to
                   module:pred" ).


get_loc( as${ locator=> Loc } , Loc ).




list2conj( []    , true  ) :-
	!.

list2conj( [A|B] , (A,Br) ) :-
	B \== [],
	!,
	list2conj( B , Br ).

list2conj( [A]   , A      ) :-
	!.




print_xtcheck_error( xtcheck( FakeGoalArg , Dic , GoalLoc , Assertion ) ) :-
	Assertion = as${ type => ppcheck , call => RealCheckPoint },
	% It is like runtime exception
	GoalLoc = loc${file => S , line_begin => LB , line_end => LE },

	% we transform from f(X,Y,Z) to (X,Y,U)
	get_xtchecks_goal( FakeGoalArg , FakeGoal ),
	FakeGoal =.. [ _ | Goal ],

	list2conj(RealCheckPoint, CheckPoint ),

	( nonvar( S ), nonvar( LB ), nonvar( LE )
	  -> error_message( loc( S , LB , LE ),
	                 " VarsChecked: ~p~n~8|"||
		         "Check Point Assertion failed:~n~p", 
		         [ '$goal'( Goal , Dic ),
			   '$goal'( CheckPoint , Dic ) ] )
	; true ),
	!.

print_xtcheck_error( xtcheck( GoalArg , Dic , GoalLoc , Assertion ) ) :-
	get_xtchecks_goal( GoalArg , Goal ),
	
	% Add Called: the term at the end if it was specified
	( 
	    get_xtchecks_called( GoalArg , Called )
	->
 	    EndString = "~2|Called: ~p",
	    EndArgs   = [ '$goal'( Called ) ]
	;
	    EndString = "",
	    EndArgs   = []
	),

	( 
	    GoalLoc = loc${file =>  S , line_begin =>  LB , line_end =>  LE }
	->
	    true
	;
	    GoalLoc = loc( S , LB , LE )
	),
 	get_loc( Assertion , AsLoc ),
	AsLoc = loc${file => AS , line_begin => ALB , line_end => ALE },
% 	error_message( loc( AS , ALB , ALE ) ,
%                        "~p Assertion failed:~n~4|~p" || EndString, 
% 		       [ '$goal'( Goal , Dic ) , Assertion | EndArgs ] ),
	error_message( loc(S,LB,LE) ,
                       "~p Assertion failed:~n~4|~p" || EndString, % In ~p~n~4|
		       [ '$goal'( Goal , Dic ) , Assertion | EndArgs ] ), % 2 arg: , AsLoc
	( nonvar( AS ), nonvar( ALB ), nonvar( ALE )
	  -> error_message( loc( AS , ALB , ALE ) , "Assertion failed." , [] )
	; true ),
	!.

print_xtcheck_error( A ) :-
	error_message( "print_xtcheck_error: While printing ~w~n" , [A] ).




get_xtchecks_goal( called( A , _ ) , A ) :-  !.
get_xtchecks_goal(         A       , A ).

get_xtchecks_called( called( _ , A ) , A ).
