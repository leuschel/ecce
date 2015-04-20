:- module( rtchecks_mod , [
	                 check/1,
	                 check/2,
			 icheck/3,
			 checkc/2,
			 checkiftrue/2,
			 checkiftrue/3,
			 check_comp/4,
			 rt_det_ncp/1,
			 rt_det_1c/1,
			 rt_nf/1,
			 rt_print/1,
			 inst/1,
			 push_parent_goal/1,
			 % current_parent_goal/1,
			 pop_parent_goal/1,
			 clean_parent_stack/1,
			 show_stack/1
		      ] , [assertions, regtypes] ).


:- use_package( library( 'ciaopp/api/ciaopp_api'   ) ).
:- use_module(  library( 'ciaopp/api/xtchecks_msg' ) ).
:- use_module(  engine(  internals   ) ).
:- use_module(  library( terms_check ) ).
:- use_module(  library( messages    ) ).
:- use_module(  library( vndict      ) ).

%       det   nf
% det      1   1
% semidet  1   0
% multidet 0   1
% nondet   0   0
%
% throw( rtcheck( det_nf  , MSG   , GOAL ) )  MSG == notdet ; MSG == fail
% throw( rtcheck( pre     , PRE   , GOAL ) )
% throw( rtcheck( post    , PRE   , GOAL ) )
% throw( rtcheck( pp_check, PRE   , LOC  ) )
% throw( rtcheck( assrt   , ASSRT , LOC  ) )

:- meta_predicate checkc( goal , ? ).
:- meta_predicate check(  goal ).
:- meta_predicate check(  goal , addmodule ).
:- meta_predicate icheck( goal , ? , ? ).
:- meta_predicate checkiftrue( ? , goal ).
:- meta_predicate checkiftrue( ? , goal , addmodule ).
:- meta_predicate icheckiftrue( ? , goal ).
%:- meta_predicate inst( goal ).

:- meta_predicate check_comp( ? , goal , ? , goal ).

:- meta_predicate rt_det_ncp( goal ).
:- meta_predicate rt_nf(      goal ).
:- meta_predicate rt_det_1c(  goal ).

:- comment( module , 

"This library package can be used by programmers directly and it is
also used by the run-time transformations performed by CiaoPP (see the
@tt{CiaoPP} manual) .  Predicates @pred{check/1}, @pred{checkc/2}, and
@pred{checkiftrue/2} are meant typically to be used by programmers,
while @pred{check/2}, @pred{checkc/2}, and @pred{checkiftrue/3} are
typically used by the CiaoPP transformations.  Note that the CiaoPP
run-time checks transformation will replace @pred{check/1} predicates
that appear in the original program with calls to @pred{check/2}, but
the end effect is the same.

As this is a reference manual, we only comment on predicates
@pred{check/1}, @pred{checkc/2} and @pred{checkiftrue/2}.
Documentation on other predicates is left for the @tt{internals}
manual.  Predicate @pred{check/1} allows writing program-point check
assertions.  In other words, the user can write
@verbatim+check(Condition)+ whenever he/she thinks that Condition has
to hold.  The @pred{check/1} call will whther Conditions holds, and if
it does not, it will raise an exception.

The other two predicates: @pred{checkc/2} and @pred{checkiftrue/2} are
used in conjunction and are useful to check postconditions that depend
on a precondition.  For example, assume that condition B has to be
checked only if condition A holds before calling predicate P.  After
calling predicate P it is not possible to check condition A because
some variables may have changed value, so we are need to check whether
condition A holds before calling P and pass this information to B. The
scheme used is:

@begin{verbatim}
clause :-
  ...,
  C is true if condition A holds,
  P,
  ( C -> check( B ) ; true ),
  ...
@end{verbatim}

@noindent
This is exactly what these two predicates do:

@begin{itemize}

@item check( CondA , H ). H is bound to 'true' if Cond holds.

@item checkiftrue( H , CondB ). Performs a check( CondB ) if H is
   @tt{true} and an exception is raised only if H is @tt{true} and
   condB does not hold.

@end{itemize}
"

% "This library package can be used either by the user or by CiaoPP
% (see @tt{CiaoPP} manual) run-time transformations. When used by user
% the predicates @pred{check/1}, @pred{checkc/2} and
% @pred{checkiftrue/2} are expected to be used. On the other hand,
% CiaoPP run-time transformacion will generate code for @pred{icheck/2},
% @pred{checkc/4} and @pred{checkiftrue/3}. In fact, CiaoPP run-time
% transformation will generate code for @pred{check/1}, @pred{checkc/3}
% and @pred{checkiftrue/3}, and it will be the run-time package which
% will add the 3rd (location) and 4th argument (module).

% The CiaoPP run-time transformation stores assertions in the
% transformated files, that are referred by the check-point
% predicates. These stored assertions are neccesary because the run-time
% transformation simulates compile-time checking process but in run
% time.

% All predicates raise an exception when the condition fails. This
% exception is caugh by the top-level and is printed depending on the
% run-time pretty printer is loaded in that moment. The conexion between
% run-time checks library and the run-time pretty printer is done via
% @pred{print} predicate (see @index{portray/1})."

% % " This library package allows the use of run-time
% % 	checks for the assertions introduced in a program.

% %         The recommended way of performing @index{run-time checks} of
% % 	predicate assertions in a program is via the Ciao preprocessor
% % 	(see @tt{CiaoPP} manual), which performs the required program
% % 	transformation. However, this package can also be used to
% % 	perform checking of program-point assertions.
% % "
).

:- data parent_goal/2.

:- meta_predicate push_parent_goal( addmodule ).
%:- meta_predicate current_parent_goal( addmodule ).
:- meta_predicate pop_parent_goal( addmodule ).

:- trust pred push_parent_goal( Term , M ) : term * atm

# "Push @var{Term} in the @var{M} module stack.".

push_parent_goal( Term , M ) :-
	asserta_fact( parent_goal( M , Term ) ).
push_parent_goal( Term , M ) :-
	retract_one_fact( parent_goal( M , Term ) ),
	fail.

:- trust pred current_parent_goal( M , Term ) : atm * term

# "@var{Term} is the top of the module @var{M} stack.".

current_parent_goal( M , Term ) :-
	current_fact( parent_goal( M , Term ) ),
	!.
current_parent_goal( _ , unknown ).

:- trust pred pop_parent_goal( Term , M ) : term * atm

# "The top of the module @var{M} stack is removed. @var{Term} is used
  in case of fail.".

pop_parent_goal( Term , M ) :-
	retract_one_fact( parent_goal( M , Term ) ).

pop_parent_goal( Term , M ) :-
	asserta_fact( parent_goal( M , Term ) ),
	fail.

retract_one_fact( Fact ) :-
	retract_fact( Fact ),
	!.

:- trust pred clean_parent_stack( M ) : atm

# "Cleans the module @var{M} stack.".

clean_parent_stack( M ) :-
	retractall_fact( parent_goal( M , _ ) ),
	!.
clean_parent_stack( _ ).

:- regtype boolean/1.

boolean( true  ).
boolean( false ).

:- trust pred checkc( A , B ) : callable * boolean

# "@var{B} is boolean variable that express whether @var{A} had succed.".

% checkc( A , true ) :-
% 	\+ \+ A,
% 	!.
%
checkc( A , Out ) :-
	copy_term( A , AC ),
	AC,
	!,
	(
	    instance( A , AC )
	->
	    Out = true 
	;
	    Out = false
	).
checkc( _A , false ).

:- pred check( X ) : term( X ) + native.
:- trust pred check( A ) : callable

# "@var{A} is executed. If it fails, an exception is raised.".

:- trust pred check( A ) : term.

:- set_prolog_flag( multi_arity_warnings , off ).

% checkc( A ) :-
% 	\+ \+ A,
% 	!.
%
check( A ) :-
	check_internal( A ).
check( A ) :-
	throw( rtcheck( pp_check , A , true ) ).

:- pred check( X , As ) : term( X ) + native.
:- trust pred check( A , As , M ) : (callable(A), term(As), atom(M))

# "@var{A} is executed. If it fails, an exception is raised. @var{As}
  indicated the assertion that failed. @var{As} is a number that is
  used as key in '$saved_assertions'/3 multifile predicate.".

:- trust pred check( A , As , M ) : (term(A),term(As)).

check( CC , AS , M ) :-
	icheck( CC , AS , M ).

%:- set_prolog_flag( multi_arity_warnings , on ).

check_internal( A ) :-
	copy_term( A , AC ),
	AC,
	!,
	instance( A , AC ).

:- trust pred icheck( A , B , M ) : callable * term * atom

# "The same as @pred{check/2} but with error information".


:- multifile '$saved_assertion'/3.


icheck( A , _ErrorInfo , _M ) :-
	check_internal( A ),
	!.

% Case 1: the pp check point comes from an assertion
icheck( _ , as(ErrorInfo,Loc) , M ) :-
%	num( ErrorInfo ),
	'$saved_assertion'( M , ErrorInfo , MessageInfo ),
	!,
	throw( rtcheck( assrt , MessageInfo , Loc ) ).
% Case 2: it is user program check point => we only have the locator
icheck( Goal , Loc , _M ) :-
	%Loc = loc( _ , _ , _ ),
	functor( Loc , loc , _ ),
	!,
	throw( rtcheck( pp_check, Goal, Loc ) ).
% Case 3: None of the above options => ERROR, something was added and
%         we do not consider it here.
icheck( _ , ErrorInfo , M ) :-
	error_message( "Program check point failed. INTERNAL ERROR: " ||
		       "Unconsidered case of error ~q (module ~q)" , 
		       [ ErrorInfo , M ] ).

% :- set_prolog_flag( multi_arity_warnings , on ).


:- trust pred checkiftrue( A , B ) : boolean * callable

# "If @var{Condition} is true, then @var{Post} is executed. If
	@var{Post} fails, then an exception is raised.".

checkiftrue( C , A ) :-
	icheckiftrue( C , A ),
	!.
checkiftrue( true  , A ) :- 
	throw( rtcheck( post , A , true ) ).


:- trust pred checkiftrue( Condition , Post , ErrorInfo , Module ) 
	: boolean * callable * term * atm

# "If @var{Condition} is true, then @var{Post} is
  executed. @var{ErrorInfo} and @var{Module} are used for printing
  error message. These values are got from CiaoPP run-time
  transformation in conjuntion with run-time library package.".

checkiftrue( C , A , _ , _ ) :-
	icheckiftrue( C , A ),
	!.
checkiftrue( true  , _ , as(ErrorInfo,Loc) , M ) :- 
%	num( ErrorInfo ),
	'$saved_assertion'( M , ErrorInfo , MessageInfo ),
	!,
	throw( rtcheck( assrt , MessageInfo , Loc ) ).
checkiftrue( true  , A , ErrorInfo , _ ) :- 
%	ErrorInfo = loc( _ , _ , _ ),
	functor( ErrorInfo , loc , _ ),
	!,
	throw( rtcheck( pp_check , A , ErrorInfo ) ).
checkiftrue( true , _ , ErrorInfo , M ) :-
	error_message( "Program check point failed. INTERNAL ERROR: " ||
		       "Unconsidered case of error ~q (module: ~q)" , 
		       [ ErrorInfo , M ] ),
	throw( rtcheck( pp_check , unknown , loc( unknown , 0 , 0 ) ) ).
	
:- set_prolog_flag( multi_arity_warnings , on ).


:- trust pred icheckiftrue( Condition , Post ) 
	: boolean * callable

# "If @var{Condition} is true, then @var{Post} is executed.".

icheckiftrue( false , _ ).
icheckiftrue( true  , A ) :- 
	check_internal( A ).

% was: (the same as check_internal)
% 	copy_term( A , AC ),
% 	AC,
% 	!,
% 	instance( A , AC ).

:- trust pred inst( A ) : callable.

inst( A ) :-
	copy_term( A , AC ),
	AC,
	!,
	instance( A , AC ).


:- pred check_comp( Condition , CompGoal, CompGoalArg , Head )

# "If @var{Condition} is @tt{true} then the @var{CompGoal} containing
the nested comp predicate calls is called with @var{Head} as
argument. To allow efficient implementation, @var{CompGoalArg} is the
last nested argument of @var{CompGoal}, so unifiying with @var{Head}
we have the comp check, and calling directly to @var{Head} we 
skip the test. An example call could be:

@begin{verbatim}
check_comp(C,not_fails(is_det(A)),A,partition(_1,_2,_3,_4))
@end{verbatim}

so if C is true (it should come from @pred{checkc/2} predicate), then
A is unified with partiton(_1,_2,_3,_4) and
not_fails(is_det(partiton(_1,_2,_3,_4))) is called. Else, just
partiton(_1,_2,_3,_4) is called.".


:- trust pred check_comp( Condition , CompGoal, CompGoalArg , Head ) 
	: boolean * callable * var * callable.

check_comp( true , CompGoal , CompGoalArg , Head ) :-
	!,
	CompGoalArg = Head,
	call( CompGoal ).
check_comp( _ , _CompGoal , _CompGoalArg , Head ) :-
	call( Head ).

%:- meta_predicate rt_det_nf( goal ).
%% it is like rt_det( rt_nf ) == rt_nf( rt_det )
% rt_det_nf(X) :- 
% 	'$metachoice'( C0 ),
% 	X,
% 	'$metachoice'( C1 ),
% 	( C1 == C0 -> ! ; rtcheck( det_nf , nodet , X  ) ).
%
% rt_det_nf(X) :- throw( rtcheck( det_nf , fail , X  ) ).

%rt_det_nf(X) :- rt_det( rt_nf( X ) ).

% det Not Choice Points
rt_det_ncp(X) :- 
	'$metachoice'( C0 ),
	X,
	'$metachoice'( C1 ),
	( C1 == C0 -> true ; throw( rtcheck( det , nodet , X  ) ) ).

rt_nf(X) :- 
	'$metachoice'( C0 ),
	X,
	'$metachoice'( C1 ),
	( C1 == C0 -> ! ; true ).
rt_nf(X) :- throw( rtcheck( nf , fail , X  ) ).

% No choice-points are allowed after returning from the predicate 
rt_det_1c(X) :-
	Solved = solved(no),
	X,
	( 
	    arg(1, Solved, no) 
	-> 
	    true
	; 
	    throw( rtcheck( det , nodet , X  )) % more than one solution!
	),
        % Update without trailing: be careful!
        % (in this case, the operation is safe because we are 
        % writing one-cell terms without dereferencing 
        % chains)
	'$setarg'(1, Solved, yes, off). 

rt_print( rtcheck( det , nodet , X  ) ) :-
	message( error , [ 'Goal ' , X , ' was suppoused to be deterministic' ] ),
	!.
rt_print( rtcheck( nf  , fail  , X  ) ) :-
	message( error , [ 'Goal ' , X , ' was suppoused not to fail' ] ),
	!.
rt_print( rtcheck( pre , PRE  , _X  ) ) :-
	message( error , [ 'Precondition ' , PRE , ' failed' ] ),
	!.
rt_print( rtcheck( post , POST  , _X  ) ) :-
	message( error , [ 'Postcondition ' , POST , ' failed' ] ),
	!.
rt_print( rtcheck( ass , POST  , _X  ) ) :-
	message( error , [ 'Assertion ' , POST , ' failed' ] ),
	!.
rt_print( rtcheck( pp_check , GoalArgs , Loc ) ) :-
	null_dict( NDict ),
	( GoalArgs = '$:'(GoalArgs2) -> true ; GoalArgs2 = GoalArgs ),
       	Assertion = as${ type    => calls
		       , status  => check
		       , head    => program_point_check
		       , compat  => []
		       , call    => [GoalArgs2]
		       , succ    => []
		       , comp    => []
		       , dic     => []
		       , comment => [] },
	Loc = loc( S , LB , LE ),
	GoalLoc = loc${file => S , line_begin => LB , line_end => LE },
	print_xtcheck_error( xtcheck( GoalArgs , NDict , GoalLoc , Assertion ) ),
	!.
rt_print( rtcheck( assrt , Assrt , GoalLoc ) ) :-
        current_parent_goal( _ , Goal ),
	print_xtcheck_error( xtcheck( called( Goal , Goal ) , 
	                              ([],[]) , GoalLoc , Assrt ) ), 
	show_stack( _ ),
	!.
rt_print( X ) :-
	message( error , [ 'Internal error while printing:' , X ] ).

show_stack( M ) :-
	current_fact( parent_goal( M , Goal ) ),
	note_message( "Call to ~w" , [ Goal ] ),
	fail.
show_stack( _ ).

% q(a).
% q(b).

% r(_,a).
% r(_,b).

% p(1).
