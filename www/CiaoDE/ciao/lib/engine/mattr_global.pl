:- module( mattr_global , [ 
	                         get_attr/2 , set_attr/2 , 
				 
	                         % Back doors for local_attributes
			         bd_get_attr/3 , bd_set_attr/3
			         %, bd_update_attr/3 
			       ] , [] ).

:- use_module( engine( attributes ) ).

:- use_module( library(lists)).

:- meta_predicate set_attr(    ? , addmodule ).
:- meta_predicate get_attr(    ? , addmodule ).
:- meta_predicate update_attr( ? , addmodule ).


% BACK DOOR versions
bd_set_attr(    A , B , C ) :- set_attr( A , B , C ).
bd_get_attr(    A , B , C ) :- get_attr( A , B , C ).
%bd_update_attr( A , B , C ) :- update_attr( A , B , C ).

%get_attr
get_attr( X , ModuleAttr , Name ) :-
	attributes:get_attribute( X , simple_attr( Attr , _ , _ ) ),
	Find =.. [ Name , ModuleAttr ],
	member( Find , Attr ).


%% This is because update of nothing has tu be updatted
% subst( A , [ ]       , [ A ]     ) :- !.

% subst( A , [ P | R ] , [ A | R ] ) :-
% 	functor( A , N , _ ),
% 	functor( P,  N , _ ), 
% 	!.

% subst( A , [ P | R ] , [ P | RM ] ) :-
% 	subst( A , R , RM ).


% constant version
%internal_insert_attr( A , L , [A|L] ).

%order version

% the same => rewrite
internal_insert_attr( A , [P|R] , [A|R] ) :-
	functor( A , N , _ ),
	functor( P,  N , _ ), 
	!.

% less => insert
internal_insert_attr( A , [P|R] , [A,P|R] ) :-
	A @< P, 
	!.

internal_insert_attr( A , [P|R] , [P|LR] ) :-	
	internal_insert_attr( A , R, LR ), 
	!.

% last or empty => add
internal_insert_attr( A , [] ,[A] ).



%update_attr
% update_attr( X , A , Name ) :-
% 	get_attr( X , _ , Name ),
% 	% there is an argument from module
% 	attributes:get_attribute( X , simple_attr( Attr , New , Lock ) ),
% 	NA =.. [Name , A ],
% 	NB = Attr,
% 	subst( NA , NB , NAttr ),!,
% 	attributes:detach_attribute( X ),
% 	attributes:attach_attribute( X , simple_attr( NAttr , New , Lock ) ).



% set_attr: Cannot have an attribute from that module.
% update have to be used instead
% set_attr( X , _ , M ) :-
% 	get_attr( X , _ , M ),!,
% 	fail.

% Things that can happend:
% 1. Module 'Name has never set an attribute to variable X
% 2. Attribute is locked, so 2nd arg has to be changed
% 3. Attribute is no locked, so 1st arg has to be changed
set_attr( X , A , Name ) :-
	(
	    attributes:get_attribute( X , simple_attr( Attr , New , Lock ) )
	->    
	    attributes:detach_attribute( X ),
	    NA =.. [Name , A ],
	    (
		Lock = 0
	    ->
	        % It is unlocked
	        internal_insert_attr( NA , Attr , New_AttList ),
		attributes:attach_attribute( X , simple_attr( New_AttList , New , 0 ) )
	    ;
		% unificating version: 
	        % Variable X is unificating with something, attr are locked
		internal_insert_attr( NA , New , New_AttList ),
		attributes:attach_attribute( X , simple_attr( Attr , New_AttList , Lock ) )
	    )
	;
	    % It is the first time that module 'Name' sets and attribute to this variable
	    NA =.. [Name , A ],
            attributes:attach_attribute( X , simple_attr( [NA] , [] , 0 ) )
	).



prv_lock_attr( X ) :-
	attributes:get_attribute( X, simple_attr( Current , New , N ) ),
	attributes:detach_attribute( X ),
	N1 is N + 1,
	attributes:attach_attribute( X , simple_attr( Current , New , N1 ) ).



prv_unlock_attr( X ) :-
	attributes:get_attribute( X, simple_attr( Current , New , N ) ),
	attributes:detach_attribute( X ),
	N1 is N - 1,
	(
	    N1 = 0 
	-> 
	    attributes:attach_attribute( X , simple_attr( New , [] , 0 ) )
	;
	    (
		N1 > 0
	    ->
	        attributes:attach_attribute( X , simple_attr( Current , New , N1 ) )
	    ;
		fail
	    )
	),
	!.

prv_unlock_attr( X ) :-
	attributes:get_attribute( X, simple_attr( Current , New , N ) ),	
	message( error , ['Internal error: Unlocking attributes: ' , simple_attr( Current , New , N ) ] ).

prv_unlock_attr( X ) :-
	message( error , ['Internal error: Unlocking attributes: Could not get attributes from variable ' , X ] ).

prv_is_locked( X ) :-
	attributes:get_attribute( X, simple_attr( _Current , _New , N ) ),	
	N > 0.


% :- multifile verify_attribute/2, combine_attribute/2.


% %% There is unification iff both variables are NO LOCKED and have attributes
% int_pending_unification( simple_attr( A1 ) , simple_attr( A2 ) , X , Y ) :-
% 	int_combine_attr( X , A1 , Y , A2 ).


% int_combine_attr( [ ] , B , X , Y ) :-
% 	int_verify_attr( X , Y , B ).

% int_combine_attr( B , [ ] , X , Y ) :-
% 	int_verify_attr( Y , X , B ).

% % belongs to the same module
% int_combine_attr( [ A | BR ] , [ B | BR ] , X , Y ) :-
% 	functor( A , N , _ ),
% 	functor( B , N , _ ),
% 	!,
% 	local_combine_attr( A , B , X , Y ).



% int_verify_attr( X , Y , [YA|YAR] ) :- 
% 	verify_attribute( X , Y , YA ),
% 	!,
% 	int_verify_attr( X , Y , YAR ).

% int_verify_attr( X , Y , [_|YAR] ) :- 
% 	!,
% 	int_verify_attr( X , Y , YAR ).
	
% int_verify_attr( _X , _Y , [] ) :- !.


% %% --- TEMPORALY. Till we have _LOCAL_ multiattributes
% local_combine_attr( A , B , X , Y ) :-
% 	arg( 1 , A , XA ),
% 	arg( 1 , B , XB ),
% 	combine_attr( XA , XB ).

% verify_attribute( X , Y , YA ) :-
% 	verify_attribute( X , YA ).
