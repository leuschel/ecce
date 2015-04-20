:- use_package( argnames ).

:- argnames loc( file , line_begin , line_end , module ).



:- argnames as( ref , status , type , head , compat , call , succ , comp , orig_call, orig_succ, 
                dic , locator , comment , fromwhere ).

:- argnames as_commented( ref , status , type , head , compat , call , succ ,  comp , dic ).

% fromwhere = read , asserted

:- argnames lit( id , key , goal , type , locator ).

:- argnames cls( ref , id , key , head , body , dic , locator ).


% :- regtype as( Ref , Status , Type , Head , Compat , Call , Succ , OrigCall, OrigSucc, Comp , Dic , 
% 	        Locator , Comment , Fromwhere )
% #
% "
% @var{Ref} is internal abstract type (used to delete or consult in DB).
% @var{Status} is the assertion status (check, checkd, true, false...).
% @var{Type} is the assertion type (call,success,entry,exi).
% @var{Call} is the call field.
% @var{Succ} is the success field.
% @var{OrigCall} is the original call field.
% @var{OrigSucc} is the original success field.
% @var{Comp} is the computation field.
% @var{Dic}  is the assertion dictionary.
% @var{locator} is the assertion locator.
% @var{comment} is the assertion comment.
% @var{fromwhere}. Ignore this field (can take the values: read,
% asserted, commented), but maybe will dissapear in future
% implementations.  ".

% :- regtype cls/7.

% cls( _A1 , _A2 , _A3 , _A4 , _A5 , _A6 , _A7 ).


:- regtype t_cls/1.

t_cls( cls( _1 , _2 , _3 , _4 , _5 , _6 , _7 ) ).

:- regtype t_as/1.

t_as( as( _Ref , _Status , _Type , _Head , 
	  _Compat , _Call , _Succ , _Comp , 
	  _Dic , _Locator , _Comment , _Fromwhere ) ).
