
%% :- use_module(library(messages),
%% 	[debug_message/1,error_message/1,debug_message/2]).
:- use_module(library(metaterms),[atom_concat/2]).
:- use_module(library(lists),[length/2,member/2]).
:- use_module(library(dynamic),[assert/1,retract/1]).

:- include(library(assertions)).

:- use_module(library(format),[format/2]).
%% debug_message(X,Y) :- format(X,Y).
%% debug_message(X) :- format(X,[]).
debug_message(X,Y).
debug_message(X).

:- op(1200,xfx,['-:']). %% Same as :-/2.
:- op( 700,xfx,[':=']). %% Same as is/2.
:- op( 700,xfx,['::']). %% For higher-order modules

%% Problems:
%% non var pred names (normal assert) does not work
%% 0 arity does not work: ( A :- b ). (but, does it make sense?)
%% ( A(_) :- b(_) ), ( A(_) -: W ). Problems with W... 

:- pred ':-'(Head,Body) # "Higher order predicate definition
   (a.k.a. 'assert'). Undone correctly on backtracking. If used
   repeatedly, new clauses are added to the predicate.

   Example:
   @begin{verbatim}
   main :- 
          ( P(X,Y) :- write(X) ), 
          ( P(X,Y) :- write(Y) ), 
	  P(a,b),
	  fail.
   @end{verbatim}
   writes @tt{ab}.".

':-'(HOHead,Body) :- 
	( HOHead =.. [call,PredName|Args] 
	-> (  var(PredName)
	   -> newsymbol(hopred,PredName),
	      debug_message('[Defining HO')
	   ;  debug_message('[Adding clause to HO') ),
	   length(Args,Arity), % only needed for the debugging message...
	   debug_message(' pred ~w/~w ...]~n',[PredName,Arity]), 
	   Head =.. [PredName|Args]
	;  Head = HOHead,
	   HOHead =.. [PredName|Args],
	   length(Args,Arity), % only needed for the debugging message...
	   debug_message('[Adding clause to normal pred ~w/~w]~n',
	                  [PredName,Arity]) ),
	debug_message('[Asserting (~w)]~n',[(Head :- Body)]), 
	( assert( (Head :- Body) )
	; debug_message('[Retracting ~w ]~n',[(Head :- Body)]),
 	  retract( (Head :- Body) ), !, fail ). %% Cleanup on backtracking!

:- pred '-:'(Head,Body) # "Higher order predicate un-definition
   (a.k.a. 'retract'). Undone correctly on backtracking.  If used
   repeatedly, more clauses are retracted from the predicate.

   Example (cleanup):
   @begin{verbatim}
   main :- ( P(X) :- write(X) ), P(a), ( P(_) -: _ ), P(b).
   @end{verbatim}
   writes @tt{a} and then fails.".

'-:'(HOHead,Body) :- 
	( HOHead =.. [call,PredName|Args] 
	-> (  var(PredName)
	   -> error_message('[Attempt to retract undefined predicate]'),
	      throw(error('Attempt to retract undefined predicate')) %%FIX!!
	   ;  true),
	   length(Args,Arity), % only needed for the debugging message...
	   debug_message('[Retracting clause from HO pred ~w/~w ...]~n',
                          [PredName,Arity]), 
	   Head =.. [PredName|Args]
	;  Head = HOHead,
	   HOHead =.. [PredName|Args],
	   length(Args,Arity), % only needed for the debugging message...
	   debug_message('[Retracting clause from normal pred ~w/~w]~n',
	                  [PredName,Arity]) ),

	debug_message('[Retracting (~w)]~n',[(Head :- Body)]), 
	( retract( (Head :- Body) )
	; debug_message('[Asserting ~w ]~n',[(Head :- Body)]),
 	  assert( (Head :- Body) ), !, fail ). %% Cleanup on backtracking!


:- pred ':='(Var,Value) # "Backtrackable, destructive assigment for
   (predicate) variables.

   Example (cleanup):
   @begin{verbatim}
   main :- X := 1, X := 2, X(Value), write(Value).
   @end{verbatim}
   writes @tt{2}. Or, using the function syntax package:
   @begin{verbatim}
   main :- X := 1, X := 2, write(X~).
   @end{verbatim}
   ".

':='(PredName,Value) :- 
	( var(PredName)
	-> newsymbol(hopred,PredName),
	   debug_message('[Defining',[])
	;  debug_message('[Modifying',[]) 
	),
	debug_message(' mutable variable ~w...]~n',[PredName]), 
	Head =.. [PredName,Value], !,
	debug_message('[Asserting (~w)]~n',[Head]), 
	( asserta( Head )
	; debug_message('[Retracting ~w]~n',[Head]),
 	  retract( Head ), !, fail ). %% Cleanup on backtracking!


:- pred '::'(Instance,Call) # "Implements calls to modules implemented
   via higher-order. Example:
   @begin{verbatim}
   define_module(lists,[member:Member,append:Append]) :-

	( List([]) :- true ),
	( List([_|T]) :- List(T) ),
	
	( Member(X,[X|Y]) :- List(Y) ),
	( Member(X,[_|Y]) :- Member(X,Y) ),

	( Append([],Y,Y) :- List(Y) ),
	( Append([X|Y],Z,[X|W]) :- Append(Y,Z,W) ).
   @end{verbatim}
   writes @tt{2}. Or, using the function syntax package:
   @begin{verbatim}
   main :- X := 1, X := 2, write(X~).
   @end{verbatim}
   ".

%% Calls to higher-order modules.
'::'(M,Call) :-
	Call =.. [MethodName|Args],
	member(MethodName:PredId,M),
	RealCall =.. [call,PredId|Args],
	!,
	RealCall.

%% ------------------------------------------------------------------------
%% Newsymbol should be a builtin...

:- data counter/1.

newsymbol(Base,NewSymbol) :- 
	( counter(_) -> retract(counter(C)) ; C=0 ),
	NC is C+1,
	assert(counter(NC)),
	name(C,Cs),
	atom_codes(CA,Cs),
	atom_concat([Base,CA],NewSymbol).
