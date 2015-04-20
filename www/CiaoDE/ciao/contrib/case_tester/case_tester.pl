%:- module( case_tester , [case_tester/2] , [assertions] ).

:- use_package( assertions ).

:- use_module( library(tester) , [run_tester/10] ).


:- use_module( library( write ) ).
:- use_module( library( lists ) ).
:- use_module( library( terms_check ) , [variant/2] ).
:- use_module( library( aggregates  ) , [findall/3] ).


:- comment( author , "David Trallero Mena" ).
:- comment( module, 

"This module is intented to be a tool to test case-test of a
program. A list of the form:
( variables , predicate , results )
is passes as argument" ).

init_func.
	

tester_func( (PatArg,PArg,SOL) ) :-
	display( 'Executing test: ' ),
	display( PArg ), nl,
	display( 'and comparing with: ' ),
	display( SOL ), nl,
	copy_term( (PatArg , PArg ) , (Pat,P ) ),
	(
	    findall( Pat , P , L )
	->
	    compare( L , SOL )
	;
	    SOL == fail
	).


compare( [A|RA] , [B|RB] ) :-
	variant(A,B),
	!,
	compare( RA , RB ).

compare( [] , [] ).

checker_func( _ ).

end_func.



:- pred case_tester( L , Res )

# "
	L = [
		( (A,B) , main2(A,B) , [(at1(1),1)] ),

		( X     , main3( X        ) , [at2(21,22),at3(1)]),
		( X     , main3( at1(X )  ) , []),
		( X     , main3( at3(X )  ) , [1]),
		( (A,B) , main3( at2(A,B) ) , [ (21,22) ]),

                ( Y     , main4( at2(1), at3(2)   , Y ) , [at2(1),at3(2)] ),
		( Y1    , main4( at1(5), at2(1,2) , Y1) , [at1(5),at2(1,2)] )
	      ],
".


%:- meta_predicate( list( (?,goal,?) ) , ? ).

case_tester( L , Res , Title ) :-
	run_tester(
		      'test.log',
		      'result.log',
		      init_func ,
		      tester_func ,
		      L,
		      checker_func,
		      L,
		      end_func,
		      Res,
		      slider(Title)
		  ).
