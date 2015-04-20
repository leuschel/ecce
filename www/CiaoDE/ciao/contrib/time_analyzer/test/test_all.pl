:- module( test_all , _ , _ ).


:- use_module( library(tester) , [run_tester/10] ).
:- use_module( library(lists) ).
:- use_module( library(write) ).

:- use_module(library(filenames)).

:- use_module( library( compiler ) ).

:- use_module(engine(internals), [term_to_meta/2,module_concat/3]).

init_func :-
	write( 'Starting the test\n' ).


tester_func( X ) :-
	write( 'Running test ' ) , write( X ) , nl ,
	(unload( X ) -> true ; true ),
	use_module( X ),
	get_module(X , Y),
	Y:main,
	unload( X ).

get_module( Path , Module ) :-
	no_path_file_name( Path , File ),
	(atom_concat( Module , '.pl' , File )
	-> true ; Module = File ).


checker_func( _ ).


end_func.


main :-
	L = [ 
		'1/ta1' ,
		'2/ta2' ,
		'3/ta3' ,
		'4/ta4' ,
		'5/ta5' ,
		'6/ta6'
	    ],
	      
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
		      slider( 'Time analizer test: ' )
		  ),

	 length( L , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '%' ] ).
	

main :- 
	message( note , [ 'Somthing was wrong' ] ).
