:- module(example_dynamic,_,[]).

:- use_package(iso).
:- use_package(persdb).


main([X]):-
%%   Declare the directory associated to the key "db" 
     asserta_fact(persistent_dir(db,'./')),
%%   Declare the predicate bar/1 as dynamic (and data) at run-time  
     data(bar/1),
%%   Declare the predicate bar/1 as persistent at run-time  
     make_persistent(bar/1, db),
     assertz_fact(bar(X)),
     findall(Y, bar(Y), L),
     write(L).    
