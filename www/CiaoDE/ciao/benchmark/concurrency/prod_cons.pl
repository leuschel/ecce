 %% prod_cons.pl -- A simple producer-consumer using the shared database 
 %% Author          : Manuel Carro
 %% Created On      : Tue Nov 25 18:55:16 1997
 %% Last Modified By: MCL
 %% Last Modified On: Tue Mar 21 20:55:47 2000
 %% Update Count    : 101
 %% Status          : Correct.

 %% Two  producer/consumer with 

:- module(
        prod_cons, 
        [
            prod_cons_iter/1,
            prod_cons_fail/1
        ], 
        []).

 %% produced/1 is the item asserted to the data base.  Its first
 %% argument is the item (an integer) produced.  The integer zero
 %% denotes the end of the stream.

:- concurrent produced/1.

 %% Top level call: receives the number of items to produce.  The
 %% consumer and the producer are started in separate threads.  In
 %% this case we are calling an iterative consumer.
prod_cons_iter(Items) :-                                   
        eng_call(consumer, create, create),
        eng_call(producer(Items), create, create).

prod_cons_fail(Items) :-                                   
        eng_call(consumer_fail, create, create),
        eng_call(producer(Items), create, create).


 %% Produce a given number of items; when we reach the 0, signal a
 %% "quit".  We assert the items at the end of the clause so that the
 %% consumer reads them in order.
producer(-1).
producer(X):-
        X > -1,
        X1 is X - 1,
        assertz_fact(produced(X)),
        producer(X1).

 %% Consumer: retracts from the database until the item numbered zero
 %% is reached.  To produce some interaction, it will print a message
 %% every 100 items read.
consumer:-
        retract_fact(produced(This)),
        c(This).

c(0):- display('consumer finished reading'), nl.
c(This):-
        This > -1,
        (This mod 100 =:= 0 -> display(read(This)), nl; true),
        retract_fact(produced(Next)),
        c(Next).


 %% This is a failure-driven consumer.

consumer_fail:-
        retract_fact(produced(This)),
        (This mod 100 =:= 0 -> display(read(This)), nl; true),
        This = 0.
