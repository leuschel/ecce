:- module( m1 , [mf/2] ).

%:- use_module( m2 ).

:- multifile mf/2.

mf( 1 , 2 ).
