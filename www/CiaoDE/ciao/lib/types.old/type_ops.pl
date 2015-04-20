%% Syntactic sugar for types: ops and new_declarations.

:- new_declaration(type/1,on).
:- new_declaration(type/2,on).
   %% Actually, typedef is just syntactic sugar, so it will never 
   %% even get a chance to reach the itf:
:- new_declaration(typedef/1,on).  
:- new_declaration(typedef/2,on).  

:- op(1190, fx,(typedef)).   :- op(1190,xfx,(typedef)).
:- op(25,   fy,(^)).
:- op(1180,xfx,(::=)).
%% Also in assertions???
:- op(1150, fx,(type)).      :- op(1150,xfx,(type)).
