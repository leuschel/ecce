 %% freeze.pl  --- freeze/2, frozen/2 implementation based on
 %%                attributed variables
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro & Daniel Cabeza
 %% Created On      : Thu Jul 25 19:23:32 1996
 %% Last Modified By: Manuel Carro
 %% Last Modified On: Thu Aug  1 19:27:17 1996
 %% Update Count    : 37
 %% Status          : OK

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% From Holzbauer's examples
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(attrdecl)).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% freeze(-X, -Goal): delay goal until X is non-variable.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

freeze(X, Goal) :-
        attach_attribute( V, '$frozen_goals'(V,Goal)),
        X = V.

verify_attribute('$frozen_goals'(Var,Goal), Value) :-
        detach_attribute( Var),
        Var = Value, 
        call(Goal).

combine_attributes('$frozen_goals'(V1,G1), '$frozen_goals'(V2,G2)) :-
        detach_attribute( V1),
        detach_attribute( V2),
        V1 = V2,
        attach_attribute( V1, '$frozen_goals'(V1,(G1,G2))).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% frozen(-Var, -Goal): Goal is delayed until Var is bound.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

frozen(Var, Goal):-
        get_attribute(Var, '$frozen_goals'(_, Goal)).
