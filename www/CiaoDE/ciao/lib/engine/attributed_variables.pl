
:- multifile 
        verify_attribute/2,
        combine_attributes/2.

%
% called from the emulator
% if there is just one single pending unification
%
uvc(A, B) :- verify_attribute(A, B).
ucc(A, B) :- combine_attributes(A, B). 

% there are more pending unifications (relatively rare)
%
pending_unifications([]).
pending_unifications([ [Var|Val] |Cs]) :-
  pending_unification(Var, Val),
  pending_unifications(Cs).

pending_unification(A, B) :-
  get_attribute(A, Ac),
  get_attribute(B, Bc),
  !,
  combine_attributes(Ac, Bc). 
pending_unification(A, B) :-
  get_attribute(A, Ac),
  !,
  verify_attribute(Ac, B). 
pending_unification(A, B) :-
  get_attribute(B, Bc),
  !,
  verify_attribute(Bc, A).
%
pending_unification(A, A).		% reduced to syntactic unification
