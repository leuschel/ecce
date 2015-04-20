cross(N):-
    (N > 5 -> 
     cross(N,1,0,1) % an additional space for each number < 10.
    ;
     cross(N,1,0,0)).

% cross(Size,Starting_from,Shift,Additional_space).
cross(0,_,_,_).
cross(1,M,N,Spc):-
    spaces(N),
    display(M),nl.

cross(Size,Sf,Shift,Spc):-
    spaces(Shift),
    display(Sf),
    Size_next is Size - 2,
    inner_spaces(Size_next,Sf,Inner,Spc),
    spaces(Inner),
    Sf1 is Sf + 1,
    display(Sf1),nl,
    % calling inner square.
    Sf_next is Sf + 4,
    Shift_next is Shift + 2 + Spc,
    cross(Size_next,Sf_next,Shift_next,Spc),
    % last row.
    spaces(Shift),
    Sf3 is Sf + 3,
    display(Sf3),
    spaces(Inner),
    Sf2 is Sf + 2,
    display(Sf2),nl.

inner_spaces(Size,Sf,Inner,Spc):-
     Inner is Size*(2+Spc)+1.

spaces(0).
spaces(N):-
    display(' '),N1 is N-1,spaces(N1).
