:- ensure_loaded(tk).

foreign_file('trigo.o', [c_sin,c_cos]).
foreign(c_sin,sin(+float,[-float])).
foreign(c_cos,cos(+float,[-float])).
:- load_foreign_files(['trigo.o'], ['-lm']).

e4:-
        tk([]),
        tcl('frame .fb'),               % button
        tcl('frame .fc'),               % canvas
        tcl('button .fb.q -text "Quit" -command exit'),
        tcl('canvas .fc.s -width 400 -height 400'),
        tcl('pack .fc.s; pack .fb.q; pack .fb .fc'),
        square('.fc.s', 150,350, blue, IdBlue),
        square('.fc.s', 150,300, red, IdRed),
        square('.fc.s', 200, 250, yellow,IdYellow),
        delta_ellipse(coord(100,50), 75,Cred),
        delta_ellipse(coord(50,150),100,Cblue),
        delta_ellipse(coord(80,110), 80, Cyellow),
        move_squares(Cblue,Cred,Cyellow,Cblue,Cred,Cyellow,IdBlue,IdRed,IdYellow,'.fc.s').

move_squares([],Cred,Cyellow,Cbl,Crd,Cyw,Blue,Red,Yellow,W):-
        move_squares(Cbl,Cred,Cyellow,Cbl,Crd,Cyw,Blue,Red,Yellow,W).
move_squares(Cblue,[],Cyellow,Cbl,Crd,Cyw,Blue,Red,Yellow,W):-
                move_squares(Cblue,Crd,Cyellow,Cbl,Crd,Cyw,Blue,Red,Yellow,W).
move_squares(Cblue,Cred,[],Cbl,Crd,Cyw,Blue,Red,Yellow,W):-
                move_squares(Cblue,Cred,Cyw,Cbl,Crd,Cyw,Blue,Red,Yellow,W).
move_squares([coord(Xb,Yb)|Cbl],[coord(Xr,Yr)|Crd],[coord(Xy,Yy)|Cyw],
        Cblue,Cred,Cyellow,Blue,Red,Yellow,W):-
 move(Blue,Xb,Yb,W),
 move(Red,Xr,Yr,W),
 move(Yellow,Xy,Yy,W),
 move_squares(Cbl,Crd,Cyw,Cblue,Cred,Cyellow,Blue,Red,Yellow,W).

square(Where, X,Y,Col, Id):-
        Size = 10,
        X1 is X + Size, 
        Y1 is Y + Size,
        tcl("## create rectangle ## ## ## ## -fill ##",  %% Strings also work..
            [Where, X, Y, X1, Y1, Col], Id).

move(Id,Xon,Xoff, Where):- 
        tcl('## move ## ## ##; update;', [Where, Id, Xon, Xoff]).

delta_ellipse(Diameters, Slices, Deltas):-
        pi(Pi),
        Dp is 2*Pi,
        point_ellipse(Diameters, Dp, This_Point),
        ellipse(Slices, Slices, Diameters, Pi, This_Point, Deltas).

pi(3.141592).

ellipse(Slice, _Slices, _Diameters, _Pi, _Prev, []):- Slice = 0.
ellipse(Slice, Slices, Diameters, Pi, Previous, [Delta|Deltas]):-
        Theta is (2*Pi*Slice)/Slices,
        point_ellipse(Diameters, Theta, Point),
        delta(Previous, Point, Delta),
        NewSlice is Slice - 1,
        ellipse(NewSlice, Slices, Diameters, Pi, Point, Deltas).


point_ellipse(coord(Dx, Dy), Ang, coord(Px, Py)):-
        sin(Ang, SinAng),
        cos(Ang, CosAng),
        Px is Dx*SinAng,
        Py is Dy*CosAng.
        

delta(coord(X,Y),coord(X1,Y1),coord(X2,Y2)):-
        X2 is X1 - X,
        Y2 is Y1 - Y.
