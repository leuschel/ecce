:- ensure_loaded(tk).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tcl0(R):-                               % Calculate a expression, get
                                        % return result
        tk([nodisplay]), tcl('expr 1 + 8', [], R), tcl(exit).


tcl1(E1, E2, E3):-
        tk([]),
        tk_do_one_event(0, E1),         % No event yet 
        tcl('prolog_event { a list {inside another} list} '),
        tk_do_one_event(0, E2),         % Get the above list
        tk_do_one_event(0, E3),         % And no other event now
        tcl(exit).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


e0:-
        tk([]), 
        tcl('button .b -text {tanto TeXtO} -command exit ; pack .b').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e1:- 
        tk([]), 
        tcl('button .b -text ## -command ##; pack .b', 
            ["{tanto texto}", exit]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Write all events in the window

tcl2:- tcl2(16'1e).

tcl2(Event_Type):-
        tk([]),
        tk_do_one_event(Event_Type, Event),
        tcl('button .b -text {Press to exit} -command {prolog_event exit} ; pack .b;update'),
        tcl2loop(Event_Type, Event).

tcl2loop(_, [exit]):- !, tcl(exit).
tcl2loop(Event_Type, NoExit):-
        write(event = NoExit), nl,
        tk_do_one_event(Event_Type, Event),
        tcl2loop(Event_Type, Event).


% Wait for the button to be pressed

tcl3:-
        tk([]),
        tcl('frame .bot; frame .salir'),
        tcl('frame .bot.bttn1; frame .bot.bttn2'),
        tcl('button .bot.bttn1.b -text {Botón 1} -command {prolog_event {boton 1}}'),
        tcl('button .bot.bttn2.b -text {Botón 2} -command {prolog_event {boton 2}}'),
        tcl('button .salir.b -text {Salir} -command {prolog_event salir}'),
        tcl('pack .bot.bttn1.b; pack .bot.bttn2.b; pack .salir.b'),
        tcl('pack .bot.bttn1 .bot.bttn2 -side left'),
        tcl('pack .bot .salir'),
        tk_next_event(Event),           % Waits for event
        wait_loop(Event).

wait_loop(Event):- 
        Event = ["salir"],
        format("Boton ~s~n", Event),
        tcl(exit).
wait_loop(["boton", X]):-
        format("Boton ~s~n", [X]),
        tk_next_event(Event),
        wait_loop(Event).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tcl4:-                                  % Same as above, with failure,
        tk([]),
        tcl('frame .bot; frame .salir'),
        tcl('frame .bot.bttn1; frame .bot.bttn2'),
        tcl('button .bot.bttn1.b -text {Botón 1} -command {prolog_event {boton 1}}'),
        tcl('button .bot.bttn2.b -text {Botón 2} -command {prolog_event {boton 2}}'),
        tcl('button .salir.b -text {Salir} -command {prolog_event salir}'),
        tcl('pack .bot.bttn1.b; pack .bot.bttn2.b; pack .salir.b'),
        tcl('pack .bot.bttn1 .bot.bttn2 -side left'),
        tcl('pack .bot .salir'),
        repeat,
        tk_get_event(Event),            % Does not wait. Fails if no
                                        % event available 
        (
            Event = ["boton", X] ->
            format("Boton ~s~n", [X]),
            fail
        ;
            Event = ["salir"],
            format("Boton ~s~n", Event)
        ),
        tcl(exit).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e2:- 
        tk([]),
        tcl('frame .bttn'),             % Button frame
        tcl('frame .cnvs'),             % Canvas frame
        tcl('canvas .cnvs.c -width 100 -height 80'),
        tcl('button .bttn.b -text {Press here to exit} -command exit'),
        tcl('pack .cnvs.c'),
        tcl('pack .bttn.b'),
        tcl('pack .bttn .cnvs -side top'),
        square(10, 20, blue),
        square(30,50,yellow),
        square(60,25,red).

square(X,Y,Col):-
        Size = 10,
        X1 is X + Size, 
        Y1 is Y + Size,
        tcl('.cnvs.c create rectangle ## ## ## ## -fill ##',
            [X, Y, X1, Y1, Col]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e3:-
        tk([]),
        tcl('frame .bttn'),             % Button frame
        tcl('frame .cnvs'),             % Canvas frame
        tcl('canvas .cnvs.c -width 400 -height 400'),
        tcl('button .bttn.b -text {Press here to exit} -command exit'),
        tcl('pack .cnvs.c'),
        tcl('pack .bttn.b'),
        tcl('pack .bttn .cnvs -side top'),
        square(200, 210, blue, IdBlue),
        square(270, 230, yellow, IdYellow),
        square(290, 250, red, IdRed),
        move_figures(100, [offset(IdYellow, 2, -1), 
                      offset(IdBlue, -1, -1),
                      offset(IdRed, -2, 1)]),
        move_figures(200, [offset(IdYellow, -2, 1), 
                      offset(IdBlue, 1, 1),
                      offset(IdRed, 2, -1)]),
        tcl(exit).

move_figures(0, _X).
move_figures(N, X):- 
        N > 0,
        move_figures(X),
        N1 is N - 1,
        move_figures(N1, X).

move_figures([]).
move_figures([offset(F,X,Y)|Os]):-
%%        write(offset(F,X,Y)), nl,
        move(F, X, Y),
        move_figures(Os).
        
square(X,Y,Col, Id):-
        Size = 10,
        X1 is X + Size, 
        Y1 is Y + Size,
        tcl('.cnvs.c create rectangle ## ## ## ## -fill ##',
            [X, Y, X1, Y1, Col],
            Id).

move(Id,Xon,Xoff):- 
        tcl('.cnvs.c move ## ## ##; update;', [Id, Xon, Xoff]).
