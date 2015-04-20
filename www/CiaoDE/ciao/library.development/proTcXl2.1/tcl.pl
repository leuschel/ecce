%
% ECLiPSe version of the Prolog Tk interface
%
%	Predicates to invoke the Tcl interpreter from Prolog.
%
% Author: Micha Meier ---- LOTS of changes by MCL
% Date:   August 95
% 
%
%
%
% sccsid("@(#)tcl.pl	1.7          96/02/09").
% sccscr("@(#)  Copyright 1996 Micha Meier ").
%

%% :- coroutine.

expand_tcl(tcl(Script, Vars), Exp):-
        nonvar(Script),
        make_string(Script, String),
        merge_vars(String, Vars, FullCommand),
        Exp = tcl_eval_string(FullCommand, _Result).
expand_tcl(tcl(Script, Vars, Result), Exp):-
        nonvar(Script),
        make_string(Script, String),
        merge_vars(String, Vars, FullCommand),
        Exp = tcl_eval_string(FullCommand, Result).


  %% MCL. How could merge_vars have been so complicated?
  %% merge_vars also takes care of making Tcl strings 

merge_vars(Command, [], Command).       % No more vars
merge_vars([0'#, 0'# | Command], [Var|Vars], NewCommand):-
        make_string(Var, StringVar),
        make_incomplete_string(StringVar, NewCommand, RestCommand),
        merge_vars(Command, Vars, RestCommand).
merge_vars([Char|Command], Vars, [Char|RestCommand]):-
        merge_vars(Command, Vars, RestCommand).


 %% make_string(+AtomOrString, ?String): make a String with the same
 %% name as AtomOrString

make_string(Something, String):-
        (
            Something = [_|_] ->        % Assume it is already a string
            Something = String
        ;
            (
                is_evaluable(Something) ->
                Number is Something,
                name(Number, String)
            ;
                name(Something, String)
            )
        ).

is_evaluable(X):- number(X).
is_evaluable(+X):- is_evaluable(X).
is_evaluable(-X):- is_evaluable(X).
is_evaluable(X+Y):- is_evaluable(X),is_evaluable(Y).
is_evaluable(X-Y):- is_evaluable(X),is_evaluable(Y).
is_evaluable(X*Y):- is_evaluable(X),is_evaluable(Y).
is_evaluable(X/Y):- is_evaluable(X),is_evaluable(Y).


 %% make_atom(+AtomOrString, ?Atom): make an Atom with the same
 %% name as AtomOrString

make_atom(Something, Atom):-
        (
            atomic(Something) ->
            Atom = Something
        ;
            name(Atom, Something)
        ).
            

make_incomplete_string([], _Xs, _Xs).
make_incomplete_string([X|Xs], [X|Ys], Rest):- 
        make_incomplete_string(Xs, Ys, Rest).


tcl(Script):-                           % Single argument
        tcl_eval(Script).

tcl(Script, Vars):-                     % We have vars to substitute
        expand_tcl(tcl(Script, Vars), Exp), 
        call(Exp).                         

tcl(Script, Vars, Result):-             % We want the result as well
        expand_tcl(tcl(Script, Vars, Result), Exp),
        call(Exp).

tcl_eval(S) :- tcl_eval(S, _).

tcl_eval(S, R) :-
        make_atom(S, Atom),
        tcl_eval_string(Atom, R, Err),
        Err = 0.

 %% tcl_eval_string/2 by MCL

tcl_eval_string(S, R) :- 
        make_atom(S, Atom),
        tcl_eval_string(Atom, R, Err),
        Err = 0.
