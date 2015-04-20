:- module(tcltk,[],[assertions,isomodes,regtypes]).

%%----------------------------------------------------------------------
:- comment(title, "The Tcl/Tk interface").

:- comment(author,"Montse Iglesias Urraca").
:- comment(author, "@tt{http://www.clip.dia.fi.upm.es/}").
:- comment(author, "The CLIP Group").
:- comment(author, "Facultad de Inform@'{a}tica").
:- comment(author, "Universidad Polit@'{e}cnica de Madrid").

:- export(tclInterpreter/1).
:- export(tclCommand/1).
:- export(tcl_new/1).
:- export(tcl_eval/3).
:- export(tcl_delete/1).
:- export(tcl_event/3).
%:- export(tcl_event_bueno/3).
:- export(tk_event_loop/1).
:- export(tk_loop/1).
:- export(tk_new/2).
%:- export(tk_new/4).
:- export(tk_next_event/2).


:- use_module(tcltk_low_level).
:- use_module(engine(internals)).
:- use_module(library(write)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3,list_insert/2]).

:- set_prolog_flag(multi_arity_warnings, off).

%%------------------------------------------------------------------------
:- comment(copyright,"@include{Copyright.Manuals}").

:- comment(summary, "This document includes the reference manual of
   the Prolog @index{Tcl/Tk bidirectional interface} implemented in
   Ciao. This library of prolog predicates allows connection between a
   @em{Tcl/Tk}-based @concept{graphical interface} and @em{Prolog}
   programs.").

:- comment(module,"The @lib{tcltk} library package is a bidirectional
   interface to the @em{Tcl} (pronounced Tickle) language and @em{Tk}
   toolkit. Tcl is an interpreter scripting language with many
   extension packages, in particular the graphical interface toolkit
   Tk.  The interaction between both languages is implemented as an
   interface between two processes, a Tcl/Tk process and a Prolog
   process. The approach allows programmers to program both in Tcl/Tk
   and Prolog. 

    @noindent @bf{Prolog - Tcl/Tk interface structure}

    The interface is made up of two parts: a Prolog part and a Tcl/Tk
    part. The Prolog part encodes the requests from a Prolog program
    and sends them to the Tcl/Tk part via a socket. The Tcl/Tk part
    receives from this socket and performs the actions included
    implied by the requests.

    @noindent @bf{Prolog side of the Prolog - Tcl/Tk interface}

    The Prolog side receives the actions to perform in the Tcl/Tk side
    from the user program and sends them to the Tcl/Tk side through
    the socket connection. When the action is finished in the Tcl/Tk
    side, the result is returned to the user program, or the action
    fails if any problem occurs.

    @noindent @bf{Tcl/Tk side of the Prolog - Tcl/Tk interface}

    The Tcl/Tk side waits for requests from the Prolog side, executes
    the Tcl/Tk code sent from the Prolog side. At the same time, the
    Tcl/Tk side handles the events and exceptions raised in the Tcl/Tk
    side, possibly passing on control to the Prolog side. ").

%%------------------------------------------------------------------------

:- regtype tclInterpreter(I) # "@var{I} is a reference to a @em{Tcl}
   interpreter.".

tclInterpreter(_).

%%------------------------------------------------------------------------
:- pred tcl_new(-TclInterpreter) :: tclInterpreter # "Creates a new
   interpreter, initializes it, and returns a handle to it in
   @var{TclInterpreter}.".
%%------------------------------------------------------------------------

tcl_new(I) :-
        tcltk_low_level:new_interp(I).

%%------------------------------------------------------------------------
:- pred tcl_delete(+TclInterpreter) :: tclInterpreter # "Given a
   handle to a Tcl interpreter in variable @var{TclInterpreter}, it
   deletes the interpreter from the system.".
%%------------------------------------------------------------------------

tcl_delete(I) :-
        tcltk_low_level:delete(I).

%%------------------------------------------------------------------------
:- pred tcl_eval(+TclInterpreter,+Command,-Result) 

        :: tclInterpreter * tclCommand * string

        # "Evaluates the commands given in @var{Command} in the Tcl
          interpreter @var{TclInterpreter}. The result will be stored
          as a string in @var{Result}. If there is an error in
          @em{Command} an exception is raised. The error messages will
          be @em{Tcl Exception:} if the error is in the syntax of the
          tcltk code or @em{Prolog Exception:}, if the error is in the
          prolog term.".
%%------------------------------------------------------------------------
%:- export(tcl_eval_result/1).

:- meta_predicate tcl_eval_result(X,addmodule).

:- impl_defined(tcl_eval_result/2).

tcl_eval_result(X,Result,FromModule) :-
        ( member(execute(Goal),[Result]) -> 
          (
              catch(do_call(Goal,FromModule),Error,tcl_loop_exit(X,Error) ),
              tcltk_low_level:send_term(Goal,X)
          )
             ;
            true
        ).

tcl_eval(I,Command,Result) :-
        tcltk_low_level:tcltk_raw_code("prolog_cmd {",I),
        tcltk_low_level:tcltk(Command,I),
        tcltk_low_level:tcltk_raw_code("}",I),
%       tcltk_low_level:receive_term(Result,I), display(Result),nl.
        tcltk_low_level:receive_result(Result,I),
        tcl_eval_result(I,Result).
%       tcltk_low_level:tcltk_error(Result,I).


%%------------------------------------------------------------------------
%:- pred tcl_event(+TclInterpreter,+Command,-Events)
%
%       :: tclInterpreter * tclCommand * list
%
%        # "Do the receive non blocking of the event and terms will be 
%           stored from Tcl by the @em{prolog_event} command as a list 
%           of terms in @em{Events}.". 
%%------------------------------------------------------------------------
%
%tcl_event(I, _Command, EventList):-
%       tcltk_low_level:receive_event(EventList,I).
%
%%------------------------------------------------------------------------
:- pred tcl_event(+TclInterpreter,+Command,-Events)

        :: tclInterpreter * tclCommand * list

        # "Evaluates the commands given in @var{Command} in the Tcl
          interpreter whose handle is provided in
          @var{TclInterpreter}. @var{Events} is a list of terms stored
          from Tcl by @em{prolog_event}. Blocks until there is
          something on the event queue".
%%------------------------------------------------------------------------

%tcl_event(I,[],EventList):-
%       display('In tcl_event1'),nl,
%       tcltk_low_level:tcltk_raw_code("prolog_list_events ",I),
%       tcltk_low_level:receive_list(EventList,I),
%       display('In end of tcl_event'),nl.

tcl_event(I,Command,EventList):-
%       display('In tcl_event1'),nl,
        tcltk_low_level:tcltk(Command,I),
        tcltk_low_level:tcltk_raw_code("prolog_list_events ",I),
        tcltk_low_level:receive_list(EventList,I).
%       display('In end of tcl_event'),nl.

%tcl_event(I,Command,EventList):-
%       tcl_eval(I,Command,_),
%       !,
%       tcltk_low_level:receive_event(EventList,I).

%%------------------------------------------------------------------------
:- comment(tclInterpreter/1,"
        To use Tcl, you must create a @em{Tcl interpreter} object and send 
        commands to it."). 

:- comment(tclCommand/1,"
        A @em{Tcl} command is specified as follows:
@begin{verbatim}
      Command         --> Atom  @{ other than [] @}
                        | Number
                        | chars(PrologString)
                        | write(Term)
                        | format(Fmt,Args)
                        | dq(Command)
                        | br(Command)
                        | sqb(Command)
                        | min(Command)
                        | ListOfCommands
      ListOfCommands  --> []
                        |[Command|ListOfCommands]
@end{verbatim}

where:

@begin{description}

@item{@tt{Atom}} denotes the printed representation of the atom.

@item{@tt{Number}} denotes their printed representations.

@item{@tt{chars(PrologString)}} denotes the string represented by
     @em{PrologString} (a list of character codes).

@item{@tt{write(Term)}} denotes the string that is printed by the
     corresponding built-in predicate.

@item{@tt{format(Term)}} denotes the string that is printed by the
     corresponding built-in predicate.

@item{@tt{dq(Command)}} denotes the string specified by @em{Command},
     enclosed in double quotes.

@item{@tt{br(Command)}} denotes the string specified by @em{Command},
     enclosed in braces.

@item{@tt{sqb(Command)}} denotes the string specified by @em{Command},
     enclosed in square brackets.

@item{@tt{min(Command)}} denotes the string specified by @em{Command},
     immediately preceded by a hyphen.

@item{@tt{ListOfCommands}} denotes the strings denoted by each element,
     separated by spaces.

@end{description}

").
%%------------------------------------------------------------------------
:- regtype tclCommand(C) # "@var{C} is a @em{Tcl} command.".

tclCommand(_).

%%-------------------------------------------------------------
%%  TK
%%-------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred tk_new(+Options,-TclInterpreter)

        :: list * tclInterpreter

        # "Performs basic Tcl and Tk initialization and creates the
          main window of a Tk application.@var{Options} is a list of
          optional elements according to:

@begin{description}

@item{@tt{name(+ApplicationName)}} Sets the Tk main window title to
@var{ApplicationName}. It is also used for communicating between
Tcl/Tk applications via the Tcl @em{send} command. Default name is an
empty string.

@item{@tt{display(+Display)}} Gives the name of the screen on which to
create the main window. Default is normally determined by the
@tt{DISPLAY} environment variable.

@item{@tt{file}} Opens the sript @tt{file}. Commands will not be read
from standard input and the execution returns back to Prolog only
after all windows (and the interpreter) have been deleted.

@end{description}

".
%%------------------------------------------------------------------------

%tk_new([],Interp):-
%       tcl_new(Interp).
tk_new(Options, Interp):-
        tk_options(Options,_,Appname,_,Display,_,File),
        !,
%       tcl_new(Interp),
        tk_new(Interp,Appname,Display,File).


tk_options(Option,_,_,_,_,_,_):-
        var(Option),
        !,
        fail.
tk_options([],App,App,Disp,Disp,File,File).
tk_options([Option|Options],App0,App,Disp0,Disp,File0,File):-
        nonvar(Option),
        tk_option(Option,App0,App1,Disp0,Disp1,File0,File1),
        tk_options(Options,App1,App,Disp1,Disp,File1,File).

tk_option(file(File),App0,App,Disp0,Disp,_,Filename):-
        App=App0, 
        Disp=Disp0,
        Filename = File.
tk_option(name(Name),_,App,Disp0,Disp,File0,File):-
        App=Name,
        Disp=Disp0,
        File=File0.
tk_option(display(Display),App0,App,_,Disp,File0,File):-
        App=App0,
        Disp=Display,
        File=File0.

% Need to put in the second condition of the if case no display
tk_new(Interp,Appname,Display,File):-
        (nonvar(Appname)->atom_concat(' -name ',Appname,Str1);
            atom_concat(' ',' ',Str1)),
        (nonvar(Display)->atom_concat(' -display ',Display,Str2);
%           atom_concat(' -display ',Display,Str2)),
            atom_concat(' ',' ',Str2)),
        (nonvar(File)->atom_concat(' ',File,Str3);
            atom_concat(' ',' ',Str3)),
        atom_concat(Str1,Str2,Str4),
        atom_concat(Str4,Str3,Options),
        tcltk_low_level:new_interp(Interp,Options).

%%------------------------------------------------------------------------
:- pred tk_event_loop(+TclInterpreter) :: tclInterpreter 

# "Waits for an event and executes the goal associated to it. Events
   are stored from Tcl with the @em{prolog} command. The unified term
   is sent to the Tcl interpreter in order to obtain the value of the
   tcl array of @em{prolog_variables}.  If the term received does not
   have the form @tt{execute(Goal)}, the predicate silently exits.  If
   the execution of @var{Goal} raises a Prolog error, the interpreter
   is deleted and an error message is given.".

%%------------------------------------------------------------------------

:- meta_predicate tk_event_loop(addmodule).

tk_event_loop(X,FromModule):-
        tcltk_low_level:receive_event(Event,X),
 ( 
        member(execute(Goal), Event) -> 
        (
            Goal = exit_tk_event_loop ->       % Leave event loop

            tcl_delete(X),
            true
        ;
            (
                Goal = exit_tk_event_loop(G1) ->
                tcltk_low_level:send_term(Goal,X),
                tcl_delete(X),
%               catch(do_call(G1,FromModule),Error,tcl_loop_exit(X,Error)),
                catch(do_call(G1,_FromModule),Error,_), true
            ;
                catch(do_call(Goal,FromModule),Error,tcl_loop_exit(X,Error)),
                tcltk_low_level:send_term(Goal,X),
                tk_event_loop(X,FromModule)
            )
        )
  %   (do_call(Goal,FromModule), tk_event_loop(X,FromModule)) 
 ;      %% Unknown command --- raise exception
        %     throw(unknown_command_in_tk_event_loop(X,FromModule))
        ( 
            Event = [end_of_file] ->
            true
        ;
            unknown_command_in_tk_event_loop(X)
        )
 ).

unknown_command_in_tk_event_loop(X) :-
        write('Prolog exception: '),
        write('The term must have the form execute(goal)'),
        tcl_delete(X),
        fail.

%predicate used to exit closing the main window
tcl_loop_exit(X,E):-
        write('Prolog exception: '),
        write(E),
        tcl_delete(X),
        fail.

do_call(Module:Goal,_FromModule) :-
%       last_module_exp(Module:Goal,goal,FromModule,_,NewGoal),
        module_concat(Module,Goal,NewGoal),
        !,
        '$meta_call'(NewGoal).

%do_call(Goal,FromModule) :-
%       functor(Goal,F,A),
%       imports(FromModule,Defines,F,A),
%       display('imports'),
%       module_concat(Defines,Goal,NewGoal),
%       !,
%       '$meta_call'(NewGoal).

do_call(Goal,FromModule) :-
        module_concat(FromModule,Goal,NewGoal),
        !,
        '$meta_call'(NewGoal).

%%------------------------------------------------------------------------
:- pred tk_next_event(+TclInterpreter,-Event) 
        
        :: tclInterpreter * string 

# "Processes events until there is at least one Prolog event
   associated with @var{TclInterpreter}. @var{Event} is the term
   correspondig to the head of a queue of events stored from Tcl with
   the @em{prolog_event} command.".
%%------------------------------------------------------------------------

tk_next_event(X,Event) :-
        tcl_event(X,[],[Event1|_]),
        Event1 == end_of_event_list,!,
        tk_next_event(X,Event).

tk_next_event(X,Event) :-
        tcl_event(X,[],[Event|_]),
        tcltk_low_level:tcltk_raw_code("prolog_delete_event ",X),
        tcltk_low_level:receive_confirm(_,X).
        
%       display('In tk_next_event'),nl,
%       tcl_event(X,[],[Event1|_]),
%       ( Event1 = end_of_event_list -> tk_next_event(X,_)
%         ; 
%       Event = Event1,
%%      display('After tcl_event '),display(Event),nl,
%       tcltk_low_level:tcltk_raw_code("prolog_delete_event ",X),
%%      tcltk_low_level:delete_item_queue(X),
%%      display('After prolog_delete '),display(Event),nl,
%       tcltk_low_level:receive_confirm(_,X),display(Event),nl).
%%      display('In end of tk'),nl.
%%      tcltk_low_level:tcltk_raw_code("prolog_delete_event ",X).

tk_next_event(_,_).
%%------------------------------------------------------------------------
:- pred tk_loop(+TclInterpreter) 
        
        :: tclInterpreter  
        
        # "Passes control to Tk until all windows are gone.".
%%------------------------------------------------------------------------

:- meta_predicate tk_loop(addmodule).

tk_loop(X,FromModule):-
        tk_next_event(X,Event),
        ( member(end_of_event_list,[Event]) -> 
          ( tk_loop(X,FromModule) )
        ;
            catch(do_call(Event,FromModule),Error,tcl_loop_exit(X,Error)),
            tk_loop(X,FromModule)
        ).
