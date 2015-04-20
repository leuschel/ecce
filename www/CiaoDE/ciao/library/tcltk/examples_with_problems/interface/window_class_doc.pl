%%---------------------------------------------------------------------
%%
%% TCL WINDOW
%%
%%---------------------------------------------------------------------

:- module(window_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- use_class(library('tcltk/examples/widget_class/widget_class')).

:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(library(aggregates)).

%%----------------------------------------------------------------------

:- comment(title, "The Tcl/Tk Widget Class Interface").

:- comment(author,"Montse Iglesias").

:- comment(copyright,"@include{Copyright.Manuals}").

:- comment(summary,
        "This section will explain how to use the widget class using the 
          tcl/tk library ").

:- comment(module,"The @lib{canvas_class} permits create a widget class using
        tcltk library. The constructor class is window_class. ").

:- regtype wiget(W) # "@var{W} is a reference to one type of the items
        widgets.".

:- comment(widget/1,"Each item type is characterized by two things: 
        first, the form of the create  command
       used  to  create instances of the type;  and second, a set
       of configuration options for items of that type, which may
       be  used  in the create and itemconfigure widget commands.").

widget(_).
:- regtype option(O) # "@var{O} is @em{hidden} if the Widget is not visible or 
        @em{shown} if its visible.".
option(_).
%%----------------------------------------------------------------------

:- data item/2.
:- data interp/1.

:- export(interp/1).
%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred interp(-TclInterpreter) :: tclInterpreter
        # "Creates the TclInterpreter.".
interp(_).
%%----------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).
%%------------------------------------------------------------------------
:- export(window_class/0).

:- pred window_class :: list
        #" Creates a new interpreter, asserting the predicate 
        @em{interp(I)}, and the widget object.".

:- export(window_class/1).

:- pred window_class(+ItemsList) :: list

        # "Adds items of the list to the window object.".
%%------------------------------------------------------------------------

window_class :-
        tcl_new(I),
        asserta_fact(interp(I)),
        tcl_eval(I,[wm,withdraw,'.'],_).
%       self(Id),
%       X = '.',
%       tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_).

window_class([]) :-
        window_class.

window_class([Item|Next]) :-
        ( add_item(Item) ; true ),
        !,
        window_class(Next).

:- set_prolog_flag(multi_arity_warnings,on).
%%------------------------------------------------------------------------
:- export(destructor/0).

:- pred destructor

        # "Deletes the widgets of the window object and the objtect.".
%%------------------------------------------------------------------------

destructor :-
        self(Principal),
        retract_fact(item(Widget,_)),
        Widget :remove_owner(Principal),
        fail.
destructor :-
        interp(I),
        tcl_delete(I),
        retract_fact(interp(_)).

%%---------------------------------------------------------------------
%% ADD/REMOVE ITEMS
%%---------------------------------------------------------------------

:- export(add_item/1).
:- export(remove_item/1).
:- export(item_changed/1).

%%------------------------------------------------------------------------
:- pred add_item(+Widget) :: widget

        # "Adds shapes to the canvas object.".
%%------------------------------------------------------------------------

add_item(Widget) :-
        \+ item(Widget,_),
        Widget instance_of widget_class,
        assertz_fact(item(Widget,hidden)),
        self(Principal),
        Widget:add_owner(Principal),
        !.
add_item(_).

%%------------------------------------------------------------------------
:- pred remove_item(+Widget) :: widget

        # "Removes widget from the window object.".
%%------------------------------------------------------------------------

remove_item(Widget) :-
        hide_item(Widget),
        retract_fact(item(Widget,_)),
        Widget instance_of widget_class,
        self(Principal),
        Widget:remove_owner(Principal),
        !.
remove_item(_).

%%------------------------------------------------------------------------
:- pred item_changed(+Widget) :: widget

        # "Removes widget from the window object and creates a new one.".
%%------------------------------------------------------------------------

item_changed(Widget) :-
        hide_item(Widget),
        show_item(Widget).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE WIDGET
%%---------------------------------------------------------------------

:- export(show/0).
%%------------------------------------------------------------------------
:- pred show

        # "Adds widgets to the window object.".
%%------------------------------------------------------------------------

show :-
        self(Id),
        interp(I),
        X = '.',
        tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_), 
        item(Widget,hidden),
        show_item(Widget),
        fail.
show.

:- export(hide/0).
%%------------------------------------------------------------------------
:- pred hide

        # "Removes widgets from the window object.".
%%------------------------------------------------------------------------

hide :-
        item(Widget,shown),
        hide_item(Widget),
        fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_item/1).
%%------------------------------------------------------------------------
:- pred show_item(+Widget) :: widget

        # "Shows Widget into the window object.".
%%------------------------------------------------------------------------

show_item(Widget) :-
        item(Widget,hidden),
        Widget instance_of widget_class,
        Widget:tcl_name(ItemName),
%       atom_concat('.',ItemName,Name),
%       atom_concat(Name,Pr,Var),
%       atom_concat(Name,Widget,Var),
%        append(".",Widget,Name),
%       atom_codes(Var,Name),
        self(ID),
        Widget:creation_options(Opts),
        X = '.',
        append([write(Widget)],Opts,Opts1),
        Widget:creation_position(Post),
        Widget:creation_bind(Bind),
        interp(I),
%       tcl_eval(I,[ItemName,Var|Opts],_),
        tcl_eval(I,[ItemName,write(X),write(ID),write(X)|Opts1],_),
%       tcl_eval(I,[pack,Var|Post],_),
        append([write(Widget)],Post,Post1),
        tcl_eval(I,[pack,write(X),write(ID),write(X)|Post1],_),
%       tcl_eval(I,[bind,Var|Bind],_),
        append([write(Widget)],Bind,Bind1),
        tcl_eval(I,[bind,write(X),write(ID),write(X)|Bind1],_),
        retract_fact(item(Widget,hidden)),
        asserta_fact(item(Widget,shown)).

:- export(hide_item/1).

%%------------------------------------------------------------------------
:- pred hide_item(+Widget) :: widget

        # "Removes Widget from the window object.".
%%------------------------------------------------------------------------

hide_item(Widget) :-
        self(ID),
        retract_fact(item(Widget,shown)),
        !,
%       retract_fact(item(Widget,_)),
        interp(I),
        X = '.',
        tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Widget)],_),
        asserta_fact(item(Widget,hidden)).

:- export(set_variable/2).
%%------------------------------------------------------------------------
:- pred set_variable(-X,+Y) :: atom * atom

        # "Assigned to X the value of Y (tcltk variable).".
%%------------------------------------------------------------------------

set_variable(X,Y) :-
        interp(I),
        tcl_eval(I,[set,X,write(Y)],_).


:- export(get_variable/2).
%%------------------------------------------------------------------------
:- pred get_variable(-X,+Y) :: atom * int

        # "Puts the value X (tcltk variable) into Y.".
%%------------------------------------------------------------------------

get_variable(X,Y) :-
        interp(I),
        tcl_eval(I,[set,'aux',write(X)],Z),
        number_codes(Y,Z).

:- export(set_title/1).
%%------------------------------------------------------------------------
:- pred set_title(+X) :: string

        # "If  string  is specified, then it will be passed to
              the window manager for use as the title for  window
              (the  window  manager should display this string in
              window's title bar).   In  this  case  the  command
              returns an empty string.  If string isn't specified
              then the command returns the current title for  the
              window.   The  title  for  a window defaults to its
              name.".
%%------------------------------------------------------------------------

set_title(X) :-
        interp(I),
        self(ID),
        Y='.',
        tcl_eval(I,[wm,'title',write(Y),write(ID),'',X],_).
%%------------------------------------------------------------------------
:- pred set_title(+X) :: string

        # "If width and height are specified,  they  give  the
              maximum  permissible  dimensions  for  window.".
%%------------------------------------------------------------------------

:- export(set_maxsize/2).
%%------------------------------------------------------------------------
:- pred set_maxsize(+X,+Y) :: int * int

        # "If width and height are specified,  they  give  the
              maximum  permissible  dimensions  for  window.".
%%------------------------------------------------------------------------

set_maxsize(X,Y) :-
        interp(I),
        self(ID),
        Z='.',
        tcl_eval(I,[wm,'maxsize',write(Z),write(ID),' ',X,Y],_).

:- export(set_minsize/2).
%%------------------------------------------------------------------------
:- pred set_minsize(+X,+Y) :: int * int

        # "If width and height are specified,  they  give  the
              minimum  permissible  dimensions  for  window.".
%%------------------------------------------------------------------------

set_minsize(X,Y) :-
        interp(I),
        self(ID),
        Z='.',
        tcl_eval(I,[wm,'minsize',write(Z),write(ID),'',X,Y],_).

:- export(set_withdraw/0).
%%------------------------------------------------------------------------
:- pred set_withdraw

        # "Arranges  for  window  to  be  withdrawn  from  the
              screen.".
%%------------------------------------------------------------------------

set_withdraw :-
        interp(I),
        self(ID),
        Z='.',
        tcl_eval(I,[wm,'withdraw',write(Z),write(ID)],_).

%%-------------------------------------------------------------------
%% TO USE EVENTS
%%-------------------------------------------------------------------

:- export(event_loop/0).
%%------------------------------------------------------------------------
:- pred event_loop

        # "Waits for a tcltk event.".
%%------------------------------------------------------------------------

event_loop :-
        interp(I),
        tk_event_loop(I).


%%---------------------------------------------------------------------
%% MACROS
%%---------------------------------------------------------------------
                

codify([],"").

codify([''|Next],[C,C|CNext]) :-
        !,
        atom_codes('\"',[C]),
        codify(Next,CNext).

codify([[]|Next],[C,C|CNext]) :-
        !,
        atom_codes('\"',[C]),
        codify(Next,CNext).


codify([X|Next],Str) :-
        atom(X),
        !,
        atom_codes(X,XCodes),
        codify(Next,CNext),
        append(XCodes,CNext,Str).

codify([X|Next],Str) :-
        number(X),
        number_codes(X,XCodes),
        !,
        codify(Next,CNext),
        append(XCodes,CNext,Str).

codify([X|Next],Str) :-
        !,
        codify(Next,CNext),
        append(X,CNext,Str).
