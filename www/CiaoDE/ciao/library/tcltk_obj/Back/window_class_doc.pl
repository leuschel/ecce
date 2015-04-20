%%---------------------------------------------------------------------
%%
%% TCL WINDOW
%%
%%---------------------------------------------------------------------

:- module(window_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- use_class(library('tcltk/examples/interface/widget_class')).
%:- use_class(library('tcltk/examples/interface/menu_class')).
%:- use_class(library('tcltk/examples/interface/canvas_class')).

:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(library(aggregates)).
%:- use_module(library('tcltk/examples/interface/Doc/pruebas')).
:- use_module(engine(internals)).


:- data item/2.
:- data menu/2.
:- data canvas/2.
:- data interp/1.

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).
:- set_prolog_flag(multi_arity_warnings,off).


%%----------------------------------------------------------------------

:- comment(title, "The Tcl/Tk Class Interface").

:- comment(author,"Montserrat Iglesias Urraca").
:- comment(author, "@tt{http://www.clip.dia.fi.upm.es/}").
:- comment(author, "The CLIP Group").
:- comment(author, "Facultad de Inform@'{a}tica").
:- comment(author, "Universidad Polit@'{e}cnica de Madrid").

:- comment(copyright,"@include{Copyright.Manuals}").

:- comment(summary,
        "This section will explain the classes of Tcl/Tk to create an interface. 
        ").

:- comment(module,"The @lib{window_class} contains three clases: widget class, menu class and canvas class. The constructor class is window_class. ").

:- export(widget/1).

:- regtype widget(W) 
        # "@var{W} is a reference to one type of the widget".

widget(_).

:- comment(widget/1,"Each @var{Widget} type is characterized in two ways: 
        first, the form of the create  command
       used  to  create instances of the type;  and second, a set
       of configuration options for items of that type, which may
       be  used  in the create and itemconfigure widget commands.").

:- export(option/1).

:- regtype option(O) # "@var{O} is @em{hidden} if the Widget is not visible or 
        @em{shown} if its visible.".
option(_).

:- export(menu/1).

:- regtype menu(M) # "@var{M} is a reference to one type of the menu.".

:- comment(menu/1,"").

menu(_).

:- export(canvas/1).

:- regtype canvas(C) # "@var{C} is a reference to one type of the canvas.".

:- comment(canvas/1,"").

canvas(_).

%:- export(interp/1).

%%----------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred interp(+TclInterpreter) :: tclInterpreter
        # "@var{TclInterpreter} is the reference to the Interpreter.".
interp(_).
%%----------------------------------------------------------------------
%%------------------------------------------------------------------------
:- export(window_class/0).

:- pred window_class 
        #"Creates a new interpreter, asserting the predicate 
        @em{interp(I)}, and the widgets, menus and canvases objects.".

:- export(window_class/3).

:- pred window_class(+WidgetList,+MenusList,+CanvasList) :: list * list *list

        # "Adds the widgets, menus and canvases in the list to the window object.".
%%------------------------------------------------------------------------
window_class :-
        tcl_new(I),
        asserta_fact(interp(I)),
        tcl_eval(I,[wm,withdraw,'.'],_),
        self(Id),
        X = '.',
        tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_),
        tcl_eval(I,[wm,withdraw,write(X),write(Id)],_).

window_class([],[],[]) :-
        window_class.

window_class([],[],[Canvas|Next]) :-
        display('canvas'),display(Canvas),nl,
        ( add_canvas(Canvas) ; true ),
        !,
        window_class([],[],Next).

window_class([],[Menu|Next],Canvas) :-
        display('menu'),display(Menu),nl,
        ( add_menu(Menu) ; true ),
        !,
        window_class([],Next,Canvas).

window_class([Item|Next],Menu,Canvas) :-
        ( add_item(Item) ; true ),
        !,
        window_class(Next,Menu,Canvas).

:- set_prolog_flag(multi_arity_warnings,on).

%%------------------------------------------------------------------------
:- export(destructor/0).

:- pred destructor

        # "Deletes the widgets, menus and canvases of the window object and the 
           window object.".
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

%:- export(add_item/1).
%:- export(remove_item/1).
%:- export(item_changed/1).

%%------------------------------------------------------------------------
:- pred add_item(+Widget) :: widget

        # "Adds @var{Widget} to the window object.".
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
        # "Removes @var{Widget} from the window object.".
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
        # "Removes @var{Widget} from the window object and shows the new one.".
%%------------------------------------------------------------------------
item_changed(Widget) :-
        display('En el item_changed'),nl,
        hide_item(Widget),
        show_item(Widget).

%%---------------------------------------------------------------------
%% ADD/REMOVE MENUS
%%---------------------------------------------------------------------

%:- export(add_menu/1).
%:- export(remove_menu/1).
%:- export(menu_changed/1).

%%------------------------------------------------------------------------
:- pred add_menu(+Menu) :: menu
        # "Adds @var{Menu} to the window object.".
%%------------------------------------------------------------------------
add_menu(Menu) :-
        display('add_menu'),display(Menu),nl,
        \+ menu(Menu,_),
        Menu instance_of menu_class,
        assertz_fact(menu(Menu,hidden)),
        self(Principal),
        Menu:add_owner(Principal),
        !.
add_menu(_).

%%------------------------------------------------------------------------
:- pred remove_menu(+Menu) :: menu
        # "Removes @var{Menu} from the window object.".
%%------------------------------------------------------------------------
remove_menu(Menu) :-
%       hide_menu(Menu),
        retract_fact(menu(Menu,_)),
        Menu instance_of menu_class,
        self(Principal),
        Menu:remove_owner(Principal),
        !.
remove_menu(_).

%%------------------------------------------------------------------------
:- pred menu_changed(+Menu) :: menu
        # "Removes @var{Menu} from the window object and show the new one.".
%%------------------------------------------------------------------------
menu_changed(Menu) :-
        display('En el menu_change'),nl,
        hide_menu(Menu),
        show_menu(Menu).
%       hide_menu_entry(Menu),
%       show_menu_entry(Menu).

%%---------------------------------------------------------------------
%% ADD/REMOVE CANVAS
%%---------------------------------------------------------------------

%:- export(add_canvas/1).
%:- export(remove_canvas/1).
%:- export(canvas_changed/1).

%%------------------------------------------------------------------------
:- pred add_canvas(+Canvas) :: canvas
        # "Adds @var{Canvas} to the window object.".
%%------------------------------------------------------------------------
add_canvas(Canvas) :-
        \+ canvas(Canvas,_),
        Canvas instance_of canvas_class,
        assertz_fact(canvas(Canvas,hidden)),
        self(Principal),
        Canvas:add_owner(Principal),
        !.
add_canvas(_).

%%------------------------------------------------------------------------
:- pred remove_canvas(+Canvas) :: canvas
        # "Removes @var{Canvas} from the window object.".
%%------------------------------------------------------------------------
remove_canvas(Canvas) :-
        hide_canvas(Canvas),
        retract_fact(canvas(Canvas,_)),
        Canvas instance_of canvas_class,
        self(Principal),
        Canvas:remove_owner(Principal),
        !.
remove_canvas(_).

%%------------------------------------------------------------------------
:- pred canvas_changed(+Canvas) :: canvas
        # "Removes @var{Canvas} from the window object and show the new one.".
%%------------------------------------------------------------------------
canvas_changed(Canvas) :-
        hide_canvas(Canvas),
        show_canvas(Canvas).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE WIDGET
%%---------------------------------------------------------------------

:- export(show/0).

%%------------------------------------------------------------------------
:- pred show
        # "Shows widgets, menus and canvas of the window object.".
%%------------------------------------------------------------------------
show :-
%       display('********En el show window'),nl,
        self(Id),
        interp(I),
        X = '.',
%       tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_), 
        tcl_eval(I,[wm,deiconify,write(X),write(Id)],_), 
%       item(Widget,hidden),
%       display('__________w'),display(Widget),nl,
        show_item(Widget),
%       menu(Menu,hidden),
%       display('__________m'),display(Menu),nl,
        show_menu(Menu),
%       canvas(Canvas,hidden),
%       display('__________c'),display(Canvas),nl,
        show_canvas(Canvas),
        fail.

show.

%:- export(hide/0).

%%------------------------------------------------------------------------
:- pred hide
        # "Removes widgets, menus and canvas from the window object.".
%%------------------------------------------------------------------------
hide :-
        item(Widget,shown),
        hide_item(Widget),
        menu(Menu,shown),
        hide_menu(Menu),
        canvas(Canvas,shown),
        hide_canvas(Canvas),
        fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

%:- export(show_item/1).

%%------------------------------------------------------------------------
:- pred show_item(+Widget) :: widget
        # "Shows @var{Widget} into the window object.".
%%------------------------------------------------------------------------
show_item(Widget) :-
        item(Widget,hidden),
        display('***************En el show item'),nl,
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
        display('Antes del position***'),nl,
        Widget:creation_position(Post),
%       Widget:creation_position_grid(Post),
        Widget:creation_bind(Bind),
        interp(I),
%       tcl_eval(I,[ItemName,Var|Opts],_),
%       display('En el eval'),nl,
%       display(I),nl,
        tcl_eval(I,[ItemName,write(X),write(ID),write(X)|Opts1],_),
%       tcl_eval(I,[pack,Var|Post],_),
        append([write(Widget)],Post,Post1),
        tcl_eval(I,[pack,write(X),write(ID),write(X)|Post1],_),
%       tcl_eval(I,[grid,write(X),write(ID),write(X)|Post1],_),
%       tcl_eval(I,[bind,Var|Bind],_),
        append([write(Widget)],Bind,Bind1),
        tcl_eval(I,[bind,write(X),write(ID),write(X)|Bind1],_),
        retract_fact(item(Widget,hidden)),
        asserta_fact(item(Widget,shown)),
        fail.

show_item(_).

%:- export(hide_item/1).

%%------------------------------------------------------------------------
:- pred hide_item(+Widget) :: widget
        # "Hides @var{Widget} of the window object.".
%%------------------------------------------------------------------------
hide_item(Widget) :-
        display('*********En el hide item'),nl,
        self(ID),
        retract_fact(item(Widget,shown)),
        !,
%       retract_fact(item(Widget,_)),
        interp(I),
        X = '.',
        tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Widget)],_),
        asserta_fact(item(Widget,hidden)).

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC MENUS
%%---------------------------------------------------------------------

%:- export(show_menu/1).

%%------------------------------------------------------------------------
:- pred show_menu(+Menu) :: menu
        # "Shows @var{Menu} into the window object.".
%%------------------------------------------------------------------------
show_menu(Menu):-
        menu(Menu,hidden),
        Menu instance_of menu_class,
        Menu:tcl_name(ItemName),
        Menu:creation_options(Opts),
        interp(I),
        tcl_eval(I,[ItemName,''|Opts],_),
        retract_fact(menu(Menu,hidden)),
        asserta_fact(menu(Menu,shown)),
        fail.

show_menu(_).

%:- export(show_menu_entry/1).
%show_menu_entry(Menu) :-
%       display('En el show_menu_entry'),nl,
%       menu(Menu,hidden),
%       display('En el show item'),nl,
%       Menu instance_of menu_class,
%       self(ID),
%       Menu:creation_options_entry(Opts),
%       X = '.',
%       append([write(Widget)],Opts,Opts1),
%       Widget:creation_position(Post),
%       Widget:creation_bind(Bind),
%       interp(I),
%       tcl_eval(I,[Opts],_),
%       tcl_eval(I,[pack,Var|Post],_),
%        append([write(Widget)],Post,Post1),
%       tcl_eval(I,[pack,write(X),write(ID),write(X)|Post1],_),
%       tcl_eval(I,[bind,Var|Bind],_),
%        append([write(Widget)],Bind,Bind1),
%       tcl_eval(I,[bind,write(X),write(ID),write(X)|Bind1],_),
%       retract_fact(menu(Menu,hidden)),
%       asserta_fact(item(Menu,shown)).

%:- export(hide_menu_entry/1).
%:- export(hide_menu/1).

%hide_menu_entry(Menu) :-
%       self(ID),
%       retract_fact(menu(Menu,shown)),
%       asserta_fact(menu(Menu,hidden)).

%%------------------------------------------------------------------------
:- pred hide_menu(+Menu) :: menu
        # "Removes @var{Menu} from the window object.".
%%------------------------------------------------------------------------
hide_menu(Menu) :-
%       self(ID),
        retract_fact(menu(Menu,shown)),
        !,
        Menu instance_of menu_class,
        Menu:creation_menu_name(Opts),
        interp(I),
%       X = '.',
%       tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Menu)],_),
        tcl_eval(I,['destroy',''|Opts],_),
        asserta_fact(menu(Menu,hidden)).

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC MENUS
%%---------------------------------------------------------------------

%:- export(show_canvas/1).

%%------------------------------------------------------------------------
:- pred show_canvas(+Canvas) :: canvas
        # "Shows @var{Canvas} into the window object.".
%%------------------------------------------------------------------------
show_canvas(Canvas):-
        display('****************En el show_canvas'),nl,
        canvas(Canvas,hidden),
        Canvas instance_of canvas_class,
%       Canvas:tcl_name(ItemName),
        display('****************En el show_canvas'),nl,
        Canvas:creation_options(Opts),
        display('****************En el show_canvas'),nl,
        Canvas:creation_position(Post),
        interp(I),
        X='.',
        self(ID),
        tcl_eval(I,[canvas,write(X),write(ID),write(X),write(Canvas),' '|Opts],_),
%       tcl_eval(I,[canvas,write(X),write(ID),write(X),write(Canvas)],_),
        tcl_eval(I,[pack,write(X),write(ID),write(X),write(Canvas),' '|Post],_),
        retract_fact(canvas(Canvas,hidden)),
        asserta_fact(canvas(Canvas,shown)),
        fail.

show_canvas(_).

%%------------------------------------------------------------------------
:- pred hide_canvas(+Canvas) :: canvas
        # "Removes @var{Canvas} from the window object.".
%%------------------------------------------------------------------------
hide_canvas(Canvas) :-
        self(ID),
        retract_fact(canvas(Canvas,shown)),
        !,
        display('*******En el hide canvas'),
        interp(I),
        X = '.',
        tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Canvas)],_),
        asserta_fact(canvas(Canvas,hidden)).

%%
%%
%%
%:- export(set_variable/2).

%set_variable(X,Y) :-
%       interp(I),
%       tcl_eval(I,[set,X,write(Y)],_).


%:- export(get_variable/2).

%get_variable(X,Y) :-
%       interp(I),
%       tcl_eval(I,[set,'aux',write(X)],Z),
%       number_codes(Y,Z).

:- export(set_title/1).

%%------------------------------------------------------------------------
:- pred set_title(+X) :: string
        # "@var{X} is the title for the window. The default window title
           is the name of the window.".
%%------------------------------------------------------------------------
set_title(X) :-
        interp(I),
        self(ID),
        Y='.',
        tcl_eval(I,[wm,'title',write(Y),write(ID),'',X],_).

:- export(set_maxsize/2).

%%------------------------------------------------------------------------
:- pred set_maxsize(+X,+Y) :: int * int
        # "@var{X} specifies the maximum width and @var{Y} the maximum height for 
           the window.".
%%------------------------------------------------------------------------
set_maxsize(X,Y) :-
        interp(I),
        self(ID),
        Z='.',
        tcl_eval(I,[wm,'maxsize',write(Z),write(ID),' ',X,Y],_).

:- export(set_minsize/2).

%%------------------------------------------------------------------------
:- pred set_minsize(+X,+Y) :: int * int
        # "@var{X} specifies the minimum width and @var{Y} the minimum height for 
           the window.".
%%------------------------------------------------------------------------
set_minsize(X,Y) :-
        interp(I),
        self(ID),
        Z='.',
        tcl_eval(I,[wm,'minsize',write(Z),write(ID),'',X,Y],_).

:- export(set_withdraw/0).

%%------------------------------------------------------------------------
:- pred set_withdraw
        # "Arranges for window to be withdrawn from  the
              screen.".
%%------------------------------------------------------------------------
set_withdraw :-
        display('En el withdraw'),nl,
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
        # "Waits for a @em{Tcl/Tk} event.".
%%------------------------------------------------------------------------
event_loop :-
        interp(I),
        tk_event_loop(I).
