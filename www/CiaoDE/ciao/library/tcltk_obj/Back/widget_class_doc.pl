%%---------------------------------------------------------------------
%%
%% WIDGET CLASS
%%
%%---------------------------------------------------------------------

:- module(widget_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- use_module(library('tcltk_obj/window_class')).



%%---------------------------------------------------------------------
%% TEXT
%%---------------------------------------------------------------------

text('').

:- export(set_text/1).

%%------------------------------------------------------------------------
:- pred set_text(+Text) :: atom
        # "Sets the @var{Text} to be displayed in the widget.".
%%------------------------------------------------------------------------
set_text(Text) :-
        atom(Text),
        set_fact(text(Text)),
        notify_changes.

:- export(get_text/1).

%%------------------------------------------------------------------------
:- pred get_text(-Text) :: atom
        # "@var{Text} which is displayed in the widget.".
%%------------------------------------------------------------------------
get_text(Text) :-
        text(Text).

%%---------------------------------------------------------------------
%% FONT
%%---------------------------------------------------------------------


font('normal').

:- export(set_font/1).

%%------------------------------------------------------------------------
:- pred set_font(+Font) :: atom
        # "Sets the @var{Font} of the widget's text.".
%%------------------------------------------------------------------------
set_font(Font) :-
        atom(Font),
        set_fact(font(Font)),
        notify_changes.

:- export(get_font/1).

%%------------------------------------------------------------------------
:- pred get_font(-Font) :: atom
        # "Gets the @var{Font} of the widget's text.".
%%------------------------------------------------------------------------
get_font(Font) :-
        font(Font).

%%---------------------------------------------------------------------
%% BACKGROUND
%%---------------------------------------------------------------------


background('gray').

:- export(set_background/1).

%%------------------------------------------------------------------------
:- pred set_background(+Background) :: atom
        # "Sets the @var{Background} color. Default to gray.".
%%------------------------------------------------------------------------
set_background(Bg) :-
        atom(Bg),
        set_fact(background(Bg)),
        notify_changes.

:- export(get_background/1).

%%------------------------------------------------------------------------
:- pred get_background(-Background) :: atom
        # "Gets the @var{Background} color.".
%%------------------------------------------------------------------------
get_background(Bg) :-
        background(Bg).

%%---------------------------------------------------------------------
%% BORDERWIDTH
%%---------------------------------------------------------------------


borderwidth('2').

:- export(set_borderwidth/1).

%%------------------------------------------------------------------------
:- pred set_borderwidth(+BorderWidth) :: atom
        # "Sets the width's border (@var{Borderwidth}). Default to 2.".
%%------------------------------------------------------------------------
set_borderwidth(Bw) :-
        atom(Bw),
        set_fact(borderwidth(Bw)),
        notify_changes.

:- export(get_borderwidth/1).

%%------------------------------------------------------------------------
:- pred get_borderwidth(-BorderWidth) :: atom
        # "Gets the width's border (@var{Borderwidth}).".
%%------------------------------------------------------------------------
get_borderwidth(Bw) :-
        borderwidth(Bw).

%%---------------------------------------------------------------------
%% FOREGROUND
%%---------------------------------------------------------------------


foreground('black').

:- export(set_foreground/1).

%%------------------------------------------------------------------------
:- pred set_foreground(+Foreground) :: atom
        # "Sets the @var{Foreground} color. Default to black".
%%------------------------------------------------------------------------
set_foreground(Fg) :-
        atom(Fg),
        set_fact(foreground(Fg)),
        notify_changes.

:- export(get_foreground/1).

%%------------------------------------------------------------------------
:- pred get_foreground(-Foreground) :: atom
        # "Gets the @var{Foreground} color.".
%%------------------------------------------------------------------------
get_foreground(Fg) :-
        foreground(Fg).

%%---------------------------------------------------------------------
%% HIGHLIGHTBACKGROUND
%%---------------------------------------------------------------------


highlightbackground('white').

:- export(set_highlightbackground/1).

%%------------------------------------------------------------------------
:- pred set_highlightbackground(+Color) :: atom
        # "@var{Color} specifies the highlight background color. Default to white".
%%------------------------------------------------------------------------
set_highlightbackground(Highlightb) :-
        atom(Highlightb),
        set_fact(highlightbackground(Highlightb)),
        notify_changes.

:- export(get_highlightbackground/1).

%%------------------------------------------------------------------------
:- pred get_highlightbackground(-Color) :: atom
        # "Gets the @var{Color} of the highlight background.".
%%------------------------------------------------------------------------
get_highlightbackground(Highlightb) :-
        highlightbackground(Highlightb).

%%---------------------------------------------------------------------
%% HIGHLIGHTBACKGROUND
%%---------------------------------------------------------------------


highlightcolor('white').

:- export(set_highlightcolor/1).

%%------------------------------------------------------------------------
:- pred set_highlightcolor(+Color) :: atom
        # "@var{Color} specifies the highlight color. Default to white".
%%------------------------------------------------------------------------
set_highlightcolor(Highlightb) :-
        atom(Highlightb),
        set_fact(highlightcolor(Highlightb)),
        notify_changes.

:- export(get_highlightcolor/1).

%%------------------------------------------------------------------------
:- pred get_highlightcolor(-Color) :: atom
        # "Gets the @var{Color} of the highlight.".
%%------------------------------------------------------------------------
get_highlightcolor(Highlightb) :-
        highlightcolor(Highlightb).

%%---------------------------------------------------------------------
%% WIDTH
%%---------------------------------------------------------------------


width('0').

:- export(set_width/1).

%%------------------------------------------------------------------------
:- pred set_width(+Width) :: atom
        # "Specifies the @var{Width} for the widget. Default to 0".
%%------------------------------------------------------------------------
set_width(Width) :-
        atom(Width),
        set_fact(width(Width)),
        notify_changes.

:- export(get_width/1).

%%------------------------------------------------------------------------
:- pred get_width(+Width) :: atom
        # "Gets the @var{Width} specified for the widget.".
%%------------------------------------------------------------------------
get_width(Width) :-
        width(Width).

%%---------------------------------------------------------------------
%% RELIEF
%%---------------------------------------------------------------------


relief('sunken').

:- export(set_relief/1).

%%------------------------------------------------------------------------
:- pred set_relief(+Relief) :: atom
        # "Specifies a desired @var{Relief} for the widget. Default to sunken".
%%------------------------------------------------------------------------
set_relief(Relief) :-
        atom(Relief),
        set_fact(relief(Relief)),
        notify_changes.

:- export(get_relief/1).

%%------------------------------------------------------------------------
:- pred get_relief(-Relief) :: atom
        # "Gets the @var{Relief} of the widget.".
%%------------------------------------------------------------------------
get_relief(Relief) :-
        relief(Relief).

%%---------------------------------------------------------------------
%% SIDE
%%---------------------------------------------------------------------


side('top').

:- export(set_side/1).

%%---------------------------------------------------------------------
:- pred set_side(+Side) :: atom
        # "Specifies which @var{Side} of the master, the slave(s) will be packed against. Must be left, right, top or bottom. Defaults to top".
%%---------------------------------------------------------------------
set_side(Side) :-
        atom(Side),
        set_fact(side(Side)),
        notify_changes.

:- export(get_side/1).

%%---------------------------------------------------------------------
:- pred get_side(-Side) :: atom
        # "Gets the @var{Side} of the canvas.".
%%---------------------------------------------------------------------
get_side(Side) :-
        side(Side).

%%---------------------------------------------------------------------
%% EXPAND
%%---------------------------------------------------------------------


expand('0').

:- export(set_expand/1).

%%---------------------------------------------------------------------
:- pred set_expand(+Value) :: atom
        # "Specifies whether the slaves should be expanded to consume extra space in their master. @var{Value} may have any proper boolean value, such as 1 or no. Defaults to 0".
%%---------------------------------------------------------------------
set_expand(Expand) :-
        atom(Expand),
        set_fact(expand(Expand)),
        notify_changes.

:- export(get_expand/1).

%%---------------------------------------------------------------------
:- pred get_expand(-Value) :: atom
        # "Gets the boolean @var{Value} which indicates if the slaves should be expanded or no.".
%%---------------------------------------------------------------------
get_expand(Expand) :-
        expand(Expand).

%%---------------------------------------------------------------------
%% FILL
%%---------------------------------------------------------------------


fill('none').

:- export(set_fill/1).

%%---------------------------------------------------------------------
:- pred set_fill(+Option) :: atom
        # "If a slave's parcel is larger than its requested dimensions, this option may be used to stretch the slave. @var{Option} must have one of the following values: none ( this is the default), x, y, both".
%%---------------------------------------------------------------------
set_fill(Fill) :-
        atom(Fill),
        set_fact(fill(Fill)),
        notify_changes.

:- export(get_fill/1).

%%---------------------------------------------------------------------
:- pred get_fill(-Option) :: atom
        # "Gets the fill @var{Option} of the canvas".
%%---------------------------------------------------------------------
get_fill(Fill) :-
        fill(Fill).

%%---------------------------------------------------------------------
%% PADX
%%---------------------------------------------------------------------


padx('0').

:- export(set_padx/1).

%%---------------------------------------------------------------------
:- pred set_padx(+Amount) :: atom
        # "@var{Amount} specifies how much horizontal external padding to leave on each side of the slave(s). Amount defaults to 0".
%%---------------------------------------------------------------------
set_padx(Padx) :-
        atom(Padx),
        set_fact(padx(Padx)),
        notify_changes.

:- export(get_padx/1).

%%---------------------------------------------------------------------
:- pred get_padx(-Amount) :: atom
        # "Gets the @var{Amount} which specifies how much horizontal external padding to leave on each side of the slaves.".
%%---------------------------------------------------------------------
get_padx(Padx) :-
        padx(Padx).

%%---------------------------------------------------------------------
%% PADY
%%---------------------------------------------------------------------


pady('2').

:- export(set_pady/1).

%%---------------------------------------------------------------------
:- pred set_pady(+Amount) :: atom
        # "@var{Amount} specifies how much vertical external padding to leave on each side of the slave(s). Amount defaults to 0".
%%---------------------------------------------------------------------
set_pady(Pady) :-
        atom(Pady),
        set_fact(pady(Pady)),
        notify_changes.

:- export(get_pady/1).

%%---------------------------------------------------------------------
:- pred get_pady(-Amount) :: atom
        # "Gets the @var{Amount} which specifies how much vertical external padding to leave on each side of the slaves.".
%%---------------------------------------------------------------------
get_pady(Pady) :-
        pady(Pady).

%%---------------------------------------------------------------------
%% ROW
%%---------------------------------------------------------------------


row('0').

:- export(set_row/1).

%%---------------------------------------------------------------------
:- pred set_row(+Row) :: int
        # "Indicates the @var{Row} in which the widget should be allocated.".
%%---------------------------------------------------------------------
set_row(Row) :-
        num(Row),
        set_fact(row(Row)),
        notify_changes.

:- export(get_row/1).
 
%%---------------------------------------------------------------------
:- pred get_row(-Row) :: int
        # "Gets the @var{Row} in which the widget is allocated.".
%%---------------------------------------------------------------------
get_row(Row) :-
        row(Row).


rowspan(1).

:- export(set_rowspan/1).

%%---------------------------------------------------------------------
:- pred set_rowspan(+Row) :: int
        # "Indicates the number of @var{Row} which are going to be occupied in the grid.".
%%---------------------------------------------------------------------
set_rowspan(Rowspan) :-
        num(Rowspan),
        set_fact(rowspan(Rowspan)),
        notify_changes.

:- export(get_rowspan/1).
 
%%---------------------------------------------------------------------
:- pred get_rowspan(-Row) :: int
        # "It gets the number of @var{Row} which are occupied by the widget in the grid.".
%%---------------------------------------------------------------------
get_rowspan(Rowspan) :-
        rowspan(Rowspan).

%%---------------------------------------------------------------------
%% COLUMN
%%---------------------------------------------------------------------


column('0').

:- export(set_column/1).

%%---------------------------------------------------------------------
:- pred set_column(+Column) :: int
        # "Indicates the @var{Column} in which the widget should be allocated.".
%%---------------------------------------------------------------------
set_column(Column) :-
        num(Column),
        set_fact(column(Column)),
        notify_changes.

:- export(get_column/1).

%%---------------------------------------------------------------------
:- pred get_column(-Column) :: int
        # "Gets the @var{Column} in which the widget is allocated.".
%%---------------------------------------------------------------------
get_column(Column) :-
        column(Column).


columnspan(1).

:- export(set_columnspan/1).

%%---------------------------------------------------------------------
:- pred set_columnspan(+Column) :: int
        # "Indicates the number of @var{Column} which are going to be occupied in the grid.".
%%---------------------------------------------------------------------
set_columnspan(Columnspan) :-
        num(Columnspan),
        set_fact(columnspan(Columnspan)),
        notify_changes.

:- export(get_columnspan/1).

%%---------------------------------------------------------------------
:- pred get_columnspan(-Column) :: int
        # "Gets the number of @var{Column} which are occupied by the widget in the grid.".
%%---------------------------------------------------------------------
get_columnspan(Columnspan) :-
        columnspan(Columnspan).

%%---------------------------------------------------------------------
%% EVENT TYPE
%%---------------------------------------------------------------------


event_type('').

:- export(set_event_type/1).

%%---------------------------------------------------------------------
:- pred set_event_type(+EventType) :: atom
        # "The event @var{EventType} is going to be manage by the interface.".
%%---------------------------------------------------------------------
set_event_type(EventT) :-
        atom(EventT),
        set_fact(event_type(EventT)),
        notify_changes.

:- export(get_event_type/1).

%%---------------------------------------------------------------------
:- pred get_event_type(-EventType) :: atom
        # "Gets the event @var{EventType} is going to be manage by the interface.".
%%---------------------------------------------------------------------
get_event_type(EventT) :-
        event_type(EventT).

%%---------------------------------------------------------------------
%% ACTION
%%---------------------------------------------------------------------


action('').

:- export(set_action/3).
:- export(set_action/1).

%%---------------------------------------------------------------------
:- pred set_action(+Term) :: atom
        # "@var{Term} is going to be associated to the action of the object indicated with the operacion @em{set_event_type}.".
%%---------------------------------------------------------------------
set_action(Predicate) :-
        atom(Predicate),
        set_fact(action([prolog1,dq(write(execute(Predicate)))])),
        notify_changes.

%%---------------------------------------------------------------------
:- pred set_action(+Input, +Output, +Term) :: atom * atom * atom
        # "Executes @var{Term} with @var{Input} value and @var{Output} variable.".
%%---------------------------------------------------------------------
set_action(Input,Output,Predicate) :-
        atom(Predicate),
        atom_concat('$',Input,Input1),
        atom_concat('$prolog_variables(',Output,Output1),
        atom_concat(Output1,')',Output2),
        set_fact(action([set,Input,Input1,'\n',
                            prolog_one_event,dq(write(execute(Predicate))),'\n',
                            set,Output,Output2])),
%       set_fact(predicate([prolog_one_event,dq(write(execute(Predicate)))])),
        notify_changes.

:- export(get_action/1).

%%---------------------------------------------------------------------
:- pred get_action(-Term) :: atom
        # "@var{Term} is associated to the action of the object indicated with the operacion @em{set_event_type}.".
%%---------------------------------------------------------------------
get_action(Predicate) :-
        action(Predicate).

%%---------------------------------------------------------------------
%% VARIABLES
%%---------------------------------------------------------------------


variables('').
:- comment(hide,set_variables/1).

:- export(set_variables/1).

set_variables(Variable) :-
%       display('Variables'),nl,
        atom(Variable),
        atom_concat('$',Variable,Variable1),
%       display(Variable1),nl,
        set_fact(variables([set,Variable,Variable1])),
        notify_changes.


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------


:- export([tcl_name/1,creation_options/1,creation_position/1,creation_position_grid/1,creation_bind/1]).

:- comment(hide,tcl_name/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Name) :: atom
        # "@var{Name} is the command to create widget.".
%%---------------------------------------------------------------------
tcl_name(_) :- fail.

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the widget.".
%%---------------------------------------------------------------------
creation_options([min(text),dq(T),min(font),dq(F),min(background),B,min(borderwidth),O,min(foreground),G,min(width),W,min(highlightbackground),HB,min(highlightcolor),HC,min(relief),R]) :-
        %self(ID), 
        %display(ID),nl,
        text(T),
        font(F),
        background(B),
        borderwidth(O),
        foreground(G),
        width(W),
        highlightbackground(HB),
        highlightcolor(HC),
        relief(R).


%%---------------------------------------------------------------------
:- pred creation_position(-OptionsList) :: list
        # "Creates a list with the options supported by the pack command.".
%%---------------------------------------------------------------------
creation_position([' ',min(side),S,min(expand),E,min(fill),F,min(padx),X,min(pady),Y]) :-
        side(S),
        expand(E),
        fill(F),
        padx(X),
        pady(Y).

%%---------------------------------------------------------------------
:- pred creation_position_grid(-OptionsList) :: list
        # "Creates a list with the options supported by the grid command.".
%%---------------------------------------------------------------------
creation_position_grid([' ',min(row),R,min(rowspan),S,min(column),C,min(columnspan),P]) :-
        row(R),
        rowspan(S),
        column(C),
        columnspan(P).


%%---------------------------------------------------------------------
:- pred creation_bind(-BindList) :: list
        # "Creates a list with the event to be manage and the action associated to this event.".
%%---------------------------------------------------------------------
creation_bind([' ',Eventtype,br(C)]) :-
        event_type(Eventtype),
%display('Antes variables'),nl,
%       variables(V),
%display(V),
        action(C).

:- inheritable(notify_changes/0).

notify_changes:-
        display('En el notify_changes widget'),nl,
        self(Widget),
        owner(AnOwner),
        AnOwner instance_of window_class,
        AnOwner:item_changed(Widget),
        fail.
notify_changes.

:- export([add_owner/1,remove_owner/1]).
:- comment(hide,add_owner/1).
:- comment(hide,remove_owner/1).

add_owner(Owner) :-
        \+ owner(Owner),
        Owner instance_of window_class,
        assertz_fact(owner(Owner)),
        self(Widget),
        Owner:add_item(Widget),
        !.
add_owner(_).


remove_owner(Owner) :-
        retract_fact(owner(Owner)),
        Owner instance_of window_class,
        self(Widget),
        Owner:remove_item(Widget),
        !.

remove_owner(_).

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------


:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

widget_class.  % Not owned

widget_class([]) :- !.

widget_class([AnOwner|Next]) :-
        add_owner(AnOwner),
        !,
        widget_class(Next).

widget_class(AnOwner) :-
        !,
        add_owner(AnOwner).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
        self(Widget),
        retract_fact(owner(AnOwner)),
        AnOwner instance_of window_class,     % Owner is still alive
        AnOwner:remove_item(Widget),
        fail.
