%%---------------------------------------------------------------------
%%
%% TEXT CLASS
%%
%%---------------------------------------------------------------------

:- module(text_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- inherit_class(library('tcltk/examples/interface/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).
%:- implements(library('class/examples/class/mobile')).

:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

%%---------------------------------------------------------------------
:- export(coord/4).
:- pred coord(+X,+Y) :: int * int
        # "@var{X} and @var{Y} specify the coordinates of a point used to position the text on the display.".
%%---------------------------------------------------------------------
coord(0,0).

:- export([set_point/2]).

%%---------------------------------------------------------------------
:- pred set_point(+X,+Y) :: int * int
        # "@var{X} and @var{Y} change the coordinates of a point used to position the text on the display.".
%%---------------------------------------------------------------------
set_point(X,Y) :-
        number(X),
        number(Y),
        retract_fact(coord(_,_)),
        asserta_fact(coord(X,Y)),
        notify_changes.

:- export([get_point/2]).

%%---------------------------------------------------------------------
:- pred get_point(-X,-Y) :: int * int
        # "Gets @var{X} and @var{Y} values of the point used to position the text on the display.".
%%---------------------------------------------------------------------
set_point(X,Y) :-
        coord(X,Y).

%%---------------------------------------------------------------------
%%  TEXT 
%%---------------------------------------------------------------------


text('').

:- export(set_text/1).

%%---------------------------------------------------------------------
:- pred set_text(+Text) :: atom
        # "@var{Text} specifies the characters to be displayed in the text item. This option defaults to an empty string.".
%%---------------------------------------------------------------------
set_text(Text) :-
        atom(Text),
        set_fact(text(Text)),
        notify_changes.

:- export(get_text/1).

%%---------------------------------------------------------------------
:- pred get_text(-Text) :: atom
        # "Gets the @var{Text} displayed in the text item.".
%%---------------------------------------------------------------------
get_text(Text) :-
        text(Text).
%%---------------------------------------------------------------------
%%  ANCHOR 
%%---------------------------------------------------------------------

anchor('center').

:- export(set_anchor/1).

%%---------------------------------------------------------------------
:- pred set_anchor(+AnchorPos) :: atom
        # "@var{AnchorPos} tells how to position the text relative to the positioning point for the text. This option defaluts to center.".
%%---------------------------------------------------------------------
set_anchor(Anchor) :-
        atom(Anchor),
        set_fact(anchor(Anchor)),
        notify_changes.

:- export(get_anchor/1).

%%---------------------------------------------------------------------
:- pred get_anchor(-AnchorPos) :: atom
        # "Gets the position of the text relative to the positioning point.".
%%---------------------------------------------------------------------
get_anchor(Anchor) :-
        anchor(Anchor).

%%---------------------------------------------------------------------
%%  FONT 
%%---------------------------------------------------------------------

font('arial').

:- export(set_font/1).

%%---------------------------------------------------------------------
:- pred set_font(+Font) :: atom
        # "@var{Font} specifies the font to use for the text item. This option defaluts to arial.".
%%---------------------------------------------------------------------
set_font(Font) :-
        atom(Font),
        set_fact(font(Font)),
        notify_changes.

:- export(get_font/1).

%%---------------------------------------------------------------------
:- pred get_font(-Font) :: atom
        # "Gets the value of the @var{Font} used for the text item.".
%%---------------------------------------------------------------------
get_font(Font) :-
        font(Font).

%%---------------------------------------------------------------------
%%  JUSTIFY 
%%---------------------------------------------------------------------

justify('left').

:- export(set_justify/1).

%%---------------------------------------------------------------------
:- pred set_justify(+How) :: atom
        # "@var{How} specifies how to justify the text within its bounding region. @var{How} must be one of the values left, right or center.  This option defaluts to left.".
%%---------------------------------------------------------------------
set_justify(Justify) :-
        atom(Justify),
        set_fact(justify(Justify)),
        notify_changes.

:- export(get_justify/1).

%%---------------------------------------------------------------------
:- pred get_justify(-How) :: atom
        # "Gets @var{How} is justified the text.".
%%---------------------------------------------------------------------
get_justify(Justify) :-
        justify(Justify).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- comment(hide,tcl_name/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Shape) :: atom 
        # "Specifies the name of the @var{Shape}. In this case is text.".
%%---------------------------------------------------------------------
tcl_name(text).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the text.".
%%---------------------------------------------------------------------
creation_options([X,Y,min(text),T,min(anchor),A,min(font),F,min(justify),J|Other]) :-
        coord(X,Y),
        text(T),
        anchor(A),
        font(F),
        justify(J),
        creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

text_class.
text_class(Owner) :-
        shape_class(Owner).

text_class(X,Y,Owner) :-
        shape_class(Owner),
        set_point(X,Y).

text_class(X,Y) :-
        set_point(X,Y).

:- set_prolog_flag(multi_arity_warnings,on).
