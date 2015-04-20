
:- module(sample2,[],[objects]).

:- export(sample2/0).

:- use_class(library('javaobs/java/lang/String')).
:- use_class(library('javaobs/java/awt/Frame')).
:- use_class(library('javaobs/java/awt/TextField')).
:- use_class(library('javaobs/java/awt/Button')).
:- use_class(library('javaobs/java/awt/Container')).
:- use_class(library('javaobs/java/awt/GridLayout')).
:- use_class(library('javaobs/java/awt/BorderLayout')).
:- use_class(library('javaobs/java/awt/event/ActionEvent')).
:- use_class(library('javaobs/java/awt/Label')).

:- use_module(library(lists)).

:- dynamic connected/1.

sample2:-

%% Frame.
	Frm new 'Frame'("Hello World"),
display(pasa),nl,
	Frm:resize(300,200),
	Frm:setLocation(100,100),

%% Layout.
	Layout new 'GridLayout'(4,1),
	Frm:setLayout(Layout),

%% Buttons grid.
        BtnClear new 'Button'("Clear"),
        BtnHello new 'Button'("Say Hello World"),
        BtnExit new 'Button'("Exit"),
	LblHello new 'Label'(""),

        Frm:add(LblHello,_),
        Frm:add(BtnClear,_),
        Frm:add(BtnHello,_),
        Frm:add(BtnExit,_),

%% Event listeners.
        BtnClear:java_add_listener('java.awt.event.ActionEvent',
               sample2:clear(LblHello)),
        BtnHello:java_add_listener('java.awt.event.ActionEvent',
	       sample2:helloWorld(LblHello)),
        BtnExit:java_add_listener('java.awt.event.ActionEvent',
	       sample2:exit(Frm)),

%% Show the calculator.
	Frm:show.

waitForEver :-
	waitForEver.

%%--------------------------------------------------
%% Event Handlers.
%%
exit(Frm) :-
        Frm:dispose.

clear(Lbl) :-
	 Lbl:setText("").

helloWorld(Lbl) :-
	 Lbl:setText("Hello World").
