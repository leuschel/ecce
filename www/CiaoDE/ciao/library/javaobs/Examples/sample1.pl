%
% example to show low-level java interface capabilities.
% On this example can be seen the following features:
%  - java object creation
%  - java method invocation
%  - handling of java events from prolog
%
% This example shows a calculator pad that displays on the calculator
% screen the keys clicked. On the java side are used the jdk standard
% classes, controlled by the prolog side. The action events are also
% controlled by the prolog side via event-handling predicates, posted
% on the java event queue listener through the interface primitive
% java_add_listener/3.
%
% To run this example, just use this module from the top-level and
% type example.
%

:- module(sample1,[],[objects]).

:- export(sample1/0).

:- use_class(library('javaobs/java/lang/String')).
:- use_class(library('javaobs/java/awt/Frame')).
:- use_class(library('javaobs/java/awt/TextField')).
:- use_class(library('javaobs/java/awt/Button')).
:- use_class(library('javaobs/java/awt/Container')).
:- use_class(library('javaobs/java/awt/GridLayout')).
:- use_class(library('javaobs/java/awt/BorderLayout')).
:- use_class(library('javaobs/java/awt/event/ActionEvent')).

%:- use_class(library('javaobs/java_obj')).

:- use_module(library(lists)).

:- dynamic acumulator/1.
:- dynamic operator/1.
:- dynamic number_editing/1.

:- dynamic connected/1.
:- concurrent finished/1.

acumulator(0).
operator("+").
number_editing(false).



sample1:-

%% Frame.
	Frm new 'Frame'("Prueba"),
	Frm:resize(300,300),
	Frm:setLocation(1,1),

%% Layout.
	Layout new 'BorderLayout'(1,1),
	Frm:setLayout(Layout),

%% Display.
	Display new 'TextField',
	Frm:add("North",Display,_),

%% Buttons grid.
	BtnContainer new 'Container',
	BtnLayout new 'GridLayout'(4,4),
        Btn1 new 'Button'("1"),
        Btn2 new 'Button'("2"),
        Btn3 new 'Button'("3"),
        Btn4 new 'Button'("4"),
        Btn5 new 'Button'("5"),
        Btn6 new 'Button'("6"),
        Btn7 new 'Button'("7"),
        Btn8 new 'Button'("8"),
        Btn9 new 'Button'("9"),
        Btn0 new 'Button'("0"),
        BtnAdd new 'Button'("+"),
        BtnMult new 'Button'("*"),
        BtnDiv new 'Button'("/"),
        BtnSub new 'Button'("-"),
        BtnEquals new 'Button'("="),
        BtnEnd new 'Button'("Exit"),

        Frm:add("Center",BtnContainer,_),
        BtnContainer:setLayout(BtnLayout),
        BtnContainer:add(Btn1,_),
        BtnContainer:add(Btn2,_),
        BtnContainer:add(Btn3,_),
        BtnContainer:add(BtnAdd,_),

        BtnContainer:add(Btn4,_),
        BtnContainer:add(Btn5,_),
        BtnContainer:add(Btn6,_),
        BtnContainer:add(BtnSub,_),

        BtnContainer:add(Btn7,_),
        BtnContainer:add(Btn8,_),
        BtnContainer:add(Btn9,_),
        BtnContainer:add(BtnMult,_),

        BtnContainer:add(BtnEnd,_),
        BtnContainer:add(Btn0,_),
        BtnContainer:add(BtnEquals,_),
        BtnContainer:add(BtnDiv,_),

%% Event listeners.
        Btn1:java_add_listener('java.awt.event.ActionEvent',
               sample1:appendText("1",Display)),
        Btn2:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("2",Display)),
        Btn3:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("3",Display)),
        BtnAdd:java_add_listener('java.awt.event.ActionEvent',
	       sample1:compute("+",Display)),

        Btn4:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("4",Display)),
        Btn5:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("5",Display)),
        Btn6:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("6",Display)),
        BtnSub:java_add_listener('java.awt.event.ActionEvent',
	       sample1:compute("-",Display)),

        Btn7:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("7",Display)),
        Btn8:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("8",Display)),
        Btn9:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("9",Display)),
        BtnMult:java_add_listener('java.awt.event.ActionEvent',
	       sample1:compute("*",Display)),

        BtnEnd:java_add_listener('java.awt.event.ActionEvent',
	       sample1:closeFrame(Frm,[Layout, Display, BtnContainer, BtnLayout,
	       Btn1, Btn2, Btn3, Btn4, Btn5, Btn6, Btn7, Btn8, Btn9, Btn0,
	       BtnAdd, BtnMult, BtnDiv, BtnSub, BtnEquals, BtnEnd])),

        Btn0:java_add_listener('java.awt.event.ActionEvent',
	       sample1:appendText("0",Display)),
        BtnEquals:java_add_listener('java.awt.event.ActionEvent',
	       sample1:compute("=",Display)),
        BtnDiv:java_add_listener('java.awt.event.ActionEvent',
	       sample1:compute("/",Display)),

%% Show the calculator.
	Frm:show,

	waitFinished.
%true.

waitFinished :-
	retract_fact(finished(yes)).

%%--------------------------------------------------
%% Event Handlers.
%%
closeFrame(Frame, JObjects) :-
	delete_objects(JObjects),
        Frame:dispose,
	asserta_fact(finished(yes)).

delete_objects([]).
delete_objects(_).
%delete_objects([X|Xs]) :-
%	X:destructor,
%	delete_objects(Xs).

appendText(Txt,Dsp) :-
	(number_editing(true) ->
	 Dsp:getText(Txt1),
	 append(Txt1,Txt,Text),
	 Dsp:setText(Text)
	;
	 Dsp:setText(Txt),
	 set_fact(number_editing(true))
	)
	.

%%--------------------------------------------------
%% Computation predicates. Perform the computation
%% between the acumulator and the number on the
%% screen, and print the result.
%%
compute(NewOperator, Dsp) :-
	set_fact(number_editing(false)),
	Dsp:getText(Txt),
	acumulator(Operand1),
	operator(Operator),
	number_codes(Operand2,Txt),
	perform_calculation(Operator, Operand1, Operand2, Result),
	number_codes(Result,ResultTxt),
	Dsp:setText(ResultTxt),
	set_fact(acumulator(Result)),
	set_fact(operator(NewOperator)).

perform_calculation("=", _Operand1, Operand2, Operand2).

perform_calculation("+", Operand1, Operand2, Result) :-
	Result is Operand1 + Operand2.

perform_calculation("-", Operand1, Operand2, Result) :-
	Result is Operand1 - Operand2.

perform_calculation("*", Operand1, Operand2, Result) :-
	Result is Operand1 * Operand2.

perform_calculation("/", Operand1, Operand2, Result) :-
	(Operand2 =\= 0 ->
	 Result is floor(Operand1 / Operand2)
	;
	    Result = 0        %% Division by zero.
	).
