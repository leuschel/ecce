:- module(stat_widget,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").


:- export(statwidget/4).
:- export(statwidget/5).

:- use_module(library('chartlib/bltclass')).
:- use_module(library('chartlib/test_format')).
:- use_module(library('chartlib/install_utils')).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%the generated script and stat_widget.tcl file are alike 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%statwidget(Title,Header,ElementList,Footer).
%%Each element in the Element list must be composed of two atoms.
%%The widget's appearance can be configure in different ways.
%%The widget's components are explained below.
%%Title: widget's title. It's displayed at the top center.
%%Header: text displayed below the title. 
%%ElementList: List containing the results you want to show. Each element of the list is composed by two atoms. 
%%The elements of the list are displayed in a regular two-columns table. The first atom's value of each sublist is placed on the left column and the second on the right column.     
%%Footer: text displayed below the table.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Example:
%%statwidget('This is the title','Header text',[['Number of processors','8'],['Average processors','95']],'Footer text').
:- push_prolog_flag(multi_arity_warnings,off).

statwidget(Title,Header,ElementList,Footer):-
	valid_elementlist(ElementList,'statwidget/4'),
	new_interp(Interp),
	length(ElementList,R),
	title(Title,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	Rows is R + 2,
	elements(ElementList,Rows,Interp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%statwidget(Title,Header,ElementList,Footer,BackgroundImage).
%%Each element in the Element list must be composed of two atoms.
%%The widget's appearance can be configure in different ways.
%%The widget's components are explained below.
%%Title: widget's title. It's displayed at the top center.
%%Header: text displayed below the title. 
%%ElementList: List containing the results you want to show. Each element of the list is composed by two atoms. 
%%The elements of the list are displayed in a regular two-columns table. The first atom's value of each sublist is placed on the left column and the second on the right column.     
%%Footer: text displayed below the table.   
%%BackgroundImage: Lets you choose a widget's background image. You must write the full file's path.
%%The file's image type should be gif.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Example:
%%statwidget('This is the title','Header text',[['Number of processors','8'],['Average processors','95']],'Footer text','./images/rain.gif').
statwidget(Title,Header,ElementList,Footer,BackgroundImage):-
	valid_elementlist(ElementList,'statwidget/5'),
	new_interp(Interp),
	length(ElementList,R),
	title(Title,BackgroundImage,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	Rows is R + 2,
	elements(ElementList,Rows,Interp).

%%title(Title,Interp).
title(Title,Interp):-
	name('package require BLT',T1),
	tcltk_raw_code(T1,Interp),
	name('if { $tcl_version >= 8.0 } {',T2),
	tcltk_raw_code(T2,Interp),
	name('namespace import blt::*',T3),
	tcltk_raw_code(T3,Interp),
	name('namespace import -force blt::tile::* }',T4),
	tcltk_raw_code(T4,Interp),
%%	name('bltdebug 1',DEBUG),  %%switch on the debugger
%%	tcltk_raw_code(DEBUG,Interp),
        img_dir(Imgdir),
	atom_concat(Imgdir,'chalk.gif',BackImage),
	atom_concat('image create photo bgTexture -file ',BackImage,BI),
	name(BI,T5),
	tcltk_raw_code(T5,Interp),
	name('option add *tile	bgTexture',T6),
	tcltk_raw_code(T6,Interp),
	name('option add *label.background  bgTexture',T7),
	tcltk_raw_code(T7,Interp),
	name('option add *header.font   -*-helvetica*-medium-r-*-*-12-*-*',T8),
	tcltk_raw_code(T8,Interp),
	name('option add *title.font   -*-helvetica*-bold-r-*-*-14-*-*',T9),
	tcltk_raw_code(T9,Interp),
	name('label .title -text "',L1),
	name(Title,L2),
	append(L1,L2,L3),
	name('" -anchor center -pady 14',L4),
	append(L3,L4,L5),
	tcltk_raw_code(L5,Interp).

%%title(Title,Interp).
title(Title,BackgroundImage,Interp):-
	name('package require BLT',T1),
	tcltk_raw_code(T1,Interp),
	name('if { $tcl_version >= 8.0 } {',T2),
	tcltk_raw_code(T2,Interp),
	name('namespace import blt::*',T3),
	tcltk_raw_code(T3,Interp),
	name('namespace import -force blt::tile::* }',T4),
	tcltk_raw_code(T4,Interp),
%%	name('bltdebug 1',DEBUG),  %%switch on the debugger
%%	tcltk_raw_code(DEBUG,Interp),
	back_image(BackgroundImage,Interp),
	name('option add *tile	bgTexture',T6),
	tcltk_raw_code(T6,Interp),
	name('option add *label.background  bgTexture',T7),
	tcltk_raw_code(T7,Interp),
	name('option add *header.font   -*-helvetica*-medium-r-*-*-12-*-*',T8),
	tcltk_raw_code(T8,Interp),
	name('option add *title.font   -*-helvetica*-bold-r-*-*-14-*-*',T9),
	tcltk_raw_code(T9,Interp),
	name('label .title -text "',L1),
	name(Title,L2),
	append(L1,L2,L3),
	name('" -anchor center -pady 14',L4),
	append(L3,L4,L5),
	tcltk_raw_code(L5,Interp).


:- pop_prolog_flag(multi_arity_warnings).

%%back_image(BackgroundImage,Interp).
back_image(BackgroundImage,Interp):-
	name('image create photo bgTexture -file ',T5),
	name(BackgroundImage,BG),
	append(T5,BG,T6),
	tcltk_raw_code(T6,Interp).
	
%%header(Header,Interp).
header(Header,Interp):-
	name('htext .header -text {',L1),
	name(Header,L2),
	append(L1,L2,L3),
	name('}',L4),
	append(L3,L4,L5),
	tcltk_raw_code(L5,Interp).
%%footer(Footer,Interp).
footer(Footer,Interp):-
	name('htext .footer -text {',F1),
	tcltk_raw_code(F1,Interp),
	name(Footer,F2),
	tcltk_raw_code(F2,Interp),
	name('Hit the %%',F3),
	tcltk_raw_code(F3,Interp),
	name('set w $htext(widget)',F4),
	tcltk_raw_code(F4,Interp),
	name('button $w.quit -text quit -command exit ',F5),
	tcltk_raw_code(F5,Interp),
	name('$w append $w.quit',F6),
	tcltk_raw_code(F6,Interp),
	name('%% button to close the widget. }',F7),
	tcltk_raw_code(F7,Interp).

%%elements(ElementList,Rows,Interp).
elements(ElementList,Rows,Interp):-
%%	setcounter(c1,2),  %% hace falta??????????????
	elements1(ElementList,Rows,2,Interp),
	table(Rows,Interp).
%%elements1(ElementList,Rows,CounterValue,Interp).
elements1([],_Rows,_CounterValue,_Interp).
elements1([[T1,V1]|Rs],Rows,CounterValue1,Interp):-
	name('label .t',L1),
	number_codes(CounterValue1,Number),
	name(' -text "',L2),
	name(T1,L3),
	name('"',L4),
	append(L1,Number,L5),
	append(L2,L3,L6),
	append(L5,L6,L7),
	append(L7,L4,L8),
	tcltk_raw_code(L8,Interp),
	name('label .v',N1),
	name(' -text "',N2),
	name(V1,N3),
	name('" -bd 2 -relief ridge',N4),
	append(N1,Number,N5),
	append(N2,N3,N6),
	append(N5,N6,N7),
	append(N7,N4,N8),
	tcltk_raw_code(N8,Interp),
%%	inccounter(Counter,CounterValue2),
	CounterValue2 is CounterValue1 + 1,
	elements1(Rs,Rows,CounterValue2,Interp).

%%table(Rows,Interp).
table(Rows,Interp):-
	name('table . 0,0 .title -fill both -cspan 2 1,0 .header -fill both -cspan 2',T1),
	append(T1,[92],T),
	tcltk_raw_code(T,Interp),
%%	setcounter(c,2),  %% hace falta??????????????
	table1(Rows,2,Interp).   
	
%%table1(Rows,CounterValue,Interp).
table1(Rows,CounterValue1,Interp):-
	Rows = CounterValue1,
	number_codes(CounterValue1,Number),
	name(',0 .footer -fill both -cspan 2',R),
	append(Number,R,RowLine),
	tcltk_raw_code(RowLine,Interp).



table1(Rows,CounterValue1,Interp):-
	Rows > CounterValue1,
	number_codes(CounterValue1,Number),
	name(',0 .t',R1),
	name(' -fill both ',R2),
	name(',1 .v',R3),
	append(Number,R1,R4),
	append(R4,Number,R5),
	append(R5,R2,C1),
	append(Number,R3,R6),
	append(R6,Number,R7),
	append(R7,R2,C2),
	append(C1,C2,C),
	append(C,[92],RowLine),    %%[92] = '\'
	tcltk_raw_code(RowLine,Interp),
%%	inccounter(Counter,CounterValue2),
	CounterValue2 is CounterValue1 + 1,
	table1(Rows,CounterValue2,Interp).
