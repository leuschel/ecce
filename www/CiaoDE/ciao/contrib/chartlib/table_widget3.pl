:- module(table_widget3,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").


:- export(tablewidget3/4).
:- export(tablewidget3/5).

:- use_module(library('chartlib/genbar1'),[header/1,title/1,footer/1]).
:- use_module(library('chartlib/bltclass')). 
:- use_module(library('chartlib/table_widget1'),[image/1,table/1]).
:- use_module(library('chartlib/test_format')).
:- use_module(library('chartlib/install_utils')).
:- use_module(library(lists)).

:- comment(module,"The predicates exported by this module display data in a
	regular table, as we brought up in the introduction. Both
	predicates have in common that the font weight for the elements
	placed in the first column is bold and the remaining elements are
	in medium font weight.

        If the arguments are not in a correct format the exception
        @tt{error8} will be thrown.

").

:- push_prolog_flag(multi_arity_warnings,off).

:- pred tablewidget3(+title,+header,+table,+footer).

:- comment(tablewidget3(Title,Header,ElementTable,Footer),"Shows a regular
	table in a window. The user does not choose a background image.

Example:
@begin{verbatim}
tablewidget3('This is the title',
	     'Header text',
	     [['Number of processors','8'],['Average processors','95'],
	       ['Tasks per fork','7.5']],
	     'Footer text').
@end{verbatim}
").

tablewidget3(Title,Header,ElementTable,Footer):-
	valid_table(ElementTable,'tablewidget3/4'),
	new_interp(Interp),
	length(ElementTable,R),
	length_element(ElementTable,Columns),
	title(Title,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	Rows is R + 2,
	label_elements(ElementTable,Interp),
	table(Rows,Columns,Interp).

:- pred tablewidget1(+title,+header,+table,+footer,+image).

:- comment(tablewidget3(Title,Header,ElementTable,Footer,BackgroundImage),
	"Shows a regular table in a window. The user must set a background
	image. 

Example:
@begin{verbatim}
tablewidget3('This is the title',
	     'Header text',
	     [['Number of processors','8'],['Average processors','95'],
	         ['Average Tasks per fork','7.5']],
	     'Footer text',
	     './images/rain.gif')
@end{verbatim}

").

tablewidget3(Title,Header,ElementTable,Footer,BackgroundImage):-
	valid_table(ElementTable,'tablewidget3/5'),
	new_interp(Interp),
	length(ElementTable,R),
	length_element(ElementTable,Columns),
	title(Title,BackgroundImage,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	Rows is R + 2,
	label_elements(ElementTable,Interp),
	table(Rows,Columns,Interp).

:- pred title(+title,+bltwish_interp).

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

:- pred back_image(+image,+bltwish_interp).

back_image(BackgroundImage,Interp):-
	name('image create photo bgTexture -file ',T5),
	name(BackgroundImage,BG),
	append(T5,BG,T6),
	tcltk_raw_code(T6,Interp).

:- pred header(+header,+bltwish_interp).

header(Header,Interp):-
	name('htext .header -text {',L1),
	name(Header,L2),
	append(L1,L2,L3),
	name('}',L4),
	append(L3,L4,L5),
	tcltk_raw_code(L5,Interp).

:- pred footer(+footer,+bltwish_interp).

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

:- pred label_elements(+table,+bltwish_interp).

label_elements(ElementTable,Interp):-
	label_elements1(ElementTable,2,Interp).

label_elements1([],_CurrentRow,_Interp).
label_elements1([Row|Rs],CurrentRow,Interp):-
	label_row(Row,CurrentRow,0,Interp),
	CurrentRow2 is CurrentRow + 1,
	label_elements1(Rs,CurrentRow2,Interp).

label_row([],_CurrentRow,_CurrentColumn,_Interp).
label_row([Element|Es],CurrentRow,0,Interp):-
	number_codes(CurrentRow,CR),
	number_codes(0,CC),
	name('label .t',L1),
	append(CR,CC,Position),
	append(L1,Position,Label),
	name(' -text "',L2),
	name(Element,L3),
	name('" -bd 2 -relief ridge ',L4),
	append(Label,L2,L5),
	append(L5,L3,L6),
	append(L6,L4,L7),
	tcltk_raw_code(L7,Interp),
	label_row(Es,CurrentRow,1,Interp).
label_row([Element|Es],CurrentRow,CurrentColumn,Interp):-
	number_codes(CurrentRow,CR),
	number_codes(CurrentColumn,CC),
	name('label .t',L1),
	append(CR,CC,Position),
	append(L1,Position,Label),
	name(' -text "',L2),
	name(Element,L3),
	name('" -bd 2 -relief ridge -font -*-helvetica*-medium-r-*-*-12-*-*',L4),
	append(Label,L2,L5),
	append(L5,L3,L6),
	append(L6,L4,L7),
	tcltk_raw_code(L7,Interp),
	CurrentColumn2 is CurrentColumn + 1,
	label_row(Es,CurrentRow,CurrentColumn2,Interp).

:- pred table(+integer,+integer,+bltwish_interp).

table(Rows,Columns,Interp):-
	name('table . 0,0 .title -fill both -cspan ',T1),
	number_codes(Columns,Cspan),
	append(T1,Cspan,T2),
	name(' 1,0 .header -fill both -cspan ',T3),
	append(T2,T3,T4),
	append(T4,Cspan,T5),
	append(T5,[92],T6),
	tcltk_raw_code(T6,Interp),
	table1(Rows,2,Columns,Interp).   
	
table1(Rows,Rows,Columns,Interp):-
	number_codes(Rows,CR),
	number_codes(Columns,Cspan),
	name(',0 .footer -fill both -cspan ',T1),
	append(CR,T1,T2),
	append(T2,Cspan,T3),
	tcltk_raw_code(T3,Interp).

table1(Rows,CurrentRow,Columns,Interp):-
	table1_column(CurrentRow,Columns,0,Interp),
	CurrentRow2 is CurrentRow + 1,
	table1(Rows,CurrentRow2,Columns,Interp).


table1_column(_CurrentRow,Columns,Columns,_Interp).
table1_column(CurrentRow,Columns,CurrentColumn,Interp):-
	number_codes(CurrentRow,CR),
	number_codes(CurrentColumn,CC),
	append(CR,CC,CRCC),
	append(CR,[44],T1),      %%[44] = ','
	append(T1,CC,T2),
	name(' .t',T3),
	append(T2,T3,T4),
	append(T4,CRCC,T5),
	name(' -fill both ',T6),
	append(T5,T6,T7),
	append(T7,[92],ElementTable),    %%[92] = '\'
	tcltk_raw_code(ElementTable,Interp),
	CurrentColumn2 is CurrentColumn + 1,
	table1_column(CurrentRow,Columns,CurrentColumn2,Interp).
