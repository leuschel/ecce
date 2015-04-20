:- module(genbar1,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").


:- export(barchart1/7).
:- export(barchart1/9).
:- export(percentbarchart1/7).
:- export(yelement/1).
:- export(axis_limit/1).
:- export(header/1).
:- export(title/1).
:- export(footer/1).
:- export(maxmin_x/3).
:- export(maxmin_y/3).

:- use_module(library('chartlib/bltclass')).
:- use_module(library('chartlib/test_format')).
:- use_module(library('chartlib/color_pattern')).
:- use_module(library('chartlib/install_utils')).
:- use_module(library(lists)).
:- use_module(library(random)).

%%

:- comment(title,"Barchart widgets - 1").

:- comment(module,"This module defines predicates to show barchart
        widgets. The three predicates exported by this module plot
        two-variable data as regular bars in a window. They all share the
        following features:

	@begin{itemize}

	@item No numeric values for the @tt{x} axis are needed because
	they will be interpreted as labels. See @pred{xbarelement1/1}
	definition type.

        @item The bars will be displayed at uniform intervals.

        @item The user can either select the appearance of the bars
        (background color, foreground color and stipple style) or not. See
        the @pred{xbarelement1} type definition. Thus, the user can call
        each predicate in two ways.

        @item The bar chart has a legend. One entry (symbol and label)
        per bar.

        @item If you don't want to display text in the elements header,
        barchart title, x axis title, y axis title or footer, simply type
        @tt{''} as the value of the argument.  

        @item The predicates test whether the format of the arguments is
        correct. If one or both vectors are empty, the exception
        @tt{error2} will be thrown. If the vectors contains elements but
        are not correct, the exception @tt{error1} or @tt{error3} will be
        thrown, depending on the error type. @tt{error1} means that
        @var{XVector} and @var{YVector} do not contain the same number of
        elements and @tt{error3} indicates that not all the @var{XVector}
        elements contain a correct number of attributes .

	@end{itemize}

        @noindent
        Particular features will be pointed out in the corresponding predicate.

").

%%name('testing for ',H1), name('more than one line. ',H2), append(H1,[10],H3),append(H3,H2,H4),name(Header,H4),barchart1(Header,'Barchart title','xaxistitle',[['element1','pr1','Blue','Yellow','pattern1'],['element2','pr2','MediumTurquoise','Plum','pattern5'],['element3','pr3','MediumTurquoise','Green','pattern5']],'yaxixtitle',[20,10,59],'footer').


%%:- comment(footer/1,"@var{X} is a text (an atom) describing the footer of
%%   the graph. @includedef{footer/1}").

:- regtype header(X) 

# "@var{X} is a text (an atom) describing the header of the graph.".

header(X) :- atm(X).

:- regtype title(X) 

# "@var{X} is a text (an atom) to be used as label, usually not very long.".

title(X) :- atm(X).

:- regtype footer(X) 

# "@var{X} is a text (an atom) describing the footer of the graph.".

footer(X) :- atm(X).

:- pred barchart1(+header,+title,+title,+list(xbarelement1),+title,
	          +list(yelement),+footer).

:- comment(
  barchart1(Header,BarchartTitle,XTitle,XVector,YTitle,YVector,Footer),
  "The @tt{y} axis range is determined from the limits of the data. Two
  examples are given to demonstrate clearly how to call the predicates. In
  the first example the user sets the bar appearance, in the second one
  the appearance features will be chosen by the system and the colors that
  have been assigned to the variables Color1, Color2 and Pattern will be
  shown also.

  Example 1:

@begin{verbatim}
barchart1('This is the header text',
  'Barchart title',
  'xaxistitle',
  [ ['bar1','legend_element1','Blue','Yellow','pattern1'],
           ['bar2','legend_element2','Plum','SeaGreen','pattern2'],
           ['bar3','legend_element3','Turquoise','Yellow',
	    'pattern5'] ],
  'yaxixtitle',
  [20,10,59],
  'footer').
@end{verbatim}

Example 2:

@begin{verbatim}
barchart1('This is the header text',
  'Barchart title',
  'xaxistitle',
  [ ['element1','legend_element1',Color1,Color2,Pattern],
            ['element2','legend_element2'],
            ['element3','legend_element3'] ],
  'yaxixtitle',
  [20,10,59],
  'footer').
@end{verbatim}
 

  

").

:- push_prolog_flag(multi_arity_warnings,off).

barchart1(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,Footer):-
	srandom(_),
	not_empty(XVector,YVector,'barchart1/7'),
	equalnumber(XVector,YVector,'barchart1/7'),
	check_sublist(XVector,2,5,'barchart1/9'),
	show_barchart1(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,Footer).


:- pred barchart1(+header,+title,+title,+list(xbarelement1),+title,
	          +list(yelement),+axis_limit,+axis_limit,+footer).

:- comment(
  barchart1(Header,BTitle,XTitle,XVector,YTitle,YVector,YMax,YMin,Footer),
  "You can set the minimum and maximum limits of the @tt{y} axis. Data
  outside the limits will not be plotted. Each limit, as you can check
  by looking at the @pred{axis_limit/1} definition, is a number. If the
  argument is a variable the limit will be calculated from the data (i.e.,
  if @var{YMax} value is YValueMax the maximum y axis limit will calculated
  using the largest data value).

  Example:

@begin{verbatim}
barchart1('This is the header text',
  'Barchart title',
  'xaxistitle',
  [ ['element1','e1','Blue','Yellow','pattern1'],
      ['element2','e2','Turquoise','Plum','pattern5'],
      ['element3','e3','Turquoise','Green','pattern5'] ],
  'yaxixtitle',
  [20,10,59],
  70,
  _,
  'footer').
@end{verbatim}

").

barchart1(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,YMax,YMin,Footer):-
	srandom(_),
	not_empty(XVector,YVector,'barchart1/9'),
	equalnumber(XVector,YVector,'barchart1/9'),
	check_sublist(XVector,2,5,'barchart1/9'),
	show_barchart1(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,YMax,YMin,Footer).


:- pred percentbarchart1(+header,+title,+title,+list(xbarelement1),+title,+list(yelement),+footer).

:- comment(
  percentbarchart1(Header,BTitle,XTitle,XVector,YTitle,YVector,Footer),
  "The y axis maximum coordinate value is 100. The @tt{x} axis limits are
  automatically worked out.  

Example:

@begin{verbatim}
percentbarchart1('This is a special barchart to represent percentages',
  'Barchart with legend',
  'My xaxistitle',
  [ [1,'bar1','Blue','Yellow','pattern1'],
       [8,'bar2','MediumTurquoise','Plum','pattern5'] ],
  'My yaxixtitle',
  [80,10],
  'This is the footer text').
@end{verbatim}

").

percentbarchart1(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,Footer):-
	barchart1(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,[100],[],Footer).



:- pred show_barchart1(+atm,+atm,+atm,+list(xbarelement1),+atm,+list(yelement),+atm).

show_barchart1(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,Footer):-
	new_interp(Interp),
	begin_graph(BarchartTitle,XaxisTitle,YaxisTitle,XVector,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawbars(XVector,YVector,Interp).


:- pred show_barchart1(+atm,+atm,+atm,+list(xbarelement1),+axis_limit,+axis_limit,+atm,+list(yelement),+atm).

show_barchart1(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,YMax,YMin,Footer):-
	new_interp(Interp),
	begin_graph(BarchartTitle,XaxisTitle,YaxisTitle,XVector,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawbars(XVector,YVector,YMax,YMin,Interp).


:- pred begin_graph(BarchartTitle,XaxisTitle,YaxisTitle,XVector,Interp).

begin_graph(BarchartTitle,XaxisTitle,YaxisTitle,XVector,Interp):-
	name('package require BLT',G1),
	tcltk_raw_code(G1,Interp),
	name('if { $tcl_version >= 8.0 } {',G2),
	tcltk_raw_code(G2,Interp),
	name('namespace import blt::*',G3),
	tcltk_raw_code(G3,Interp),
	name('namespace import -force blt::tile::* }',G4),
	tcltk_raw_code(G4,Interp),
%%	name('bltdebug 1',G5),   %%switches the debugger on
%%	tcltk_raw_code(G5,Interp),
	name('set graph .bc',G6),
	tcltk_raw_code(G6,Interp),
	xticks_label(XVector,Interp),  %%label the ticks
	proc_xlabels(Interp),
        img_dir(Imgdir),
	atom_concat(Imgdir,'_patterns.tcl',Patterns),
	atom_concat('source ',Patterns,P),
	name(P,G7),
	tcltk_raw_code(G7,Interp),
	atom_concat(Imgdir,'chalk.gif',BackImage),
	atom_concat('image create photo bgTexture -file ',BackImage,BI),
	name(BI,G9),
	tcltk_raw_code(G9,Interp),
	name('option add *font		-*-helvetica*-bold-r-*-*-14-*-*',G10),
	tcltk_raw_code(G10,Interp),
	name('option add *tile		bgTexture',G11),
	tcltk_raw_code(G11,Interp),
	name('option add *Button.tile      ""',G12),
	tcltk_raw_code(G12,Interp),
	name('option add *Htext.tileOffset	no',G13),
	tcltk_raw_code(G13,Interp),
	name('option add *Htext.font	-*-times*-medium-r-*-*-14-*-*',G14),
	tcltk_raw_code(G14,Interp),
	barchart_title(BarchartTitle,Interp),
	name('option add *Axis.TickFont	    *Courier-Medium-R*-12-*',G15),
	tcltk_raw_code(G15,Interp),
	xaxis_title(XaxisTitle,Interp),
	name('option add *x.Rotate	90',G16),
	tcltk_raw_code(G16,Interp),
	name('option add *x.Command	XLabel',G17),
	tcltk_raw_code(G17,Interp),
	yaxis_title(YaxisTitle,Interp),
	name('option add *Element.Background	white',G18),
	tcltk_raw_code(G18,Interp),
	name('option add *Element.Relief	raised',G19),
	tcltk_raw_code(G19,Interp),
	name('option add *Element.BorderWidth	2',G20),
	tcltk_raw_code(G20,Interp),
	name('option add *Grid.hide	no',G21),
	tcltk_raw_code(G21,Interp),
	name('option add *Grid.dashes	{ 2 4 }',G22),
	tcltk_raw_code(G22,Interp),
	name('option add *Grid.mapX	""',G23),
	tcltk_raw_code(G23,Interp),
	name('set visual [winfo screenvisual .]',G24),
	tcltk_raw_code(G24,Interp),
	name('if { $visual != "staticgray" && $visual != "grayscale" } {',G25),
	tcltk_raw_code(G25,Interp),
	name('option add *print.background yellow',G26),
	tcltk_raw_code(G26,Interp),
	name('option add *quit.background red',G27),
	tcltk_raw_code(G27,Interp),
	name('option add *graph.background palegreen }',G28),
	tcltk_raw_code(G28,Interp).
	


%%header(Header,Interp).
header(Header,Interp):-
	name('set types {{{Poscript files} {.ps} }}',FileTypes),
	tcltk_raw_code(FileTypes,Interp),
	name('htext .header -text {',H1),
	tcltk_raw_code(H1,Interp),
	name(Header,H2),
	tcltk_raw_code(H2,Interp),
	name('Pressing the %%',H3),
	tcltk_raw_code(H3,Interp),
	name('set w $htext(widget)',H4),
	tcltk_raw_code(H4,Interp),
	name('button $w.print -text {Save} -command {',H5),
	tcltk_raw_code(H5,Interp),
	name('set file_select [tk_getSaveFile -title "Save barchart as" -filetypes $types]',H6),
	tcltk_raw_code(H6,Interp),
	name('.bc postscript output $file_select.ps }',H7),
	tcltk_raw_code(H7,Interp),
	name('$w append $w.print',H8),
	tcltk_raw_code(H8,Interp),
	name('%% button will create a Postcript file. }',H9),
	tcltk_raw_code(H9,Interp).

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
	tcltk_raw_code(F7,Interp),
	name('barchart .bc',B),
	tcltk_raw_code(B,Interp),
	name('.bc configure -barmode overlap',F8),
	tcltk_raw_code(F8,Interp).

:- pop_prolog_flag(multi_arity_warnings).

%%barchart_title(BarchartTitle,Interp).
barchart_title(BarchartTitle,Interp):-
	name('option add *Barchart.title ',B1),
	name(BarchartTitle,B2),
	add_doublequotes(B2,B3),
	append(B1,B3,B4),
	tcltk_raw_code(B4,Interp).
%%	name('barchart .bc',B),
%%	tcltk_raw_code(B,Interp).

%%xaxis_title(XaxisTitle,Interp).
xaxis_title(XaxisTitle,Interp):-
	name('option add *x.Title ',X1),
	name(XaxisTitle,X2),
	add_doublequotes(X2,X3),
	append(X1,X3,X4),
	tcltk_raw_code(X4,Interp).


yaxis_title(YaxisTitle,Interp):-
	name('option add *y.Title ',Y1),
	name(YaxisTitle,Y2),
	add_doublequotes(Y2,Y3),
	append(Y1,Y3,Y4),
	tcltk_raw_code(Y4,Interp).

:- push_prolog_flag(multi_arity_warnings,off).


drawbars(XVector,YVector,Interp):-
	attributes(XVector,Interp),
	yvalues(YVector,Interp),
	loop_xy(Interp).
	

drawbars(XVector,YVector,YMax,YMin,Interp):-
	maxmin_y(YMax,YMin,Interp),
	attributes(XVector,Interp),
	yvalues(YVector,Interp),
	loop_xy(Interp).
	
:- pop_prolog_flag(multi_arity_warnings).


:- comment(hide,maxmin_x/3).

maxmin_x(XMax,XMin,Interp):-
	nonvar(XMax),
	var(XMin),
	name('.bc axis configure x -max ',M1),
	number_codes(XMax,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp).
maxmin_x(XMax,XMin,Interp):-
	var(XMax),
	nonvar(XMin),
	name('.bc axis configure x -min ',M1),
	number_codes(XMin,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp).
maxmin_x(XMax,XMin,Interp):-
	nonvar(XMax),
	nonvar(XMin),
	name('.bc axis configure x -max ',M1),
	number_codes(XMax,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp),
	name('.bc axis configure x -min ',M4),
	number_codes(XMin,M5),
	append(M4,M5,M6),
	tcltk_raw_code(M6,Interp).
maxmin_x(_XMax,_XMin,_Interp).

:- comment(hide,maxmin_y/3).

maxmin_y(YMax,YMin,Interp):-
	nonvar(YMax),
	var(YMin),
	name('.bc axis configure y -max ',M1),
	number_codes(YMax,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp).
maxmin_y(YMax,YMin,Interp):-
	var(YMax),
	nonvar(YMin),
	name('.bc axis configure y -min ',M1),
	number_codes(YMin,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp).
maxmin_y(YMax,YMin,Interp):-
	nonvar(YMax),
	nonvar(YMin),
	name('.bc axis configure y -max ',M1),
	number_codes(YMax,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp),
	name('.bc axis configure y -min ',M4),
	number_codes(YMin,M5),
	append(M4,M5,M6),
	tcltk_raw_code(M6,Interp).
maxmin_y(_YMax,_YMin,_Interp).


attributes(XVector,Interp):-
	name('set attributes {',A),
	tcltk_raw_code(A,Interp),
	attributes1(XVector,Interp).


attributes1([],Interp):-
	name('}',A),
	tcltk_raw_code(A,Interp).
attributes1([[_ElementLabel,LegendElement,FColor,BColor,SPattern]|XVs],Interp):-
	name(LegendElement,LE),
	add_doublequotes(LE,EN1),
	color(FColor,ForegroundColor),
	name(ForegroundColor,FC1),
	color(BColor,BackgroundColor),
	name(BackgroundColor,BC1),
	pattern(SPattern,StipplePattern),
	name(StipplePattern,SP),
	append(EN1,[32],EN),
	append(FC1,[32],FC),
	append(BC1,[32],BC),
	append(EN,FC,ENFC),
	append(BC,SP,BCSP),
	append(ENFC,BCSP,BarAttr),
	tcltk_raw_code(BarAttr,Interp),
	attributes1(XVs,Interp).

attributes1([[_ElementLabel,LegendElement]|XVs],Interp):-
	name(LegendElement,LE),
	add_doublequotes(LE,EN1),
	random_color(ForegroundColor),
	name(ForegroundColor,FC1),
	random_color(BackgroundColor),
	name(BackgroundColor,BC1),
	random_pattern(StipplePattern),
	name(StipplePattern,SP),
	append(EN1,[32],EN),
	append(FC1,[32],FC),
	append(BC1,[32],BC),
	append(EN,FC,ENFC),
	append(BC,SP,BCSP),
	append(ENFC,BCSP,BarAttr),
	tcltk_raw_code(BarAttr,Interp),
	attributes1(XVs,Interp).


xticks_label(XVector,Interp):-
	name('set names {',Begin),
	xticks_label1(XVector,X1),
	list_concat(X1,X2),
	append(Begin,X2,BX),
	name('}',End),
	append(BX,End,Line),
	tcltk_raw_code(Line,Interp).


xticks_label1([],[]).
xticks_label1([[Xtick1,_,_,_,_]|L1s],[Xtick2,[32]|L2s]):-
	name(Xtick1,Xtick2),
	xticks_label1(L1s,L2s).
xticks_label1([[Xtick1,_]|L1s],[Xtick2,[32]|L2s]):-
	name(Xtick1,Xtick2),
	xticks_label1(L1s,L2s).

%%proc_xlabels(Interp).
proc_xlabels(Interp):-
	name('proc XLabel { w value } {',L1),
	tcltk_raw_code(L1,Interp),
	name('global names',L2),
	tcltk_raw_code(L2,Interp),
	name('set index [expr round($value)]',L3),
	tcltk_raw_code(L3,Interp),	
	name('set name [lindex $names [expr $index - 1]]',L6),
	tcltk_raw_code(L6,Interp),
	name('return $name }',L9),
	tcltk_raw_code(L9,Interp).

%%loop_xy(Interp).
loop_xy(Interp):-
	name('set count 1',L1),
	tcltk_raw_code(L1,Interp),
	name('foreach {label fg bg stipple} $attributes {',L2),
	tcltk_raw_code(L2,Interp),
	name('.bc element create $label -ydata [lindex $yvalues [expr $count - 1]] -xdata $count -fg $fg -bg $bg -stipple $stipple',L3),
	tcltk_raw_code(L3,Interp),
	name('incr count }',L4),
	tcltk_raw_code(L4,Interp),
	name('table . 0,0 .header -fill x 1,0 .bc -fill both 2,0 .footer -fill x',L5),
	tcltk_raw_code(L5,Interp),
	name('table configure . r0 r2 -resize none',L6),
	tcltk_raw_code(L6,Interp),
	name('Blt_ZoomStack .bc',L7),
	tcltk_raw_code(L7,Interp),
	name('Blt_Crosshairs .bc',L8),
	tcltk_raw_code(L8,Interp),
	name('Blt_ActiveLegend .bc',L9),
	tcltk_raw_code(L9,Interp),
	name('Blt_ClosestPoint .bc',L10),
	tcltk_raw_code(L10,Interp),
	name('if 0 {',L11),
	tcltk_raw_code(L11,Interp),
	name('set printer [printer open [lindex [printer names] 0]]',L12),
	tcltk_raw_code(L12,Interp),
	name('printer getattr $printer attrs',L13),
	tcltk_raw_code(L13,Interp),
	name('set attrs(Orientation) Portrait',L14),
	tcltk_raw_code(L14,Interp),
	name('printer setattr $printer attrs',L15),
	tcltk_raw_code(L15,Interp),
	name('after 2000 {',L16),
	tcltk_raw_code(L16,Interp),
	name('$graph print2 $printer',L17),
	tcltk_raw_code(L17,Interp),
	name('printer close $printer } }',L18),
	tcltk_raw_code(L18,Interp).


:- comment(doinclude,xbarelement1/1).

:- regtype xbarelement1/1.

xbarelement1([XValue,LegendElement]):-
	atomic(XValue),
	atomic(LegendElement).
xbarelement1([XValue,LegendElement,ForegColor,BackgColor,SPattern]):-
	atomic(XValue),
	atomic(LegendElement),
	color(ForegColor),
	color(BackgColor),
	pattern(SPattern).

:- comment(xbarelement1/1,"@includedef{xbarelement1/1}

   Defines the attributes of the bar.
   @begin{description}

   @item{@var{XValue}} bar label. Although @var{XValue} values may be numbers,
   the will be treated as labels. Different elements with the same label
   will produce different bars.  

   @item{@var{LegendElement}} Legend element name. It may be a number or an
   atom and equal or different to the XValue. Every @var{LegendElement}
   value of the list must be unique.

   @item{@var{ForegColor}} It sets the Foreground color of the bar. Its
   value must be a valid color, otherwise the system will throw an
   exception. If the argument value is a variable, it gets instantiated to
   a color chosen by the library.

   @item{@var{BackgColor}} It sets the Background color of the bar. Its
   value must be a valid color, otherwise the system will throw an
   exception. If the argument value is a variable, it gets instantiated to
   a color chosen by the library.

   @item{@var{SPattern}} It sets the stipple of the bar. Its value must be
   a valid pattern, otherwise the system will throw an exception. If the
   argument value is a variable, it gets instantiated to a pattern chosen
   by the library.

   @end{description}


").


:- comment(doinclude,yelement/1).

:- regtype yelement/1.

yelement(Y):-
	number(Y).

:- comment(yelement/1," 

	@includedef{yelement/1}

        Y is the bar lenght, so it must be a numeric value.

        Both Prolog and Tcl-Tk support integers and floats. Integers are
        usually specified in decimal, but if the first character is 0 the
        number is read in octal (base 8), and if the first two characters
        are 0x, the number is read in hexadecimal (base16). Float numbers
        may be specified using most of the forms defined for ANSI C,
        including the following examples:

        @begin{itemize}
        @item 9.56

        @item 5.88e-2

        @item 5.1E2
        @end{itemize}

        Note: Be careful when using floats. While 8. or 7.e4 is interpreted
        by Tcl-tk as 8.0 and 7.0e4, Prolog will not read them as float numbers.
        Example:
        @begin{verbatim}
        ?- number(8.e+5).
        @{SYNTAX ERROR: (lns 130-130) , or ) expected in arguments
        number ( 8 
        ** here **
        . e + 5 ) . 
        @}

        no
        ?- number(8.).
        @{SYNTAX ERROR: (lns 138-138) , or ) expected in arguments
        number ( 8 
        ** here **
        . ) . 
        @}

        no

        ?- number(8.0e+5).

        yes
        ?- number(8.0).

        yes
        @end{verbatim}

        Precision: Tcl-tk internally represents integers with the C type
        @var{int}, which provides at least 32 bits of precision on most
        machines. Since Prolog integers can (in some implementations)
        exceed 32 bits but the precision in Tcl-tk depends on the machine,
        it is up to the progammer to ensure that the values fit into the
        maximum precision of the machine for integers. Real numbers are
        represented with the C type @var{double}, which is usually
        represented with 64-bit values (about 15 decimal digits of
        precision) using the IEEE Floating Point Standard.

        Conversion: If the list is composed by integers and floats, Tcl-tk
        will convert integers to floats.


").


:- pred yvalues(yelement,Interp) # "yvalues must be numeric.".
yvalues(YVector,Interp):-
	name('set yvalues {',Begin),
	yvalues1(YVector,Y1),
	list_concat(Y1,Y2),
	append(Begin,Y2,BY),
	name('}',End),
	append(BY,End,Line),
	tcltk_raw_code(Line,Interp).
	 
yvalues1([],[]).
yvalues1([Y1|Y1s],[Y2,[32]|Y2s]):-
	name(Y1,Y2),
	yvalues1(Y1s,Y2s).


:- comment(doinclude,axis_limit/1).

:- regtype axis_limit/1.


axis_limit(X):- 
	number(X).  
axis_limit(_).
  
:- comment(axis_limit/1,"@includedef{axis_limit/1}

	This type is defined in order to set the minimum and maximum limits
	of the axes. Data outside the limits will not be plotted. Each
	limit, is a number or a variable. If the argument is not a number
	the limit will be calculated from the data (i.e., if YMax value is
	@tt{Var} the maximum y axis limit will be calculated using the
	largest data value).


        ").
