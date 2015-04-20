:- module(genbar4,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").

:- export(barchart4/7).
:- export(barchart4/11).
:- export(percentbarchart4/7).

:- use_module(library('chartlib/genbar1'),
        [yelement/1,axis_limit/1,header/1,title/1,footer/1,
	maxmin_x/3,maxmin_y/3]).
:- use_module(library('chartlib/bltclass')).
:- use_module(library('chartlib/color_pattern')).
:- use_module(library('chartlib/test_format')).
:- use_module(library('chartlib/install_utils')).
:- use_module(library(lists)).
:- use_module(library(random)).

:- comment(title, "Depict barchart widgets - 4").

:- comment(module,"This module defines predicates which depict barchart
	widgets. The three predicates exported by this module plot
	two-variable data as regular bars in a window and are similar to
	those exported in genbar2 module except in that those defined in this
	module doesn't display a legend. Thus, the user does not have to
	define legend element names.

        The predicates test whether the format of the arguments is
        correct. If one or both vectors are empty, the exception
        @tt{error2} will be thrown. If the vectors contains elements but
        are not correct, the exception @tt{error1} or @tt{error3} will be
        thrown, depending on the error type. @tt{error1} means that
        @var{XVector} and @var{YVector} do not contain the same number of
        elements and @tt{error3} indicates that not all the @var{XVector}
        elements contain a correct number of attributes .

 ").


:- push_prolog_flag(multi_arity_warnings,off).

:- pred barchart4(+header,+title,+title,+list(xbarelement4),+title,
	          +list(yelement),+footer).

:- comment(barchart4(Header,BarchartTitle,XTitle,XVector,YTitle,
	YVector,Footer),

	"As we mentioned in the above paragraph, this predicate is
        comparable to @pred{barchart2/8} except in the @var{XVector}
        argument type.

Example:
@begin{verbatim}
barchart4('This is the header text',
  'Barchart without legend',
  'My xaxistitle',
  [[2],[5],[6]],
  'My yaxixtitle',
  [20,10,59],
  'Numeric values in the xaxis').
@end{verbatim}

").

barchart4(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,Footer):-
	srandom(_),
	not_empty(XVector,YVector,'barchart4/7'),
	equalnumber(XVector,YVector,'barchart4/7'),
	check_sublist(XVector,2,4,'barchart4/7'),
	show_barchart4(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,Footer).




:- pred barchart4(+header,+title,+title,+list(xbarelement4),+axis_limit,
	+axis_limit,+title,+list(yelement),+axis_limit,+axis_limit,+footer).

:- comment(
	barchart4(Hder,BT,XT,XVector,XMax,XMin,YT,YVector,
	YMax,YMin,Fter),

	"As we stated before, this predicate is quite similar to
        @pred{barchart2/10} except in the following aspects:

        @begin{itemize}

        @item The @var{XVector} argument type, because the yielded bar
        chart lacks the legend.

        @item The user can set limits for both @tt{x} axis and @tt{y} axis.

        @end{itemize}

Example:
@begin{verbatim}
barchart4('This is the header text, you can write a graph description',
  'Barchart without legend',
  'My xaxistitle',
  [[2,'Blue','Yellow','pattern1'],
       [20,'MediumTurquoise','Plum','pattern5'],
       [30,'MediumTurquoise','Green','pattern5']],
  50,
  -10,
  'My yaxixtitle',
  [20,10,59],
  100,
  -10,
  'Numeric values in the xaxis').
@end{verbatim}

").

barchart4(Header,BarchartTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVector,YMax,YMin,Footer):-
	srandom(_),
	not_empty(XVector,YVector,'barchart4/11'),
	equalnumber(XVector,YVector,'barchart4/11'),
	check_sublist(XVector,2,4,'barchart4/11'),
	show_barchart4(Header,BarchartTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVector,YMax,YMin,Footer).


:- pred percentbarchart4(+header,+title,+title,+list(xbarelement4),+title,
	+list(yelement),+footer).

:- comment(percentbarchart4(Header,BTitle,XTitle,XVector,YTitle,
	YVector,Footer), 

  "The y axis maximum coordinate value is 100. The x axis limits are
  automatically worked out. This predicate is useful when the bar height represents
  percentages.

Example:

@begin{verbatim}
percentbarchart4('This is the header text',
  'Barchart without legend',
  'My xaxistitle',
  [[2,'Blue','Yellow','pattern1'],[5,'Yellow','Plum','pattern5'],
       [6,'MediumTurquoise','Green','pattern5']],
  'My yaxixtitle',
  [20,10,59],
  'Numeric values in the xaxis').
@end{verbatim}

").
percentbarchart4(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,Footer):-
	barchart4(Header,BarchartTitle,XaxisTitle,XVector,[],[],YaxisTitle,YVector,[100],[],Footer).

:- pred show_barchart4/7.

show_barchart4(Header,BarchartTitle,XaxisTitle,XVector,YaxisTitle,YVector,Footer):-
	new_interp(Interp),
	begin_graph(BarchartTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawbars(XVector,YVector,Interp).

:- pred show_barchart4/11.

show_barchart4(Header,BarchartTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVector,YMax,YMin,Footer):-
	new_interp(Interp),
	begin_graph(BarchartTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawbars(XVector,YVector,XMax,XMin,YMax,YMin,Interp).

:- pop_prolog_flag(multi_arity_warnings).

:- pred begin_graph/5.

begin_graph(BarchartTitle,XaxisTitle,YaxisTitle,Interp):-
	name('package require BLT',G1),
	tcltk_raw_code(G1,Interp),
	name('if { $tcl_version >= 8.0 } {',G2),
	tcltk_raw_code(G2,Interp),
	name('namespace import blt::*',G3),
	tcltk_raw_code(G3,Interp),
	name('namespace import -force blt::tile::* }',G4),
	tcltk_raw_code(G4,Interp),
%%	name('bltdebug 1',G5),
%%	tcltk_raw_code(G5,Interp),
	name('set graph .bc',G6),
	tcltk_raw_code(G6,Interp),
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
	name('option add *Legend.hide	yes',G17),
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
	tcltk_raw_code(B,Interp).


barchart_title(BarchartTitle,Interp):-
	name('option add *Barchart.title ',B1),
	name(BarchartTitle,B2),
	add_doublequotes(B2,B3),
	append(B1,B3,B4),
	tcltk_raw_code(B4,Interp).

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
	xticks(XVector,Interp),
	yvalues(YVector,Interp),
	loop_xy(Interp).


drawbars(XVector,YVector,XMax,XMin,YMax,YMin,Interp):-
	maxmin_x(XMax,XMin,Interp),
	maxmin_y(YMax,YMin,Interp),
	attributes(XVector,Interp),
	xticks(XVector,Interp),
	yvalues(YVector,Interp),
	loop_xy(Interp).

:- pop_prolog_flag(multi_arity_warnings).




attributes(XVector,Interp):-
	name('set attributes {',A),
	tcltk_raw_code(A,Interp),
	attributes1(XVector,Interp).


attributes1([],Interp):-
	name('}',A),
	tcltk_raw_code(A,Interp).
attributes1([[_Xvalue,FColor,BColor,SPattern]|XVs],Interp):-
	color(FColor,ForegroundColor),
	name(ForegroundColor,FC1),
	color(BColor,BackgroundColor),
	name(BackgroundColor,BC1),
	pattern(SPattern,StipplePattern),
	name(StipplePattern,SP),
	append(FC1,[32],FC),
	append(BC1,[32],BC),
	append(FC,BC,FCBC),
	append(FCBC,SP,BarAttr),
	tcltk_raw_code(BarAttr,Interp),
	attributes1(XVs,Interp).

attributes1([[_Xvalue]|XVs],Interp):-
	random_color(ForegroundColor),
	name(ForegroundColor,FC1),
	random_color(BackgroundColor),
	name(BackgroundColor,BC1),
	random_pattern(StipplePattern),
	name(StipplePattern,SP),
	append(FC1,[32],FC),
	append(BC1,[32],BC),
	append(FC,BC,FCBC),
	append(FCBC,SP,BarAttr),
	tcltk_raw_code(BarAttr,Interp),
	attributes1(XVs,Interp).


xticks(XVector,Interp):-
	name('set xvalues {',Begin),
	xticks1(XVector,X1),
	list_concat(X1,X2),
	append(Begin,X2,BX),
	name('}',End),
	append(BX,End,Line),
	tcltk_raw_code(Line,Interp),
	name('.bc axis configure x -majorticks $xvalues',Ticks),
	tcltk_raw_code(Ticks,Interp).

xticks1([],[]).
xticks1([[Xvalue1,_,_,_]|L1s],[Xvalue2,[32]|L2s]):-
	name(Xvalue1,Xvalue2),
	xticks1(L1s,L2s).
xticks1([[Xvalue1]|L1s],[Xvalue2,[32]|L2s]):-
	name(Xvalue1,Xvalue2),
	xticks1(L1s,L2s).




loop_xy(Interp):-
	name('set count 1',L1),
	tcltk_raw_code(L1,Interp),
	name('foreach {fg bg stipple} $attributes {',L2),
	tcltk_raw_code(L2,Interp),
	name('.bc element create $count -xdata [lindex $xvalues [expr $count - 1]] -ydata [lindex $yvalues [expr $count - 1]]  -fg $fg -bg $bg -stipple $stipple',L3),
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

%%yvalues(YVector,Interp).
%%yvalues must be numeric
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

:- comment(doinclude,xbarelement4/1).

:- regtype xbarelement4/1.

xbarelement4([XValue]):-
	number(XValue).
xbarelement4([XValue,ForegColor,BackgColor,SPattern]):-
	number(XValue),
	color(ForegColor),
	color(BackgColor),
	pattern(SPattern).

:- comment(xbarelement4/1,"Defines the attributes of the bar.
   @begin{description}

   @item{@var{XValue}} x-coordinate position of the bar. Different elements
   with the same abscissas will produce overlapped bars.

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
   by the library.  @end{description}

").
