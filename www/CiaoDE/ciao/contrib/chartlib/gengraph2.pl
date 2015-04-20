:- module(gengraph2,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").


:- export(graph_b2/9).
:- export(graph_b2/13).
:- export(graph_w2/9).
:- export(graph_w2/13).
:- export(scattergraph_b2/8).
:- export(scattergraph_b2/12).
:- export(scattergraph_w2/8).
:- export(scattergraph_w2/12).


:- use_module(library('chartlib/gengraph1'),
	[vector/1,smooth/1,attributes/1,symbol/1,size/1,
	 maxming_x/3,maxming_y/3,colorb/2,colorw/2]).
:- use_module(library('chartlib/genbar1'),[axis_limit/1,header/1,title/1,
	footer/1]).
:- use_module(library('chartlib/bltclass')).
:- use_module(library('chartlib/color_pattern')).
:- use_module(library('chartlib/test_format')).
:- use_module(library('chartlib/install_utils')).
:- use_module(library(lists)).
:- use_module(library(random)).

:- comment(title,"Line graph widgets").

:- comment(module,"This module defines predicates which show line graph
        widgets. All eight predicates exported by this module plot
        two-variable data. Each point is defined by its X-Y coordinate
        values. Every predicate share the following features:

	@begin{itemize}

        @item A dataset is defined by three lists xvector, yvector and
        attributes. The arguments named @var{XVectors} (or @var{XVs}),
        @var{YVectors} (or @var{YVs}) and @var{LAtts}@footnote{In scatter
        graphs the attibute that contains the features of a point dataset
        is @var{PAtts}.} contain this information. Those arguments must be
        lists whose elements are also lists. The first dataset is defined
        by the firts element of the three lists, the second dataset is
        defined by the second element of the three lists and so on.

        @item Numeric values for the vector elements are needed. We will
        use two vectors to represent the X-Y coordinates of each set of
        data plotted. In these predicates the vectors can have different
        number of points. However, the number of elements in xvector and
        yvector that pertain to a certain dataset must be, obviously,
        equal.

        @item The active line color is blue, which means that when you
        select a legend element, the corresponding line turns into
        navyblue.

        @item The user can either select the appearance for the lines and
        the points or not. See the @pred{attributes/1} type
        definition. Thus, the user can call each predicate in different
        ways ways.

        @item The graph has a legend and one entry (symbol and label)
        per dataset.

        @item If you do not want to display text in the elements
        header, barchart title, xaxis title, yaxis title or footer,
        simply give @tt{''} as the value of the argument.

        @item The predicates check whether the format of the arguments is
        correct as well. The testing process involves some
        verifications. If one or both vectors are empty, the exception
        @tt{error2} will be thrown. If the vectors contains elements but
        are not correct, the exception @tt{error4} will be thrown.

        @end{itemize}

").

:- push_prolog_flag(multi_arity_warnings,off).

:- pred graph_b2(+header,+title,+title,+list(vector),+title,+list(vector),
	       +list(attributes),+footer,+smooth).

:- comment(
graph_b2(Header,GTitle,XTitle,XVectors,YTitle,YVectors,LAtts,Footer,Sm),

	"Besides the features mentioned at the begining of the module
	chapter, the displayed graph generated calling this predicate
	has the following distinguish characteristics:

   @begin{itemize}

   @item The plotting area background color is black.  

   @item The cross hairs color is white.

   @item The axis limits are determined from the data.  

   @end{itemize}

   Example:

@begin{verbatim}
graph_b2('This is the header text',
  'Graph_title',
  'xaxistitle',
  [[20,30,59],[25,50]],
  'yaxixtitle',
  [[10,35,40],[25,50]],
  [['line1','Blue','Yellow'],['line2']],
  'footer',
  'natural').
@end{verbatim}

").

graph_b2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth):-
	srandom(_),
	not_empty(XVectors,YVectors,LinesAttributes,'graph_b2/9'),
	valid_vectors(XVectors,YVectors,LinesAttributes,'graph_b2/9'),
	show_graph_b2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth).


:- pred graph_b2(+header,+title,+title,+list(vector),+axis_limit,+axis_limit,
	+title,+list(vector),+axis_limit,+axis_limit,+list(attributes),
	+footer,+smooth).

:- comment(
graph_b2(Header,GT,XT,XVs,XMax,XMin,YT,YVs,YMax,YMin,LAtts,Footer,Smooth),

	"In addition to the features brought up at the begining of the module
	chapter, this graph has the following:

   @begin{itemize}

   @item The plotting area background color is black.  

   @item The cross hairs color is white.

   @item You can set the maximum and minimum values for the graph axes.  

   @end{itemize}

   Example:

@begin{verbatim}
graph_b2('This is the header text',
  'Graph_title',
  'xaxistitle',
  [[20,10,59],[15,30,35]],
  50,
  _,
  'yaxixtitle',
  [[10,35,40],[25,50,60]],
  50.5,
  _,
  [['line1','Blue','Yellow'],['line','MediumTurquoise','Plum']],
  'footer',
  'step').
@end{verbatim}

").

graph_b2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth):-
	srandom(_),
	not_empty(XVectors,YVectors,LinesAttributes,'graph_b2/13'),
	valid_vectors(XVectors,YVectors,LinesAttributes,'graph_b2/13'),
	show_graph_b2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth).



:- pred graph_w2(+header,+title,+title,+list(vector),+title,+list(vector),
	       +list(attributes),+footer,+smooth).

:- comment(
graph_w2(Header,GT,XT,XVectors,YTitle,YVectors,LAtts,Footer,Smooth),

	"This predicate is quite similar to @pred{graph_b2/9}. The
	difference lies in the graph appearance, as you can see below.

        @begin{itemize}

        @item The plotting area background color is white.  

        @item The cross hairs color is black.

        @end{itemize}

        Example:

@begin{verbatim}
graph_w2('This is the header text',
  'Graph_title',
  'xaxistitle',
  [[10,30,59],[25,50]],
  'yaxixtitle',
  [[10,35,40],[25,40]],
  [['element1','Blue','DarkOrchid'],['element2','DarkOliveGreen',
    'Firebrick']],
  'footer',
  'natural').
@end{verbatim}

").


graph_w2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth):-
	srandom(_),
	not_empty(XVectors,YVectors,LinesAttributes,'graph_w2/9'),
	valid_vectors(XVectors,YVectors,LinesAttributes,'graph_w2/9'),
	show_graph_w2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth).



:- pred graph_w2(+header,+title,+title,+list(vector),+axis_limit,+axis_limit,
	+title,+list(vector),+axis_limit,+axis_limit,+list(attributes),+footer,
        +smooth).

:- comment(
graph_w2(Header,GT,XT,XV,XMax,XMin,YT,YVs,YMax,YMin,LAtts,Footer,Smooth),

	"This predicate is comparable to @pred{graph_b2/13}. The
        differences lie in the plot background color and in the cross hairs
        color, wich are white and black respectively.

Example:
@begin{verbatim}
graph_w2('This is the header text',
  'Graph_title',
  'xaxistitle',
  [[10,30,59],[10,35,40]],
  80,
  _,
  'yaxixtitle',
  [[10,35,40],[25,50,60]],
  50,
  _,
  [['element1','Blue','Green'],['element2','Turquoise','Black']],
  'footer',
  'linear').
@end{verbatim}

").

graph_w2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth):-
	srandom(_),
	not_empty(XVectors,YVectors,LinesAttributes,'graph_w2/13'),
	valid_vectors(XVectors,YVectors,LinesAttributes,'graph_w2/13'),
	show_graph_w2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth).

:- pred scattergraph_b2(+header,+title,+title,+list(vector),+title,
	+list(vector),+list(attributes),+footer).

:- comment(
scattergraph_b2(Header,GT,XT,XVectors,YT,YVectors,PAtts,Footer), 

        "Apart from the features brought up at the beginning of the
	chapter, the scatter graph displayed when invoking this predicate has
	the following features:

        @begin{itemize}

        @item The plotting area background color is black.  

        @item The cross hairs color is white.

        @item The axis limits are determined from the data.  

        @end{itemize}

        Example:
@begin{verbatim}
scattergraph_b2('This is the header text',
  'Graph_title',
  'xaxistitle',
  [[10,15,20],[8,30,40]],
  'yaxixtitle',
  [[10,35,20],[15,11,21]],
  [['element1','Blue','Yellow'],['element2','MediumTurquoise','Plum']],
  'footer').
@end{verbatim}

").
scattergraph_b2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,PointAttributes,Footer):-
	srandom(_),
	not_empty(XVectors,YVectors,PointAttributes,'scattergraph_b2/8'),
	valid_vectors(XVectors,YVectors,PointAttributes,'scattergraph_b2/8'),
	show_scattergraph_b2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,PointAttributes,Footer).


:- pred scattergraph_b2(+header,+title,+title,+list(vector),+axis_limit,
	+axis_limit,+title,+list(vector),+axis_limit,+axis_limit,
	+list(attributes),+footer).

:- comment(
scattergraph_b2(Header,GT,XT,XVs,XMax,XMin,YT,YVs,YMax,YMin,PAtts,Footer),

   "The particular features related to this predicate are described below:

   @begin{itemize}

   @item The plotting area background color is black.  

   @item The cross hairs color is white.

   @item You can set the minimum and maximum limits of the axes. Data outside
   the limits will not be plotted.

   @end{itemize}

   Example:
@begin{verbatim}
scattergraph_b2('This is the header text',
  'Graph_title',
  'xaxistitle',
  [[20,30,50],[18,40,59]],
  50,
  _,
  'yaxixtitle',
  [[10,35,40],[25,50,60]],
  50,
  _,
  [['point dataset1'],['point dataset2']],
  'footer').
@end{verbatim}

").

scattergraph_b2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer):-
	srandom(_),
	not_empty(XVectors,YVectors,PointAttributes,'scattergraph_b2/12'),
	valid_vectors(XVectors,YVectors,PointAttributes,'scattergraph_b2/12'),
	show_scattergraph_b2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer).

:- pred scattergraph_w2(+header,+title,+title,+vector,+title,+list(vector),
	+list(attributes),+footer).

:- comment(
scattergraph_w2(Header,GTitle,XTitle,XVs,YTitle,YVs,PAtts,Footer),

	"This predicate is quite similar to @pred{scattergraph_w1/8} except
	in the following:

        @begin{itemize}

        @item The plotting area background color is black.  

        @item The cross hairs color is white.

        @item If the user do not provide the colors of the points, they
        will be chosen among the lighter ones.

        @end{itemize}

Example:
@begin{verbatim}
scattergraph_w2('This is the header text',
  'Graph_title',
  'xaxistitle',
  [[20,30,40,15,30,35,20,30]],
  'yaxixtitle',
  [[10,30,40,25,20,25,20,25]],
  [['set1','cross',4]],
  'footer').
@end{verbatim}

        
"). 

scattergraph_w2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,PointAttributes,Footer):-
	srandom(_),
	not_empty(XVectors,YVectors,PointAttributes,'scattergraph_w2/8'),
	valid_vectors(XVectors,YVectors,PointAttributes,'scattergraph_w2/8'),
	show_scattergraph_w2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,PointAttributes,Footer).

:- pred scattergraph_w2(+header,+title,+title,+list(vector),+axis_limit,
	+axis_limit,+title,+list(vector),+axis_limit,+axis_limit,
	+list(attributes),+footer).

:- comment(
scattergraph_w2(Header,GT,XT,XVs,XMax,XMin,YT,YVs,YMax,YMin,PAtts,Footer), 

	"This predicate is comparable to @pred{scattergraph_w2/13}, the
	differences between them are listed below:

        @begin{itemize}

        @item The plotting area background color is white.  

        @item The cross hairs color is black.

        @end{itemize}
  
        Example:
@begin{verbatim}
scattergraph_w2('This is the header text',
  'Graph_title',
  'xaxistitle',
  [[20,10,59],[15,30,50]],
  150,
  5,
  'yaxixtitle',
  [[10,35,40],[25,20,60]],
  _,
  -10,
  [['e1','Blue','Yellow'],['e2','MediumTurquoise','Plum']],
  'footer').
@end{verbatim}

").

scattergraph_w2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer):-
	srandom(_),
	not_empty(XVectors,YVectors,PointAttributes,'scattergraph_w2/12'),
	valid_vectors(XVectors,YVectors,PointAttributes,'scattergraph_w2/12'),
	show_scattergraph_w2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer).



:- pred show_graph_b2/9.

show_graph_b2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth):-
	new_interp(Interp),
	begin_graph_b2(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	connect_lines(Smooth,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_b(XVectors,YVectors,LinesAttributes,Interp).

:- pred show_graph_b2/13.

show_graph_b2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth):-
	new_interp(Interp),
	begin_graph_b2(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	connect_lines(Smooth,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_b(XVectors,XMax,XMin,YVectors,YMax,YMin,LinesAttributes,Interp).

:- pred show_graph_w2/9.

show_graph_w2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth):-
	new_interp(Interp),
	begin_graph_w2(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	connect_lines(Smooth,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_w(XVectors,YVectors,LinesAttributes,Interp).

:- pred show_graph_w2/13.

show_graph_w2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth):-
	new_interp(Interp),
	begin_graph_w2(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	connect_lines(Smooth,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_w(XVectors,XMax,XMin,YVectors,YMax,YMin,LinesAttributes,Interp).

:- pred show_scatter_graph_b2/8.

show_scattergraph_b2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,PointAttributes,Footer):-
	new_interp(Interp),
	begin_scatter_b2(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_b(XVectors,YVectors,PointAttributes,Interp).

:- pred show_scatter_graph_b2/12.

show_scattergraph_b2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer):-
	new_interp(Interp),
	begin_scatter_b2(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_b(XVectors,XMax,XMin,YVectors,YMax,YMin,PointAttributes,Interp).

:- pred show_scattergraph_w2/9.

show_scattergraph_w2(Header,GraphTitle,XaxisTitle,XVectors,YaxisTitle,YVectors,PointAttributes,Footer):-
	new_interp(Interp),
	begin_scatter_w2(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_w(XVectors,YVectors,PointAttributes,Interp).

:- pred show_scattergraph_w2/13.

show_scattergraph_w2(Header,GraphTitle,XaxisTitle,XVectors,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer):-
	new_interp(Interp),
	begin_scatter_w2(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_w(XVectors,XMax,XMin,YVectors,YMax,YMin,PointAttributes,Interp).




begin_graph_b2(GraphTitle,XaxisTitle,YaxisTitle,Interp):-
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
	name('set tcl_precision 15 ',G7),
	tcltk_raw_code(G7,Interp),
	name('set graph .graph',G8),
	tcltk_raw_code(G8,Interp),
        img_dir(Imgdir),
	atom_concat(Imgdir,'chalk.gif',BackImage),
	atom_concat('image create photo bgTexture -file ',BackImage,BI),
	name(BI,G9),
	tcltk_raw_code(G9,Interp),
	name('option add *default	normal',G10),
	tcltk_raw_code(G10,Interp),
	name('option add *Button.tile	""',G11),
	tcltk_raw_code(G11,Interp),
	name('option add *Htext.font	-*-times*-bold-r-*-*-14-*-*',G12),
	tcltk_raw_code(G12,Interp),
	name('option add *header.font	-*-times*-medium-r-*-*-14-*-*',G13),
	tcltk_raw_code(G13,Interp),
	name('option add *footer.font	-*-times*-medium-r-*-*-14-*-*',G14),
	tcltk_raw_code(G14,Interp),
	name('option add *Graph.relief		raised',G15),
	tcltk_raw_code(G15,Interp),
	name('option add *Graph.Legend.activeBackground   white',G16),
	tcltk_raw_code(G16,Interp),
	name('option add *Graph.height	5i',G17),
	tcltk_raw_code(G17,Interp),
	name('option add *Graph.plotBackground	black',G18),
	tcltk_raw_code(G18,Interp),
	name('option add *Graph.width	7i',G19),
	tcltk_raw_code(G19,Interp),
	name('option add *tile	bgTexture',G20),
	tcltk_raw_code(G20,Interp),
	name('option add *Graph.halo	0',G21),
	tcltk_raw_code(G21,Interp),
	graph_title(GraphTitle,Interp),
	name('option add *Graph.font	-*-helvetica-bold-r-*-*-14-*',G22),
	tcltk_raw_code(G22,Interp),
	name('option add *Axis.tickFont	 -*-courier-medium-r-*-*-12-*',G23),
	tcltk_raw_code(G23,Interp),
	name('option add *Axis.titleFont  -*-helvetica-bold-r-*-*-14-*',G24),
	tcltk_raw_code(G24,Interp),
	xaxis_title(XaxisTitle,Interp),
	yaxis_title(YaxisTitle,Interp),
	name('option add *Crosshairs.Color	white',G25),
	tcltk_raw_code(G25,Interp),
	name('option add *activeLine.Fill	navyblue',G26),
	tcltk_raw_code(G26,Interp),
	name('option add *activeLine.LineWidth	2',G27),
	tcltk_raw_code(G27,Interp),
	name('option add *Element.ScaleSymbols	yes',G28),
	tcltk_raw_code(G28,Interp),
	name('option add *Symbol	square',G30),
	tcltk_raw_code(G30,Interp),
	name('option add *Element.LineWidth	1',G31),
	tcltk_raw_code(G31,Interp),
	name('option add *Pen.LineWidth		1',G32),
	tcltk_raw_code(G32,Interp),
	name('option add *Pixels		1',G33),
	tcltk_raw_code(G33,Interp),
	name('option add *Grid.color	grey50',G34),
	tcltk_raw_code(G34,Interp),
	name('option add *Grid.dashes   	"2 4"',G35),
	tcltk_raw_code(G35,Interp),
	name('option add *Grid.hide	no',G36),
	tcltk_raw_code(G36,Interp),
	name('option add *Legend.ActiveRelief		sunken',G37),
	tcltk_raw_code(G37,Interp),
	name('option add *Legend.Position	right',G38),
	tcltk_raw_code(G38,Interp),
	name('option add *Legend.Relief		flat',G39),
	tcltk_raw_code(G39,Interp),
	name('option add *Legend.font	-*-lucida-medium-r-*-*-12-*-*-*-*-*-*-*',G40),
	tcltk_raw_code(G40,Interp),
	name('option add *Legend.Pad	 0',G41),
	tcltk_raw_code(G41,Interp),
	name('option add *Legend.hide		no',G42),
	tcltk_raw_code(G42,Interp),
	name('option add *LineMarker.Dashes		5',G43),
	tcltk_raw_code(G43,Interp),
	name('option add *LineMarker.Foreground	 white',G44),
	tcltk_raw_code(G44,Interp),
	name('option add *zoomOutline.outline		yellow',G45),
	tcltk_raw_code(G45,Interp),
	name('option add *TextMarker.Background	{}',G46),
	tcltk_raw_code(G46,Interp),
	name('option add *TextMarker.Foreground	white',G47),
	tcltk_raw_code(G47,Interp),
	name('set visual [winfo screenvisual .]',G48),
	tcltk_raw_code(G48,Interp),
	name('if { $visual != "staticgray" && $visual != "grayscale" } {',G49),
	tcltk_raw_code(G49,Interp),
	name('option add *print.background yellow',G50),
	tcltk_raw_code(G50,Interp),
	name('option add *quit.background red }',G51),
	tcltk_raw_code(G51,Interp).


begin_graph_w2(GraphTitle,XaxisTitle,YaxisTitle,Interp):-
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
	name('set tcl_precision 15 ',G7),
	tcltk_raw_code(G7,Interp),
	name('set graph .graph',G8),
	tcltk_raw_code(G8,Interp),
        img_dir(Imgdir),
	atom_concat(Imgdir,'chalk.gif',BackImage),
	atom_concat('image create photo bgTexture -file ',BackImage,BI),
	name(BI,G9),
	tcltk_raw_code(G9,Interp),
	name('option add *default	normal',G10),
	tcltk_raw_code(G10,Interp),
	name('option add *Button.tile	""',G11),
	tcltk_raw_code(G11,Interp),
	name('option add *Htext.font	-*-times*-bold-r-*-*-14-*-*',G12),
	tcltk_raw_code(G12,Interp),
	name('option add *header.font	-*-times*-medium-r-*-*-14-*-*',G13),
	tcltk_raw_code(G13,Interp),
	name('option add *footer.font	-*-times*-medium-r-*-*-14-*-*',G14),
	tcltk_raw_code(G14,Interp),
	name('option add *Graph.relief		raised',G15),
	tcltk_raw_code(G15,Interp),
	name('option add *Graph.Legend.activeBackground   white',G16),
	tcltk_raw_code(G16,Interp),
	name('option add *Graph.height	5i',G17),
	tcltk_raw_code(G17,Interp),
	name('option add *Graph.plotBackground	white',G18),
	tcltk_raw_code(G18,Interp),
	name('option add *Graph.width	7i',G19),
	tcltk_raw_code(G19,Interp),
	name('option add *tile	bgTexture',G20),
	tcltk_raw_code(G20,Interp),
	name('option add *Graph.halo	0',G21),
	tcltk_raw_code(G21,Interp),
	graph_title(GraphTitle,Interp),
	name('option add *Graph.font	-*-helvetica-bold-r-*-*-14-*',G22),
	tcltk_raw_code(G22,Interp),
	name('option add *Axis.tickFont	 -*-courier-medium-r-*-*-12-*',G23),
	tcltk_raw_code(G23,Interp),
	name('option add *Axis.titleFont  -*-helvetica-bold-r-*-*-14-*',G24),
	tcltk_raw_code(G24,Interp),
	xaxis_title(XaxisTitle,Interp),
	yaxis_title(YaxisTitle,Interp),
	name('option add *Crosshairs.Color	black',G25),
	tcltk_raw_code(G25,Interp),
	name('option add *activeLine.Fill	navyblue',G26),
	tcltk_raw_code(G26,Interp),
	name('option add *activeLine.LineWidth	2',G27),
	tcltk_raw_code(G27,Interp),
	name('option add *Element.ScaleSymbols	yes',G28),
	tcltk_raw_code(G28,Interp),
	name('option add *Symbol	square',G30),
	tcltk_raw_code(G30,Interp),
	name('option add *Element.LineWidth	1',G31),
	tcltk_raw_code(G31,Interp),
	name('option add *Pen.LineWidth		1',G32),
	tcltk_raw_code(G32,Interp),
	name('option add *Pixels		1',G33),
	tcltk_raw_code(G33,Interp),
	name('option add *Grid.color	grey50',G34),
	tcltk_raw_code(G34,Interp),
	name('option add *Grid.dashes   	"2 4"',G35),
	tcltk_raw_code(G35,Interp),
	name('option add *Grid.hide	no',G36),
	tcltk_raw_code(G36,Interp),
	name('option add *Legend.ActiveRelief		sunken',G37),
	tcltk_raw_code(G37,Interp),
	name('option add *Legend.Position	right',G38),
	tcltk_raw_code(G38,Interp),
	name('option add *Legend.Relief		flat',G39),
	tcltk_raw_code(G39,Interp),
	name('option add *Legend.font	-*-lucida-medium-r-*-*-12-*-*-*-*-*-*-*',G40),
	tcltk_raw_code(G40,Interp),
	name('option add *Legend.Pad	 0',G41),
	tcltk_raw_code(G41,Interp),
	name('option add *Legend.hide		no',G42),
	tcltk_raw_code(G42,Interp),
	name('option add *LineMarker.Dashes		5',G43),
	tcltk_raw_code(G43,Interp),
	name('option add *LineMarker.Foreground	 white',G44),
	tcltk_raw_code(G44,Interp),
	name('option add *zoomOutline.outline		yellow',G45),
	tcltk_raw_code(G45,Interp),
	name('option add *TextMarker.Background	{}',G46),
	tcltk_raw_code(G46,Interp),
	name('option add *TextMarker.Foreground	white',G47),
	tcltk_raw_code(G47,Interp),
	name('set visual [winfo screenvisual .]',G48),
	tcltk_raw_code(G48,Interp),
	name('if { $visual != "staticgray" && $visual != "grayscale" } {',G49),
	tcltk_raw_code(G49,Interp),
	name('option add *print.background yellow',G50),
	tcltk_raw_code(G50,Interp),
	name('option add *quit.background red }',G51),
	tcltk_raw_code(G51,Interp).


begin_scatter_b2(GraphTitle,XaxisTitle,YaxisTitle,Interp):-
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
	name('set tcl_precision 15 ',G7),
	tcltk_raw_code(G7,Interp),
	name('set graph .graph',G8),
	tcltk_raw_code(G8,Interp),
        img_dir(Imgdir),
	atom_concat(Imgdir,'chalk.gif',BackImage),
	atom_concat('image create photo bgTexture -file ',BackImage,BI),
	name(BI,G9),
	tcltk_raw_code(G9,Interp),
	name('option add *default	normal',G10),
	tcltk_raw_code(G10,Interp),
	name('option add *Button.tile	""',G11),
	tcltk_raw_code(G11,Interp),
	name('option add *Htext.font	-*-times*-bold-r-*-*-14-*-*',G12),
	tcltk_raw_code(G12,Interp),
	name('option add *header.font	-*-times*-medium-r-*-*-14-*-*',G13),
	tcltk_raw_code(G13,Interp),
	name('option add *footer.font	-*-times*-medium-r-*-*-14-*-*',G14),
	tcltk_raw_code(G14,Interp),
	name('option add *Graph.relief		raised',G15),
	tcltk_raw_code(G15,Interp),
	name('option add *Graph.Legend.activeBackground   white',G16),
	tcltk_raw_code(G16,Interp),
	name('option add *Graph.height	5i',G17),
	tcltk_raw_code(G17,Interp),
	name('option add *Graph.plotBackground	black',G18),
	tcltk_raw_code(G18,Interp),
	name('option add *Graph.width	7i',G19),
	tcltk_raw_code(G19,Interp),
	name('option add *tile	bgTexture',G20),
	tcltk_raw_code(G20,Interp),
	name('option add *Graph.halo	0',G21),
	tcltk_raw_code(G21,Interp),
	graph_title(GraphTitle,Interp),
	name('option add *Graph.font	-*-helvetica-bold-r-*-*-14-*',G22),
	tcltk_raw_code(G22,Interp),
	name('option add *Axis.tickFont	 -*-courier-medium-r-*-*-12-*',G23),
	tcltk_raw_code(G23,Interp),
	name('option add *Axis.titleFont  -*-helvetica-bold-r-*-*-14-*',G24),
	tcltk_raw_code(G24,Interp),
	xaxis_title(XaxisTitle,Interp),
	yaxis_title(YaxisTitle,Interp),
	name('option add *Crosshairs.Color	white',G25),
	tcltk_raw_code(G25,Interp),
	name('option add *activeLine.Fill	navyblue',G26),
	tcltk_raw_code(G26,Interp),
	name('option add *activeLine.LineWidth	2',G27),
	tcltk_raw_code(G27,Interp),
	name('option add *Element.ScaleSymbols	yes',G28),
	tcltk_raw_code(G28,Interp),
	name('option add *Symbol	square',G30),
	tcltk_raw_code(G30,Interp),
	name('option add *Element.LineWidth	0',G31),
	tcltk_raw_code(G31,Interp),
	name('option add *Pen.LineWidth		1',G32),
	tcltk_raw_code(G32,Interp),
	name('option add *Pixels		2',G33),
	tcltk_raw_code(G33,Interp),
	name('option add *Grid.color	grey50',G34),
	tcltk_raw_code(G34,Interp),
	name('option add *Grid.dashes   	"2 4"',G35),
	tcltk_raw_code(G35,Interp),
	name('option add *Grid.hide	no',G36),
	tcltk_raw_code(G36,Interp),
	name('option add *Legend.ActiveRelief		sunken',G37),
	tcltk_raw_code(G37,Interp),
	name('option add *Legend.Position	right',G38),
	tcltk_raw_code(G38,Interp),
	name('option add *Legend.Relief		flat',G39),
	tcltk_raw_code(G39,Interp),
	name('option add *Legend.font	-*-lucida-medium-r-*-*-12-*-*-*-*-*-*-*',G40),
	tcltk_raw_code(G40,Interp),
	name('option add *Legend.Pad	 0',G41),
	tcltk_raw_code(G41,Interp),
	name('option add *Legend.hide		no',G42),
	tcltk_raw_code(G42,Interp),
	name('option add *LineMarker.Dashes		5',G43),
	tcltk_raw_code(G43,Interp),
	name('option add *LineMarker.Foreground	 white',G44),
	tcltk_raw_code(G44,Interp),
	name('option add *zoomOutline.outline		yellow',G45),
	tcltk_raw_code(G45,Interp),
	name('option add *TextMarker.Background	{}',G46),
	tcltk_raw_code(G46,Interp),
	name('option add *TextMarker.Foreground	white',G47),
	tcltk_raw_code(G47,Interp),
	name('set visual [winfo screenvisual .]',G48),
	tcltk_raw_code(G48,Interp),
	name('if { $visual != "staticgray" && $visual != "grayscale" } {',G49),
	tcltk_raw_code(G49,Interp),
	name('option add *print.background yellow',G50),
	tcltk_raw_code(G50,Interp),
	name('option add *quit.background red }',G51),
	tcltk_raw_code(G51,Interp).

begin_scatter_w2(GraphTitle,XaxisTitle,YaxisTitle,Interp):-
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
	name('set tcl_precision 15 ',G7),
	tcltk_raw_code(G7,Interp),
	name('set graph .graph',G8),
	tcltk_raw_code(G8,Interp),
        img_dir(Imgdir),
	atom_concat(Imgdir,'chalk.gif',BackImage),
	atom_concat('image create photo bgTexture -file ',BackImage,BI),
	name(BI,G9),
	tcltk_raw_code(G9,Interp),
	name('option add *default	normal',G10),
	tcltk_raw_code(G10,Interp),
	name('option add *Button.tile	""',G11),
	tcltk_raw_code(G11,Interp),
	name('option add *Htext.font	-*-times*-bold-r-*-*-14-*-*',G12),
	tcltk_raw_code(G12,Interp),
	name('option add *header.font	-*-times*-medium-r-*-*-14-*-*',G13),
	tcltk_raw_code(G13,Interp),
	name('option add *footer.font	-*-times*-medium-r-*-*-14-*-*',G14),
	tcltk_raw_code(G14,Interp),
	name('option add *Graph.relief		raised',G15),
	tcltk_raw_code(G15,Interp),
	name('option add *Graph.Legend.activeBackground   white',G16),
	tcltk_raw_code(G16,Interp),
	name('option add *Graph.height	5i',G17),
	tcltk_raw_code(G17,Interp),
	name('option add *Graph.plotBackground	white',G18),
	tcltk_raw_code(G18,Interp),
	name('option add *Graph.width	7i',G19),
	tcltk_raw_code(G19,Interp),
	name('option add *tile	bgTexture',G20),
	tcltk_raw_code(G20,Interp),
	name('option add *Graph.halo	0',G21),
	tcltk_raw_code(G21,Interp),
	graph_title(GraphTitle,Interp),
	name('option add *Graph.font	-*-helvetica-bold-r-*-*-14-*',G22),
	tcltk_raw_code(G22,Interp),
	name('option add *Axis.tickFont	 -*-courier-medium-r-*-*-12-*',G23),
	tcltk_raw_code(G23,Interp),
	name('option add *Axis.titleFont  -*-helvetica-bold-r-*-*-14-*',G24),
	tcltk_raw_code(G24,Interp),
	xaxis_title(XaxisTitle,Interp),
	yaxis_title(YaxisTitle,Interp),
	name('option add *Crosshairs.Color	black',G25),
	tcltk_raw_code(G25,Interp),
	name('option add *activeLine.Fill	navyblue',G26),
	tcltk_raw_code(G26,Interp),
	name('option add *activeLine.LineWidth	2',G27),
	tcltk_raw_code(G27,Interp),
	name('option add *Element.ScaleSymbols	yes',G28),
	tcltk_raw_code(G28,Interp),
	name('option add *Symbol	square',G30),
	tcltk_raw_code(G30,Interp),
	name('option add *Element.LineWidth	0',G31),
	tcltk_raw_code(G31,Interp),
	name('option add *Pen.LineWidth		1',G32),
	tcltk_raw_code(G32,Interp),
	name('option add *Pixels		2',G33),
	tcltk_raw_code(G33,Interp),
	name('option add *Grid.color	grey50',G34),
	tcltk_raw_code(G34,Interp),
	name('option add *Grid.dashes   	"2 4"',G35),
	tcltk_raw_code(G35,Interp),
	name('option add *Grid.hide	no',G36),
	tcltk_raw_code(G36,Interp),
	name('option add *Legend.ActiveRelief		sunken',G37),
	tcltk_raw_code(G37,Interp),
	name('option add *Legend.Position	right',G38),
	tcltk_raw_code(G38,Interp),
	name('option add *Legend.Relief		flat',G39),
	tcltk_raw_code(G39,Interp),
	name('option add *Legend.font	-*-lucida-medium-r-*-*-12-*-*-*-*-*-*-*',G40),
	tcltk_raw_code(G40,Interp),
	name('option add *Legend.Pad	 0',G41),
	tcltk_raw_code(G41,Interp),
	name('option add *Legend.hide		no',G42),
	tcltk_raw_code(G42,Interp),
	name('option add *LineMarker.Dashes		5',G43),
	tcltk_raw_code(G43,Interp),
	name('option add *LineMarker.Foreground	 white',G44),
	tcltk_raw_code(G44,Interp),
	name('option add *zoomOutline.outline		yellow',G45),
	tcltk_raw_code(G45,Interp),
	name('option add *TextMarker.Background	{}',G46),
	tcltk_raw_code(G46,Interp),
	name('option add *TextMarker.Foreground	white',G47),
	tcltk_raw_code(G47,Interp),
	name('set visual [winfo screenvisual .]',G48),
	tcltk_raw_code(G48,Interp),
	name('if { $visual != "staticgray" && $visual != "grayscale" } {',G49),
	tcltk_raw_code(G49,Interp),
	name('option add *print.background yellow',G50),
	tcltk_raw_code(G50,Interp),
	name('option add *quit.background red }',G51),
	tcltk_raw_code(G51,Interp).


:- pred connect_lines/2.

connect_lines('linear',Interp):-
	name('option add *Element.Smooth	linear',S),
	tcltk_raw_code(S,Interp).
connect_lines('natural',Interp):-
	name('option add *Element.Smooth	natural',S),
	tcltk_raw_code(S,Interp).
connect_lines('quadratic',Interp):-
	name('option add *Element.Smooth	quadratic',S),
	tcltk_raw_code(S,Interp).
connect_lines('step',Interp):-
	name('option add *Element.Smooth	step',S),
	tcltk_raw_code(S,Interp).
connect_lines(_,Interp):-
	name('option add *Element.Smooth	linear',S),
	tcltk_raw_code(S,Interp).
	
%%graph_title(GraphTitle,Interp).
graph_title(GraphTitle,Interp):-
	name('option add *Graph.title ',G1),
	name(GraphTitle,G2),
	add_doublequotes(G2,G3),
	append(G1,G3,G4),
	tcltk_raw_code(G4,Interp).

%%xaxis_title(XaxisTitle,Interp).
xaxis_title(XaxisTitle,Interp):-
	name('option add *x.title ',X1),
	name(XaxisTitle,X2),
	add_doublequotes(X2,X3),
	append(X1,X3,X4),
	tcltk_raw_code(X4,Interp).

%%yaxis_title(YaxisTitle,Interp).
yaxis_title(YaxisTitle,Interp):-
	name('option add *y.title ',Y1),
	name(YaxisTitle,Y2),
	add_doublequotes(Y2,Y3),
	append(Y1,Y3,Y4),
	tcltk_raw_code(Y4,Interp).

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
	name('set file_select [tk_getSaveFile -title "Save graph as" -filetypes $types]',H6),
	tcltk_raw_code(H6,Interp),
	name('.graph postscript output $file_select.ps }',H7),
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
	name('graph $graph',Graph),
	tcltk_raw_code(Graph,Interp).

:- pred drawlines_b/4.

drawlines_b(XVectors,YVectors,LinesAttributes,Interp):-
	xvalues1(XVectors,1,Interp),
	yvalues1(YVectors,1,Interp),
	attributes_b(LinesAttributes,Interp),
	loop_xy(Interp),
	table(Interp).

:- pred drawlines_b/8.

drawlines_b(XVectors,XMax,XMin,YVectors,YMax,YMin,LinesAttributes,Interp):-
	maxming_x(XMax,XMin,Interp),
	maxming_y(YMax,YMin,Interp),
	xvalues1(XVectors,1,Interp),
	yvalues1(YVectors,1,Interp),
	attributes_b(LinesAttributes,Interp),
	loop_xy(Interp),
	table(Interp).
:- pred drawlines_b/4.

drawlines_w(XVectors,YVectors,LinesAttributes,Interp):-
	xvalues1(XVectors,1,Interp),
	yvalues1(YVectors,1,Interp),
	attributes_w(LinesAttributes,Interp),
	loop_xy(Interp),
	table(Interp).

:- pred drawlines_w/8.

drawlines_w(XVectors,XMax,XMin,YVectors,YMax,YMin,LinesAttributes,Interp):-
	maxming_x(XMax,XMin,Interp),
	maxming_y(YMax,YMin,Interp),
	xvalues1(XVectors,1,Interp),
	yvalues1(YVectors,1,Interp),
	attributes_w(LinesAttributes,Interp),
	loop_xy(Interp),
	table(Interp).

:- pop_prolog_flag(multi_arity_warnings).

xvalues1([],_Number,_Interp).
xvalues1([XValues|XVs],Number1,Interp):-
	xvalues2(XValues,Number1,Interp),
	Number2 is Number1 + 1,
	xvalues1(XVs,Number2,Interp).
	
xvalues2(XValues,Number1,Interp):-
	name('vector X',E1),
	number_codes(Number1,E2),
	append(E1,E2,E3),
	tcltk_raw_code(E3,Interp),
	name('X',E4),
	append(E4,E2,E5),
	name(' set {',E6),
	append(E5,E6,E7),
	tcltk_raw_code(E7,Interp),
	xvalueslist(XValues,XValuesList1),
	list_concat(XValuesList1,XValuesList2),
	tcltk_raw_code(XValuesList2,Interp).

xvalueslist([],[[125]]).  %%[125] = }
xvalueslist([X1|X1s],[X2|X2s]):-
	name(X1,N1),
	append(N1,[32],X2),
	xvalueslist(X1s,X2s).

yvalues1([],_Number,_Interp).
yvalues1([YValues|YVs],Number1,Interp):-
	yvalues2(YValues,Number1,Interp),
	Number2 is Number1 + 1,
	yvalues1(YVs,Number2,Interp).
	
yvalues2(YValues,Number1,Interp):-
	name('vector Y',E1),
	number_codes(Number1,E2),
	append(E1,E2,E3),
	tcltk_raw_code(E3,Interp),
	name('Y',E4),
	append(E4,E2,E5),
	name(' set {',E6),
	append(E5,E6,E7),
	tcltk_raw_code(E7,Interp),
	yvalueslist(YValues,YValuesList1),
	list_concat(YValuesList1,YValuesList2),
	tcltk_raw_code(YValuesList2,Interp).
	

yvalueslist([],[[125]]).   %%[125] = }
yvalueslist([Y1|Y1s],[Y2|Y2s]):-
	number_codes(Y1,N1),
	append(N1,[32],Y2),
	yvalueslist(Y1s,Y2s).

attributes_b(LinesAttributes,Interp):-
	name('set attributes {',A),
	tcltk_raw_code(A,Interp),
	attributes1_b(LinesAttributes,1,Interp).
attributes1_b([],_,Interp):-
	name('}',A),
	tcltk_raw_code(A,Interp).

attributes1_b([[ElementName]|As],Number,Interp):-
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	random_lightcolor(OC),
	name(OC,O1),
	random_lightcolor(C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_b(As,Number2,Interp).

attributes1_b([[ElementName,Symbol,Size]|As],Number,Interp):-
	nonvar(Symbol),
	nonvar(Size),
	symbol(Symbol),
	number(Size),
	!,
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	random_lightcolor(OC),
	name(OC,O1),
	random_lightcolor(C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	atom_concat(Symbol,' ',Sy),
	name(Sy,S),
	name(Size,NPixels),
	append(S,NPixels,PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_b(As,Number2,Interp).

attributes1_b([[ElementName,OutLine,Color]|As],Number,Interp):-
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorb(OutLine,OC),
	name(OC,O1),
	colorb(Color,C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_b(As,Number2,Interp).

attributes1_b([[ElementName,OutLine,Color,Symbol,Size]|As],Number,Interp):-
	symbol(Symbol),
	number(Size),
	!,
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorb(OutLine,OC),
	name(OC,O1),
	colorb(Color,C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	atom_concat(Symbol,' ',Sy),
	name(Sy,S),
	name(Size,NPixels),
	append(S,NPixels,PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_b(As,Number2,Interp).

attributes1_b([[ElementName,OutLine,Color,_Symbol,_Size]|As],Number,Interp):-
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorb(OutLine,OC),
	name(OC,O1),
	colorb(Color,C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_b(As,Number2,Interp).



attributes_w(LinesAttributes,Interp):-
	name('set attributes {',A),
	tcltk_raw_code(A,Interp),
	attributes1_w(LinesAttributes,1,Interp).

attributes1_w([],_,Interp):-
	name('}',A),
	tcltk_raw_code(A,Interp).

attributes1_w([[ElementName]|As],Number,Interp):-
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	random_darkcolor(OC),
	name(OC,O1),
	random_darkcolor(C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_w(As,Number2,Interp).

attributes1_w([[ElementName,Symbol,Size]|As],Number,Interp):-
	nonvar(Symbol),
	nonvar(Size),
	symbol(Symbol),
	number(Size),
	!,
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	random_darkcolor(OC),
	name(OC,O1),
	random_darkcolor(C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	atom_concat(Symbol,' ',Sy),
	name(Sy,S),
	name(Size,NPixels),
	append(S,NPixels,PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_w(As,Number2,Interp).

attributes1_w([[ElementName,OutLine,Color]|As],Number,Interp):-
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorw(OutLine,OC),
	name(OC,O1),
	colorw(Color,C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_w(As,Number2,Interp).

attributes1_w([[ElementName,OutLine,Color,Symbol,Size]|As],Number,Interp):-
	symbol(Symbol),
	number(Size),
	!,
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorw(OutLine,OC),
	name(OC,O1),
	colorw(Color,C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	atom_concat(Symbol,' ',Sy),
	name(Sy,S),
	name(Size,NPixels),
	append(S,NPixels,PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_w(As,Number2,Interp).
attributes1_w([[ElementName,OutLine,Color,_Symbol,_Size]|As],Number,Interp):-
	number_codes(Number,NLine),
	append([88],NLine,NLine1),  %%[88] = 'X'
	append([89],NLine,NLine2),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorw(OutLine,OC),
	name(OC,O1),
	colorw(Color,C),
	name(C,C1),
	append(NLine1,[32],XV1),  %%[32] = ' ' 
	append(NLine2,[32],YV1),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(XV1,YV1,VData),
	append(EN2,VData,EN2VData),
	append(O2,C2,O2C2),
	append(EN2VData,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_w(As,Number2,Interp).


%%loop_xy(Interp).
loop_xy(Interp):-
	name('foreach {label xData yData outline color symbol pixels} $attributes {',L1),
	tcltk_raw_code(L1,Interp),
	name('.graph element create $label -x $xData -y $yData -outline $outline -color $color -symbol $symbol -pixels $pixels}',L2),
	tcltk_raw_code(L2,Interp).

%%table(Interp).
table(Interp):-
	name('table . .header 0,0 -fill x .graph 1,0  -fill both .footer 2,0 -fill x',T1),
	tcltk_raw_code(T1,Interp),
	name('table configure . r0 r2 -resize none',T2),
	tcltk_raw_code(T2,Interp),
	name('Blt_ZoomStack $graph',T3),
	tcltk_raw_code(T3,Interp),
	name('Blt_Crosshairs $graph',T4),
	tcltk_raw_code(T4,Interp),
	name('Blt_ActiveLegend $graph',T5),
	tcltk_raw_code(T5,Interp),
	name('Blt_ClosestPoint $graph',T6),
	tcltk_raw_code(T6,Interp),
	name('$graph element bind all <Enter> {',T7),
	tcltk_raw_code(T7,Interp),
	name('%W legend activate [%W element get current] }',T8),
	tcltk_raw_code(T8,Interp),
	name('$graph element bind all <Leave> {',T9),
	tcltk_raw_code(T9,Interp),
	name('%W legend deactivate [%W element get current] }',T10),
        tcltk_raw_code(T10,Interp).
