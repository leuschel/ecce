:- module(gengraph1,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").

:- export(graph_b1/9).
:- export(graph_b1/13).
:- export(graph_w1/9).
:- export(graph_w1/13).
:- export(scattergraph_b1/8).
:- export(scattergraph_b1/12).
:- export(scattergraph_w1/8).
:- export(scattergraph_w1/12).


:- export(vector/1).
:- export(smooth/1).
:- export(attributes/1).
:- export(symbol/1).
:- export(size/1).
:- export(maxming_x/3).
:- export(maxming_y/3).
:- export(colorb/2).
:- export(colorw/2).

:- use_module(library('chartlib/bltclass')).  
:- use_module(library('chartlib/genbar1'),[axis_limit/1,header/1,title/1,
	footer/1]).
:- use_module(library('chartlib/color_pattern')).
:- use_module(library('chartlib/test_format')).
:- use_module(library('chartlib/install_utils')).
:- use_module(library(lists)).  
:- use_module(library(random)).

:- comment(title,"Depic line graph").

:- comment(module,"This module defines predicates which depict line graph
	and scatter graph widgets. All eigth predicates exported by this
	module plot two-variable data. Each point is defined by its X-Y
	coordinate values. A dataset is defined by two lists xvector and
	yvector, which contain the points coordinates. As you might guess,
	the values placed in the the same position in both lists are the
	coordinates of a point. They both share the following features:

        @begin{itemize}

        @item Numeric values for vector elements are needed. We'll use two
        vectors to represent the X-Y coordinates of each set of plotted
        data, but in this case every dataset shares the X-vector, i.e.,
        x-coordinate of points with the same index @footnote{It should be
        pointed out that I am refering to an index as the position of an
        element in a list.} in different datasets is the same. Thus, the
        numbers of points in each yvector must be equal to the number of
        points in the xvector.

        @item The active element color is navyblue, which means that when you
        select a legend element, the corresponding line or point dataset
        turns into navyblue.

        @item The user can either select the appearance of the lines and/or
        points of each dataset or not. If not, the system will choose
        the colors for the lines and the points among the available ones in
        accordance with the plot background color and it will also set the
        points size and symbol to the default. If the plot background color
        is black, the system will choose a lighter color, and the system
        will select a darker color when the plot background color is
        white. Thus, the user can define the appearanse attributes of each
        dataset in four different ways. Take a look at the @pred{attributes/1}
        type definition and see the examples to understand it clearly.

        @item The graph has a legend and one entry (symbol and label)
        per dataset.

        @item If you do not want to display text in the element
        header, barchart title, xaxis title, yaxis title or footer,
        simply give @tt{''} as the value of the argument.

        @item The predicates check whether the format of the arguments is
        correct as well. The testing process involves some
        verifications. If one or both vectors are empty, the exception
        @tt{error2} will be thrown. If the vectors contains elements but
        are not correct, the exception @tt{error4} will be thrown.

        @end{itemize}

        The names of the line graph predicates begin with @bf{graph_} and
        those corresponding to the scatter graph group begin with
        @bf{scattergraph_}.

 ").


:- push_prolog_flag(multi_arity_warnings,off).

:- pred graph_b1(+header,+title,+title,+vector,+title,+list(vector),
	       +list(attributes),+footer,+smooth).

:-  comment(
	graph_b1(Header,GTitle,XTitle,XVector,YTitle,YVectors,LAtts,
	Footer,Smooth),

   "Besides the features mentioned at the begining of the chapter, the
   displayed graph generated when calling this predicate has the
   following distinguishing characteristics:

   @begin{itemize}

   @item The plotting area background color is black.  

   @item The cross hairs color is white.

   @item The axes limits are determined from the data.  

   @end{itemize}

   Example:

@begin{verbatim}
graph_b1('This is the header text',
  'Graph_title',
  'xaxistitle',
  [20,10,59],
  'yaxixtitle',
  [ [10,35,40],[25,50,60] ],
  [ ['element1','Blue','Yellow','plus',6],['element2',Outline,Color] ],
  'footer',
  'linear').
@end{verbatim}


").


graph_b1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth):-
	srandom(_),
	not_empty(XVector,YVectors,LinesAttributes,'graph_b1/9'),
	vectors_format(XVector,YVectors,LinesAttributes,'graph_b1/9'),
	show_graph_b1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth).



:- pred graph_b1(+header,+title,+title,+vector,+axis_limit,+axis_limit,
                 +title,+list(vector),+axis_limit,+axis_limit,
                 +list(attributes),+footer,+smooth).

:- comment(
   graph_b1(Header,GT,XT,XV,XMax,XMin,YT,YVs,YMax,YMin,LAtts,Footer,Smooth),

   "The particular features related to this predicate are described below:

   @begin{itemize}

   @item The plotting area background color is black.  

   @item The cross hairs color is white.

   @item You can set the minimum and maximum limits of the axes. Data outside
   the limits will not be plotted.

   @end{itemize}

   Example:
@begin{verbatim}
graph_b1('This is the header text',
  'Graph_title',
  'xaxistitle',
  [20,10,59],
  50,
  _,
  'yaxixtitle',
  [[10,35,40],[25,50,60]],
  50,
  _,
  [['line1','circle',4],['line2',OutlineColor,Color]],
  'footer',
  'step').
@end{verbatim}

").

graph_b1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth):-
	srandom(_),
	not_empty(XVector,YVectors,LinesAttributes,'graph_b1/13'),
	vectors_format(XVector,YVectors,LinesAttributes,'graph_b1/13'),
	show_graph_b1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth).


:- pred graph_w1(+header,+title,+title,+vector,+title,+list(vector),
               +list(attributes),+footer,+smooth).

:- comment(
	graph_w1(Header,GTitle,XTitle,XVector,YTitle,YVectors,LAtts,Footer,
	Smooth),

        "This predicate is quite similar to @pred{graph_b1/9}. The
        differences lies in the plot background color and in the cross hairs
        color, which are white and black respectively.

        Example: 
@begin{verbatim}
graph_w1('This is the header text', 
  'Graph_title', 
  'xaxistitle',
  [20,10,40,50], 
  'yaxixtitle', 
  [ [10,35,40,50],[25,20,60,40] ],
  [['line1','Blue','DarkOrchid'],['line2','circle',3]],
  'footer', 
  'quadratic').  
@end{verbatim}

").

graph_w1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth):-
	srandom(_),
	not_empty(XVector,YVectors,LinesAttributes,'graph_w1/9'),
	vectors_format(XVector,YVectors,LinesAttributes,'graph_w1/9'),
	show_graph_w1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth).

:- pred graph_w1(+header,+title,+title,+vector,+axis_limit,+axis_limit,
        +title,+list(vector),+axis_limit,+axis_limit,
        +list(attributes),+footer,+smooth).

:- comment(
graph_w1(Header,GT,XT,XV,XMax,XMin,YT,YVs,YMax,YMin,LAtts,Footer,Smooth),

	"This predicate is quite similar to @pred{graph_b1/13}, the
	differences between them are listed below:

        @begin{itemize}

        @item The plotting area background color is white.  

        @item The cross hairs color is black.

        @end{itemize}
  
        Example:
@begin{verbatim}
graph_w1('This is the header text',
  'Graph_title',
  'xaxistitle',
  [20,10,59],
  100,
  10,      
  'yaxixtitle',
  [[10,35,40],[25,20,60]],
  _,
  _,
  [['element1','Blue','Yellow'],['element2','Turquoise','Plum']],
  'footer',
  'quadratic').
@end{verbatim}

").

graph_w1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth):-
	srandom(_),
	not_empty(XVector,YVectors,LinesAttributes,'graph_w1/13'),
	vectors_format(XVector,YVectors,LinesAttributes,'graph_w1/13'),
	show_graph_w1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth).

:- pred scattergraph_b1(+header,+title,+title,+vector,+title,+list(vector),
        +list(attributes),+footer).

:- comment(
scattergraph_b1(Header,GTitle,XTitle,XVector,YTitle,YVectors,PAtts,Footer), 

        "Apart from the features brought up at the beginning of the
	chapter, the scatter graph displayed invoking this predicate has
	the following characteristics:

        @begin{itemize}

        @item The plotting area background color is black.  

        @item The cross hairs color is white.

        @item The axes limits are determined from the data.  

        @end{itemize}

        Example:
@begin{verbatim}
scattergraph_b1('This is the header text',
  'Graph_title',
  'xaxistitle',
  [10,15,20],
  'yaxixtitle',
  [[10,35,20],[15,11,21]],
  [['element1','Blue','Yellow'],['element2','Turquoise','Plum']],
  'footer').
@end{verbatim}

").

scattergraph_b1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,PointAttributes,Footer):-
	srandom(_),
	not_empty(XVector,YVectors,PointAttributes,'scattergraph_b1/8'),
	vectors_format(XVector,YVectors,PointAttributes,'scattergraph_b1/8'),
	show_scatter_b1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,PointAttributes,Footer).

:- pred scattergraph_b1(+header,+title,+title,+vector,+axis_limit,+axis_limit,
	+title,+list(vector),+axis_limit,+axis_limit,+list(attributes),
	+footer).

:- comment(
scattergraph_b1(Header,GT,XT,XV,XMax,XMin,YT,YVs,YMax,YMin,PAtts,Footer),

   "The particular features related to this predicate are described below:

   @begin{itemize}

   @item The plotting area background color is black.  

   @item The cross hairs color is white.

   @item You can set the minimum and maximum limits of the axes. Data outside
   the limits will not be plotted.

   @end{itemize}

   Example:
@begin{verbatim}
scattergraph_b1('This is the header text',
  'Graph_title',
  'xaxistitle',
  [20,10,59],
  50,
  _,
  'yaxixtitle',
  [[10,35,40],[25,50,60]],
  50,
  _,
  [['point dataset1','Blue','Yellow'],['point dataset2']],
  'footer').
@end{verbatim}

").

scattergraph_b1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer):-
	srandom(_),
	not_empty(XVector,YVectors,PointAttributes,'scattergraph_b1/12'),
	vectors_format(XVector,YVectors,PointAttributes,'scattergraph_b1/12'),
	show_scatter_b1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer).

:- pred scattergraph_w1(+header,+title,+title,+vector,+title,+list(vector),
	+list(attributes),+footer).

:- comment(
scattergraph_w1(Header,GT,XT,XVector,YT,YVectors,PAtts,Footer),

	"This predicate is quite similar to @pred{scattergraph_b1/8} except
	in the following:

        @begin{itemize}

        @item The plotting area background color is black.  

        @item The cross hairs color is white.

        @item If the user does not fix the points colors, they will be chosen
        among the lighter ones.

        @end{itemize}
        
Example:
@begin{verbatim}
scattergraph_w1('This is the header text',
  'Graph_title',
  'xaxistitle',
  [20,10,59],
  'yaxixtitle',
  [[10,35,40],[25,20,60]],
  [['e1','Blue','Green'],['e2','MediumVioletRed','Plum']],
  'footer').
@end{verbatim}

        
"). 


scattergraph_w1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,PointAttributes,Footer):-
	srandom(_),
	not_empty(XVector,YVectors,PointAttributes,'scattergraph_w1/8'),
	vectors_format(XVector,YVectors,PointAttributes,'scattergraph_w1/8'),
	show_scatter_w1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,PointAttributes,Footer).

:- pred scattergraph_w1(+header,+title,+title,+vector,+axis_limit,+axis_limit,
	+title,+list(vector),+axis_limit,+axis_limit,+list(attributes),
	+footer).

:- comment(
scattergraph_w1(Header,GT,XT,XV,XMax,XMin,YT,YVs,YMax,YMin,PAtts,Footer), 

	"This predicate is quite similar to @pred{scattergraph1_b1/13}, the
	differences between them are listed below:

        @begin{itemize}

        @item The plotting area background color is white.  

        @item The cross hairs color is black.

        @end{itemize}
  
        Example:
@begin{verbatim}
scattergraph_w1('This is the header text',
  'Graph_title',
  'xaxistitle',
  [20,10,59],
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

scattergraph_w1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer):-
	srandom(_),
	not_empty(XVector,YVectors,PointAttributes,'scattergraph_w1/12'),
	vectors_format(XVector,YVectors,PointAttributes,'scattergraph_w1/12'),
	show_scatter_w1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer).


:- pred show_graph_b1/9.

show_graph_b1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth):-
	new_interp(Interp),
	begin_graph_b1(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	connect_lines(Smooth,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_b(XVector,YVectors,LinesAttributes,Interp).


:- pred show_graph_b1/13.

show_graph_b1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth):-
	new_interp(Interp),
	begin_graph_b1(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	connect_lines(Smooth,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_b(XVector,XMax,XMin,YVectors,YMax,YMin,LinesAttributes,Interp).

:- pred show_graph_w1/9.

show_graph_w1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,LinesAttributes,Footer,Smooth):-
	new_interp(Interp),
	begin_graph_w1(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	connect_lines(Smooth,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_w(XVector,YVectors,LinesAttributes,Interp).

:- pred show_graph_w1/13.

show_graph_w1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,LinesAttributes,Footer,Smooth):-
	new_interp(Interp),
	begin_graph_w1(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	connect_lines(Smooth,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_w(XVector,XMax,XMin,YVectors,YMax,YMin,LinesAttributes,Interp).

:- pred show_scatter_b1/8.

show_scatter_b1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,PointAttributes,Footer):-
	new_interp(Interp),
	begin_scatter_b1(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_b(XVector,YVectors,PointAttributes,Interp).

:- pred show_scatter_b1/12.

show_scatter_b1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer):-
	new_interp(Interp),
	begin_scatter_b1(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_b(XVector,XMax,XMin,YVectors,YMax,YMin,PointAttributes,Interp).



:- pred show_scatter_w1/8.

show_scatter_w1(Header,GraphTitle,XaxisTitle,XVector,YaxisTitle,YVectors,PointAttributes,Footer):-
	new_interp(Interp),
	begin_scatter_w1(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_w(XVector,YVectors,PointAttributes,Interp).

:- pred show_scatter_w1/12.

show_scatter_w1(Header,GraphTitle,XaxisTitle,XVector,XMax,XMin,YaxisTitle,YVectors,YMax,YMin,PointAttributes,Footer):-
	new_interp(Interp),
	begin_scatter_w1(GraphTitle,XaxisTitle,YaxisTitle,Interp),
	header(Header,Interp),
	footer(Footer,Interp),
	drawlines_w(XVector,XMax,XMin,YVectors,YMax,YMin,PointAttributes,Interp).


%%begin_graph_b1(GraphTitle,XaxisTitle,YaxisTitle,Interp).
begin_graph_b1(GraphTitle,XaxisTitle,YaxisTitle,Interp):-
	name('package require BLT',G1),
	tcltk_raw_code(G1,Interp),
	name('if { $tcl_version >= 8.0 } {',G2),
	tcltk_raw_code(G2,Interp),
	name('namespace import blt::*',G3),
	tcltk_raw_code(G3,Interp),
	name('namespace import -force blt::tile::* }',G4),
	tcltk_raw_code(G4,Interp),
%%	name('bltdebug 1',G5),  %%switches the debugger on 
%%	tcltk_raw_code(G5,Interp),
	name('set tcl_precision 15 ',G6),
	tcltk_raw_code(G6,Interp),
	name('set graph .graph',G7),
	tcltk_raw_code(G7,Interp),
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

%%begin_graph_w1(GraphTitle,XaxisTitle,YaxisTitle,Interp).
begin_graph_w1(GraphTitle,XaxisTitle,YaxisTitle,Interp):-
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

%%begin_scatter_b1(GraphTitle,XaxisTitle,YaxisTitle,Interp).
begin_scatter_b1(GraphTitle,XaxisTitle,YaxisTitle,Interp):-
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
	name('option add *Element.Smooth	natural',G29),
	tcltk_raw_code(G29,Interp),
	name('option add *Symbol	square',G30),
	tcltk_raw_code(G30,Interp),
	name('option add *Element.LineWidth	0',G31),
	tcltk_raw_code(G31,Interp),
	name('#option add *Pen.LineWidth		1',G32),
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


%%begin_scatter_w1(GraphTitle,XaxisTitle,YaxisTitle,Interp).
begin_scatter_w1(GraphTitle,XaxisTitle,YaxisTitle,Interp):-
	name('package require BLT',G1),
	tcltk_raw_code(G1,Interp),
	name('if { $tcl_version >= 8.0 } {',G2),
	tcltk_raw_code(G2,Interp),
	name('namespace import blt::*',G3),
	tcltk_raw_code(G3,Interp),
	name('namespace import -force blt::tile::* }',G4),
	tcltk_raw_code(G4,Interp),
%%	name('bltdebug 1',G5),        %%switch on the debugger
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
	name('option add *Element.Smooth	natural',G29),
	tcltk_raw_code(G29,Interp),
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




:- pred connect_lines(smooth,Interp).

connect_lines('linear',Interp):-
	name('option add *Element.Smooth	linear',S),
	tcltk_raw_code(S,Interp).
connect_lines('cubic',Interp):-
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

drawlines_b(XVector,YVectors,LinesAttributes,Interp):-
	xvalues(XVector,Interp),
	yvalues1(YVectors,1,Interp),
	attributes_b(LinesAttributes,Interp),
	loop_xy(Interp),
	table(Interp).

:-pred drawlines_b/8.

drawlines_b(XVector,XMax,XMin,YVectors,YMax,YMin,LinesAttributes,Interp):-
	maxming_x(XMax,XMin,Interp),
	maxming_y(YMax,YMin,Interp),
	xvalues(XVector,Interp),
	yvalues1(YVectors,1,Interp),
	attributes_b(LinesAttributes,Interp),
	loop_xy(Interp),
	table(Interp).

:- pred drawlines_w/4.

drawlines_w(XVector,YVectors,LinesAttributes,Interp):-
	xvalues(XVector,Interp),
	yvalues1(YVectors,1,Interp),
	attributes_w(LinesAttributes,Interp),
	loop_xy(Interp),
	table(Interp).

:-pred drawlines_w/8.

drawlines_w(XVector,XMax,XMin,YVectors,YMax,YMin,LinesAttributes,Interp):-
	maxming_x(XMax,XMin,Interp),
	maxming_y(YMax,YMin,Interp),
	xvalues(XVector,Interp),
	yvalues1(YVectors,1,Interp),
	attributes_w(LinesAttributes,Interp),
	loop_xy(Interp),
	table(Interp).

:- comment(hide,maxming_x/3).

:- pred maxming_x/3.

maxming_x(XMax,XMin,Interp):-
	nonvar(XMax),
	var(XMin),
	name('.graph axis configure x -max ',M1),
	number_codes(XMax,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp).
maxming_x(XMax,XMin,Interp):-
	var(XMax),
	nonvar(XMin),
	name('.graph axis configure x -min ',M1),
	number_codes(XMin,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp).
maxming_x(XMax,XMin,Interp):-
	nonvar(XMax),
	nonvar(XMin),
	name('.graph axis configure x -max ',M1),
	number_codes(XMax,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp),
	name('.graph axis configure x -min ',M4),
	number_codes(XMin,M5),
	append(M4,M5,M6),
	tcltk_raw_code(M6,Interp).
maxming_x(_XMax,_XMin,_Interp).


:- comment(hide,maxming_y/3).

:- pred maxming_y/3.

maxming_y(YMax,YMin,Interp):-
	nonvar(YMax),
	var(YMin),
	name('.graph axis configure y -max ',M1),
	number_codes(YMax,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp).
maxming_y(YMax,YMin,Interp):-
	var(YMax),
	nonvar(YMin),
	name('.graph axis configure y -min ',M1),
	number_codes(YMin,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp).
maxming_y(YMax,YMin,Interp):-
	nonvar(YMax),
	nonvar(YMin),
	name('.graph axis configure y -max ',M1),
	number_codes(YMax,M2),
	append(M1,M2,M3),
	tcltk_raw_code(M3,Interp),
	name('.graph axis configure y -min ',M4),
	number_codes(YMin,M5),
	append(M4,M5,M6),
	tcltk_raw_code(M6,Interp).
maxming_y(_YMax,_YMin,_Interp).



%%xvalues(XVector,Interp).
xvalues(XVector,Interp):-	
	name('vector X',X1),
	tcltk_raw_code(X1,Interp),
	name('X set {',X2),
	tcltk_raw_code(X2,Interp),
	xvalueslist(XVector,XValuesList1),
	list_concat(XValuesList1,XValuesList2),
	tcltk_raw_code(XValuesList2,Interp).
%%xvalues(XVector,Interp).
xvalueslist([],[[125]]).  %%[125] = }
xvalueslist([X1|X1s],[X2|X2s]):-
	name(X1,N1),
	append(N1,[32],X2),
	xvalueslist(X1s,X2s).

%%yvalues1(YVectors,Number,Interp).
yvalues1([],_Number,_Interp).
yvalues1([YValues|YVs],Number1,Interp):-
	yvalues2(YValues,Number1,Interp),
	Number2 is Number1 + 1,
	yvalues1(YVs,Number2,Interp).
	
%%yvalues2(YValues,NumberBars,Number1,Interp):-
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
	

%%yvalueslist(YValues,YValuesList),
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

attributes1_b([[ElementName,Symbol,Size]|As],Number,Interp):-
	nonvar(Symbol),
	nonvar(Size),
        symbol(Symbol),
	number(Size),
	!,
	number_codes(Number,NLine),
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	random_lightcolor(OC),
	name(OC,O1),
	random_lightcolor(C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
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
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorb(OutLine,OC),
	name(OC,O1),
	colorb(Color,C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_b(As,Number2,Interp).

attributes1_b([[ElementName]|As],Number,Interp):-
	number_codes(Number,NLine),
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	random_lightcolor(OC),
	name(OC,O1),
	random_lightcolor(C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
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
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorb(OutLine,OC),
	name(OC,O1),
	colorb(Color,C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
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
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorb(OutLine,OC),
	name(OC,O1),
	colorb(Color,C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
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

attributes1_w([[ElementName,Symbol,Size]|As],Number,Interp):-
	nonvar(Symbol),
	nonvar(Size),
	symbol(Symbol),
	number(Size),
	!,
	number_codes(Number,NLine),
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	random_darkcolor(OC),
	name(OC,O1),
	random_darkcolor(C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
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
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorw(OutLine,OC),
	name(OC,O1),
	colorw(Color,C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_w(As,Number2,Interp).



attributes1_w([[ElementName]|As],Number,Interp):-
	number_codes(Number,NLine),
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	random_darkcolor(OC),
	name(OC,O1),
	random_darkcolor(C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
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
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorw(OutLine,OC),
	name(OC,O1),
	colorw(Color,C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
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
	append([89],NLine,NLine1),  %%[89] = 'Y'
	name(ElementName,EN),
	add_doublequotes(EN,EN1),
	colorw(OutLine,OC),
	name(OC,O1),
	colorw(Color,C),
	name(C,C1),
	append(NLine1,[32],NLine2),
	append(EN1,[32],EN2),
	append(O1,[32],O2),
	append(C1,[32],C2),
	append(EN2,NLine2,EN2NLine2),
	append(O2,C2,O2C2),
	append(EN2NLine2,O2C2,Attr),
	name(' square 1 ',PointFeatures),
	append(Attr,PointFeatures,Attributes),
	tcltk_raw_code(Attributes,Interp),
	Number2 is Number + 1,
	attributes1_w(As,Number2,Interp).

:- comment(hide,colorb/2).

colorb(Color,Color):-
	var(Color),
	!,
	random_lightcolor(Color).
colorb(Color,C):-
	color(Color,C).

:- comment(hide,colorw/2).

colorw(Color,Color):-
	var(Color),
	!,
	random_darkcolor(Color).
colorw(Color,C):-
	color(Color,C).



%%loop_xy(Interp).
loop_xy(Interp):-
	name('foreach {label yData outline color symbol pixels} $attributes {',L1),
	tcltk_raw_code(L1,Interp),
	name('.graph element create $label -x X -y $yData -outline $outline -color $color -symbol $symbol -pixels $pixels}',L2),
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

:- comment(doinclude,symbol/1).

:- regtype symbol/1.

symbol(square).
symbol(circle).
symbol(diamond).
symbol(plus).
symbol(cross).
symbol(splus).
symbol(scross).
symbol(triangle).

:- comment(symbol(Symbol)," @includedef{symbol/1} @var{Symbol} stands for
	the shape of the points whether in scatter graphs or in line
	graphs.

").  

:- comment(doinclude,size/1).

:- regtype size/1.

size(Size) :-
	number(Size).

:- comment(size(Size)," @includedef{size/1} @var{Size} stands for the size in
	pixels of the points whether in scatter graphs or in line graphs.

").
:- comment(doinclude,attributes/1).

:- regtype attributes/1.

attributes([ElementName]):-
	atomic(ElementName).
attributes([ElementName,OutLine,Color]):-
	atomic(ElementName),
	color(OutLine),
	color(Color).
attributes([ElementName,Symbol,Size]):-
	atomic(ElementName),
	symbol(Symbol),
	size(Size).
attributes([ElementName,OutLine,Color,Symbol,Size]):-
	atomic(ElementName),
	color(OutLine),
	color(Color),
	symbol(Symbol),
	size(Size).

:- comment(attributes/1,"@includedef{attributes/1}

       Each line or point dataset in the graph has its own attributes,
       which are defined by this type. The name of the dataset, specified
       in the @var{ElementName} argument, may be either a number or an
       atom. The second argument is the color of a thin line around each
       point in the dataset and the Color argument is the points and lines
       color. Both @var{OutLine} and @var{Color} must be a valid color (see
       available values in @pred{color/1}), otherwise a random color
       according to the plot background color will be selected. The
       @var{Symbol} must be a valid symbol and the @var{Size} must be a
       number. Be careful if you want to especify the @var{Symbol} and the
       @var{Size}, otherwise the predicate will not work as you expect. If
       you don't select a symbol and a size for a dataset the default
       values will be square and 1 pixel.

"). 

:- pop_prolog_flag(multi_arity_warnings).

:- comment(doinclude,vector/1).

:- regtype vector/1.

vector(X):-
	list(X,number).

:- comment(vector/1,"@includedef{vector/1} The type vector defines a list
	of numbers (integers or floats).


").



:- comment(doinclude,smooth/1).

:- regtype smooth/1.

smooth('linear').
smooth('cubic').
smooth('quadratic').
smooth('step').

	
:- comment(smooth(Smooth)," @includedef{smooth/1}

	Specifies how connecting segments are drawn between data points. If
        @var{Smooth} is @tt{linear}, a single line segment is drawn,
        connecting both data points. When @var{Smooth} is @tt{step}, two
        line segments will be drawn, the first line is a horizontal line
        segment that steps the next X-coordinate and the second one is a
        vertical line, moving to the next Y-coordinate. Both @tt{cubic} and
        @tt{quadratic} generate multiple segments between data points. If
        @tt{cubic}is used, the segments are generated using a cubic
        spline. If @tt{quadratic}, a quadratic spline is used. The default
        is linear.

").
