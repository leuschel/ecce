:- module(color_pattern,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").


:- export(color/1).
:- export(color/2).
:- export(pattern/1).
:- export(pattern/2).
:- export(random_color/1).
:- export(random_lightcolor/1).
:- export(random_darkcolor/1).
:- export(random_pattern/1).

:- use_module(library(lists)).
:- use_module(library(random)).

:- push_prolog_flag(multi_arity_warnings,off).

:- comment(title,"Color and Pattern Library").


:- comment(module,"This module contains predicates to access and check
   conformance to the available colors and patterns.").

:- pred pattern(P1,P2) : pattern(P1) => pattern(P2) # "Test whether the
	pattern @var{P1} is a valid pattern or not. If @var{P1} is a
	variable the predicate will choose a valid pattern randomly. If
	@var{P1} is a ground term that is not a valid pattern an exception
	(error10) will be thrown.".

pattern(X,X):-
	var(X),
	random_pattern(X).
pattern(X,X) :-
	atomic(X),
	pattern(X),
	!.
pattern(X,X):-
	atomic(X),
	!,
	throw(error(chartlib,error10,pattern/2)),
	fail.


:- regtype pattern/1.

pattern(pattern1).
pattern(pattern2).
pattern(pattern3).
pattern(pattern4).
pattern(pattern5).
pattern(pattern6).
pattern(pattern7).
pattern(pattern8).
pattern(pattern9).

:- comment(pattern(Pattern),"@includedef{pattern/1}Defines valid patterns
	used in the stipple style bar attribute.

").

:- pred color(C1,C2) 
	: color(C1) => color(C2) 

	# "Test whether the color @var{C1} is a valid color or not. If
	@var{C1} is a variable the predicate will choose a valid color
	randomly. If @var{C1} is a ground term that is not a valid color an
	exception (error9) will be thrown".

color(X,X):-
	var(X),
	random_color(X).
color(X,X) :-
	atomic(X),
	color(X),
	!.
color(X,X):-
	atomic(X),
	!,
	throw(error(chartlib,error9,color/2)),
	fail.

	

:- regtype color/1.

color('GreenYellow').
color('Yellow').
color('White').
color('Wheat').
color('BlueViolet').
color('Violet').
color('MediumTurquoise').
color('DarkTurquoise').
color('Turquoise').
color('Thistle').
color('Tan').
color('Sienna').
color('Salmon').
color('VioletRed').
color('OrangeRed').
color('MediumVioletRed').
color('IndianRed').
color('Red').
color('Plum').
color('Pink').
color('MediumOrchid').
color('DarkOrchid').
color('Orchid').
color('Orange').
color('Maroon').
color('Magenta').
color('Khaki').
color('Grey').
color('LightGray').
color('DimGray').
color('DarkSlateGray').
color('YellowGreen').
color('SpringGreen').
color('SeaGreen').
color('PaleGreen').
color('MediumSpringGreen').
color('MediumSeaGreen').
color('LimeGreen').
color('ForestGreen').
color('DarkOliveGreen').
color('DarkGreen').
color('Green').
color('Goldenrod').
color('Gold').
color('Brown').
color('Firebrick').
color('Cyan').
color('Coral').
color('SteelBlue').
color('SlateBlue').
color('SkyBlue').
color('Navy').
color('MidnightBlue').
color('MediumSlateBlue').
color('MediumBlue').
color('LightSteelBlue').
color('LightBlue').
color('DarkSlateBlue').
color('CornflowerBlue').
color('CadetBlue').
color('Blue').
color('Black').
color('MediumAquamarine').
color('Aquamarine').

:- comment(color(Color),"@includedef{color/1}Defines available colors for
	elements such as points, lines or bars.

").


:- pred random_color(+color).

:- comment(random_color(Color),
	"This predicate choose a valid color among the availables randomly.

"). 

random_color(Color):-
	random(1,64,Number),
	random_color(Number,Color).

random_color(1,'GreenYellow').
random_color(2,'Yellow').
random_color(3,'White').
random_color(4,'Wheat').
random_color(5,'BlueViolet').
random_color(6,'Violet').
random_color(7,'MediumTurquoise').
random_color(8,'DarkTurquoise').
random_color(9,'Turquoise').
random_color(10,'Thistle').
random_color(11,'Tan').
random_color(12,'Sienna').
random_color(13,'Salmon').
random_color(14,'VioletRed').
random_color(15,'OrangeRed').
random_color(16,'MediumVioletRed').
random_color(17,'IndianRed').
random_color(18,'Red').
random_color(19,'Plum').
random_color(20,'Pink').
random_color(21,'MediumOrchid').
random_color(22,'DarkOrchid').
random_color(23,'Orchid').
random_color(24,'Orange').
random_color(25,'Maroon').
random_color(26,'Magenta').
random_color(27,'Khaki').
random_color(28,'Grey').
random_color(29,'LightGray').
random_color(30,'DimGray').
random_color(31,'DarkSlateGray').
random_color(32,'YellowGreen').
random_color(33,'SpringGreen').
random_color(34,'SeaGreen').
random_color(35,'PaleGreen').
random_color(36,'MediumSpringGreen').
random_color(37,'MediumSeaGreen').
random_color(38,'LimeGreen').
random_color(39,'ForestGreen').
random_color(40,'DarkOliveGreen').
random_color(41,'DarkGreen').
random_color(42,'Green').
random_color(43,'Goldenrod').
random_color(44,'Gold').
random_color(45,'Brown').
random_color(46,'Firebrick').
random_color(47,'Cyan').
random_color(48,'Coral').
random_color(49,'SteelBlue').
random_color(50,'SlateBlue').
random_color(51,'SkyBlue').
random_color(52,'Navy').
random_color(53,'MidnightBlue').
random_color(54,'MediumSlateBlue').
random_color(55,'MediumBlue').
random_color(56,'LightSteelBlue').
random_color(57,'LightBlue').
random_color(58,'DarkSlateBlue').
random_color(59,'CornflowerBlue').
random_color(60,'CadetBlue').
random_color(61,'Blue').
random_color(62,'Black').
random_color(63,'MediumAquamarine').
random_color(64,'Aquamarine').
random_color(_,'Blue').


:- pred random_pattern(-pattern).

:- comment(random_pattern(Pattern),
	"This predicate choose a valid pattern among the availables randomly.

").

random_pattern(Pattern):-
	random(1,9,Number),
	random_pattern(Number,Pattern).

random_pattern(1,pattern1).
random_pattern(2,pattern2).
random_pattern(3,pattern3).
random_pattern(4,pattern4).
random_pattern(5,pattern5).
random_pattern(6,pattern6).
random_pattern(7,pattern7).
random_pattern(8,pattern8).
random_pattern(9,pattern9).
random_pattern(_,pattern1).



:- pred random_lightcolor(-color).

:- comment(random_lightcolor(Color), "This predicate choose a valid light
	color among the availables randomly. 

"). 


random_lightcolor(Color):-
	random(1,48,Number),
	random_lightcolor(Number,Color).

random_lightcolor(1,'GreenYellow').
random_lightcolor(2,'Yellow').
random_lightcolor(3,'White').
random_lightcolor(4,'DarkOliveGreen').
random_lightcolor(5,'Violet').
random_lightcolor(6,'MediumTurquoise').
random_lightcolor(7,'DarkTurquoise').
random_lightcolor(8,'Turquoise').
random_lightcolor(9,'Thistle').
random_lightcolor(10,'Tan').
random_lightcolor(11,'Salmon').
random_lightcolor(12,'VioletRed').
random_lightcolor(13,'OrangeRed').
random_lightcolor(14,'CornflowerBlue').
random_lightcolor(15,'IndianRed').
random_lightcolor(16,'Red').
random_lightcolor(17,'Plum').
random_lightcolor(18,'LightBlue').
random_lightcolor(19,'Orchid').
random_lightcolor(20,'PaleGreen').
random_lightcolor(21,'Maroon').
random_lightcolor(22,'Magenta').
random_lightcolor(23,'Khaki').
random_lightcolor(24,'Grey').
random_lightcolor(25,'MediumVioletRed').
random_lightcolor(26,'YellowGreen').
random_lightcolor(27,'SpringGreen').
random_lightcolor(28,'SeaGreen').
random_lightcolor(29,'Orange').
random_lightcolor(30,'SteelBlue').
random_lightcolor(31,'MediumSeaGreen').
random_lightcolor(32,'LimeGreen').
random_lightcolor(33,'ForestGreen').
random_lightcolor(34,'Wheat').
random_lightcolor(35,'DarkGreen').
random_lightcolor(36,'Green').
random_lightcolor(37,'Goldenrod').
random_lightcolor(38,'MediumAquamarine').
random_lightcolor(39,'Firebrick').
random_lightcolor(40,'Cyan').
random_lightcolor(41,'Coral').
random_lightcolor(42,'MediumSpringGreen').
random_lightcolor(43,'SkyBlue').
random_lightcolor(44,'MediumSlateBlue').
random_lightcolor(45,'LightSteelBlue').
random_lightcolor(46,'Pink').
random_lightcolor(47,'Gold').
random_lightcolor(48,'CadetBlue').




:- pred random_darkcolor(-color).

:- comment(random_darkcolor(Color), "This predicate choose a valid dark
	color among the availables randomly. 

"). 

random_darkcolor(Color):-
	random(1,54,Number),
	random_darkcolor(Number,Color).

random_darkcolor(1,'BlueViolet').
random_darkcolor(2,'Violet').
random_darkcolor(3,'MediumTurquoise').
random_darkcolor(4,'DarkTurquoise').
random_darkcolor(5,'Turquoise').
random_darkcolor(6,'Tan').
random_darkcolor(7,'DarkGreen').
random_darkcolor(8,'Salmon').
random_darkcolor(9,'VioletRed').
random_darkcolor(10,'OrangeRed').
random_darkcolor(11,'MediumVioletRed').
random_darkcolor(12,'SpringGreen').
random_darkcolor(13,'Red').
random_darkcolor(14,'Plum').
random_darkcolor(15,'Pink').
random_darkcolor(16,'Gold').
random_darkcolor(17,'DarkOrchid').
random_darkcolor(18,'Orchid').
random_darkcolor(19,'Orange').
random_darkcolor(20,'Aquamarine').
random_darkcolor(21,'Magenta').
random_darkcolor(22,'Grey').
random_darkcolor(23,'Coral').
random_darkcolor(24,'DarkSlateGray').
random_darkcolor(25,'YellowGreen').
random_darkcolor(26,'IndianRed').
random_darkcolor(27,'SeaGreen').
random_darkcolor(28,'PaleGreen').
random_darkcolor(29,'MediumSpringGreen').
random_darkcolor(30,'MediumSeaGreen').
random_darkcolor(31,'LimeGreen').
random_darkcolor(32,'ForestGreen').
random_darkcolor(33,'DarkOliveGreen').
random_darkcolor(34,'Sienna').
random_darkcolor(35,'Green').
random_darkcolor(36,'MediumSlateBlue').
random_darkcolor(37,'MediumOrchid').
random_darkcolor(38,'Brown').
random_darkcolor(39,'Firebrick').
random_darkcolor(40,'Cyan').
random_darkcolor(41,'DimGray').
random_darkcolor(42,'SteelBlue').
random_darkcolor(43,'SlateBlue').
random_darkcolor(44,'Navy').
random_darkcolor(45,'MidnightBlue').
random_darkcolor(46,'Goldenrod').
random_darkcolor(47,'MediumBlue').
random_darkcolor(48,'DarkSlateBlue').
random_darkcolor(49,'CornflowerBlue').
random_darkcolor(50,'CadetBlue').
random_darkcolor(51,'Blue').
random_darkcolor(52,'Black').
random_darkcolor(53,'MediumAquamarine').
random_darkcolor(54,'Maroon').
random_darkcolor(_,'SteelBlue').

:- pop_prolog_flag(multi_arity_warnings).
