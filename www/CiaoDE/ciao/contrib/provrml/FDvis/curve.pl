:- module(curve, [
	curve/3,
	curve/2,
	curve_to_file/2,
	max_test/4
	]).
	

:- include(library(assertions)).
%:- include(library(basicmodes)).
:- include(library(isomodes)).
%:- include(library(types)).

:- use_module(library(provrml)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- set_prolog_flag(write_strings,on).

:- comment(title,"Code creator for the constraints").

:- comment(module,"This module provides code generating predicates that
	           produces VRML terms from inputing Lists of values.
                   It is optimised for the same length of each and every 
	           list.

                   Implemented by G@..{o}ran Smedb@..{a}ck").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred curve(+Lists_of_variable_values, -List_of_vrml_terms)
   :: list(list(int)) * list(term)
   # "This predicate will construct the vrml-terminals that can be translated
      to VRML-code. The list contains lists of values, i.e. the values
      should be the number of possible values for a FD variable at each step.".

curve([First|Values], Code) :-
	count_elements([First|Values], Num_of_variables),
	count_elements(First, Num_of_points),
	create_curves([First|Values], Num_of_variables, Num_of_points, Code),
	writeq(Code).

curve_to_file(Values,FileName) :-
	curve(Values,Code),
	terms_to_vrml_file(Code,FileName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_curves(Values, Num_of_variables, Num_of_points, Code) :-
	normalise_values(Values, Norm_values),
	create_coordinates(Norm_values, Coords),
	create_index(Num_of_variables, Num_of_points, Index),
	create_faces(Coords, Index, Face_code),
	create_lines(Num_of_variables, Num_of_points, Coords, Line_code),
	complete_code([Line_code|Face_code],Num_of_variables, 
	                                           Num_of_points, Code).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_coordinates(Lists, Coords) :-
	create_coordinates_from_lists(Lists, 0, [], Coords).

%%%%%%%%%%%%%%%%
create_coordinates_from_lists([],_,R,R).

create_coordinates_from_lists([List|Rest], Z_pos, Inc, Result) :-
	create_coordinates_single(List, Z_pos, 0, Coords),
	append(Inc, Coords, Acc),
	Z_pos_new is Z_pos + 1,
	create_coordinates_from_lists(Rest, Z_pos_new, Acc, Result).
	
%%%%%%%%%%%%%%%%
create_coordinates_single([],_,_,[]).

create_coordinates_single([Y|Values], Z, X, Inc) :-
	X1 is X + 1,
	Z1 is Z + 1,
	Inc = [X,Y,Z,
               X,0,Z,
	       X,Y,Z1,
	       X,0,Z1,
	       X1,Y,Z1,
	       X1,0,Z1,
	       X1,Y,Z,
	       X1,0,Z | More_coords],
	create_coordinates_single(Values, Z, X1, More_coords).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_index(Number_of_variables, Length, index(Xz, Yz, Xy)) :-
	Total is Number_of_variables * Length,
	create_index_complete(0, Total, Xz, Yz, Xy).

%%%%%%%%%%%%%%%%
create_index_complete(N, N, [], [], []).

create_index_complete(N, Num_tot, Xz_inc, Yz_inc, Xy_inc) :-
	V  is N * 8,
	V1 is V + 1,
	V2 is V + 2,
	V3 is V + 3,
	V4 is V + 4,
	V5 is V + 5,
	V6 is V + 6, 
	V7 is V + 7,
	N_out is N + 1,
	Xz_inc = [V,V2,V4,V6,-1 |Xz_rest],
	Yz_inc = [V,V1,V3,V2,-1, V6,V7,V5,V4,-1|Yz_rest],
	Xy_inc = [V,V1,V7,V6,-1, V2,V3,V5,V4,-1|Xy_rest],
	create_index_complete(N_out, Num_tot, Xz_rest, Yz_rest, Xy_rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_faces(Coords, index(Xz,Yz,Xy), Face_code) :-
	Bright = [0,0,0],
	Middle = [0,1,0],
	Dark   = [1,1,1],
	create_face_code(cw, Coords, Xz, Middle, Xz_code),
	create_face_code(cw, Coords, Yz, Bright, Yz_code),
	create_face_code(ccw, Coords, Yz, Bright, Yz_ccode),
	create_face_code(cw, Coords, Xy, Dark, Xy_code),
	create_face_code(ccw, Coords, Xy, Dark, Xy_ccode),
	Face_code = [Xz_code,
	             Yz_code,
		     Yz_ccode,
		     Xy_code,
		     Xy_ccode].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_lines(Num_of_variables, Num_of_points, Coords, Line_code) :-
	create_line_index(Num_of_variables, Num_of_points, Line_index),
	create_line_code(Coords, Line_index, [0,0,0], Line_code).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
complete_code(Values, N_vars, N_points, Code) :-
	Head = '#VRML V2.0 utf8',
	create_coord_system(Coord),
	Code0 = [Coord|Values],
	%%Place_for_camera is N_vars // 2,
	%%create_animation(Place_for_camera, N_points, Anim_code),
        create_viewpoint(N_vars, N_points, View),
	Code = [Head, View, 'Transform'([children(Code0)])].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_line_index(Variables, Length, Index) :-
	create_line_index_complete(1, Variables, Length, [], Index).

%%%%%%%%%%%%%%%%
create_line_index_complete(N, N,_,I,I).

create_line_index_complete(Place, Total, Length, Inc, Index) :-
	Stop is Length - 1,
	create_line_index_one(Place, 0, Length, Stop, One),
	Place_new is Place + 1,
	append(Inc, One, Out),
	create_line_index_complete(Place_new, Total, Length, Out, Index).

%%%%%%%%%%%%%%%%
create_line_index_one(Place, 0, Length, Stop, [V1,V,V6,V8,-1|Index]) :-
	V is Place * Length * 8,
	V1 is V + 1,
	V6 is V + 6,
	V8 is V + 8,
	create_line_index_one(Place, 1, Length, Stop, Index).

create_line_index_one(Place, Stop, Length, Stop, [V,V6,V7,-1]) :-
	V is Place * Length * 8 + Stop * 8,
	V6 is V + 6,
	V7 is V + 7.

create_line_index_one(Place, X, Length, Stop, [V,V6,V8,-1|Index]) :-
	V is Place * Length * 8 + X * 8,
	V6 is V + 6,
	V8 is V + 8,
	X_new is X + 1,
	create_line_index_one(Place, X_new, Length, Stop, Index).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_line_code(Coord, Index, [R,G,B], Code) :-
Code = 'Shape'([
          appearance('Appearance'([
             material('Material'([
                ambientIntensity(0.8),
                emissiveColor(R,G,B),
		diffuseColor(R,G,B)]))])),
	  geometry('IndexedLineSet'([
	     coord('Coordinate'([
	        point(Coord)])),
	     color('NULL'),
             coordIndex(Index)]))]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_face_code(cw, Coord, Index, [R,G,B], Code) :-
Code = 
'Shape'([
      appearance('Appearance'([
         material('Material'([
            ambientIntensity(0.8),
            emissiveColor(R,G,B),
            diffuseColor(R,G,B)]))])),
         geometry('IndexedFaceSet'([
            coord('Coordinate'([
               point(Coord)])),
	    color('NULL'),
	    ccw('TRUE'),
	    coordIndex(Index),
	    normal('NULL')]))]).
   

create_face_code(ccw, Coord, Index, [R,G,B], Code) :-
Code = 
   'Shape'([
      appearance('Appearance'([
         material('Material'([
            ambientIntensity(0.8),
            emissiveColor(R,G,B),
            diffuseColor(R,G,B)]))])),
         geometry('IndexedFaceSet'([
            coord('Coordinate'([
               point(Coord)])),
	    color('NULL'),
	    ccw('FALSE'),
	    coordIndex(Index),
	    normal('NULL')]))]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_viewpoint(N_vars, N_points, Code ) :-
	X is N_points + 4,
	Z is N_vars + 4,
%	position 20.977186 9.352924 -6.6295433
%        orientation -0.18602566 0.9463393 0.26426595 2.6325657
	Code = 'DEF'(view,['Viewpoint'([
			position(X,5,Z),
			orientation(-0.2,1,0.25,2.6),
			fieldOfView(2.5)])]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_coord_system( P ) :-
    P =  'Shape'([
            appearance('Appearance'([
		material('Material'([
                    diffuseColor(1,1,1)]))])),
                geometry('IndexedLineSet'([
                   coord('Coordinate'([
                       point([0,0,0,100,0,0,0,100,0,0,0,100])])),
                   coordIndex([0,1,-1,0,2,-1,0,3,-1])]))]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
normalise_values(Lists, Norm_lists) :-
	get_max_value(Lists, Max),
	normalise_lists(Lists, Max, Norm_lists).

normalise_lists([],_,[]).
normalise_lists([L|Rest], Max, [L_norm|Rest_norm]) :-
	normalise_list(L, Max, L_norm),
	normalise_lists(Rest, Max, Rest_norm).

normalise_list([],_,[]).
normalise_list([Val|Rest], Max, [Val_norm|Rest_norm]) :-
	Val_norm is Val / Max,
	normalise_list(Rest, Max, Rest_norm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
count_elements(List, Elem) :-
	count_elements0(List, 0, Elem).

count_elements0([], N, N).

count_elements0([_|R],N, Elem) :-
	Num is N + 1,
	count_elements0(R,Num, Elem).

get_max_value(Values, Max) :-
	get_max_value(Values, 0, Max).


get_max_value([], M, M).
get_max_value([List|Rest], Max_so_far, Max) :-
	get_max(List, Max_so_far, Max_part),
	get_max_value(Rest, Max_part, Max).

get_max([], M, M).
get_max([V|Rest], Max_so_far, Max) :-
	( V > Max_so_far
	->
	get_max(Rest, V, Max)
	;
	get_max(Rest, Max_so_far, Max)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is predicates for the test predicates.

 %% create_number_list(0, []).
 %% create_number_list(N, [Value|Rest]) :-
 %% 	Num is N - 1,
 %% 	walltime(T0),
 %% 	time(T1),
 %% 	T2 is (T0 + 1) * N * T1,
 %% 	Value is ((T2 mod 83) + 1) / 8,
 %% 	create_number_list(Num, Rest).
 %% 
 %% create_number_lists(0,_,[]).
 %% create_number_lists(NrLists, Length, [List|NumberLists]) :-
 %% 	create_number_list(Length, List),
 %% 	NewNrLists is NrLists - 1,
 %% 	create_number_lists(NewNrLists, Length, NumberLists).
 %% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_animation(Z, Length, Code) :-
	create_key_list(Length, Key_list),
	create_pos_values(Z, Length, Pos_value_list),
	create_ori_values(Z, Length, Ori_value_list),
	Code = ['DEF'(pos,['PositionInterpolator'([
                   key([Key_list]),
                   keyValue([Pos_value_list])])]),
               'DEF'(ori,['OrientationInterpolator'([
		   key([Key_list]),
		   keyValue([Ori_value_list])])]),
               'DEF'(time,['TimeSensor'([
		   loop('TRUE'),
		   enabled('FALSE'),
		   startTime(1),
		   cycleInterval(20)])]),
	        'ROUTE'(time,fraction_changed,'TO',pos,set_fraction),
		'ROUTE'(time,fraction_changed,'TO',ori,set_fraction),
		'ROUTE'(pos,value_changed,'TO',view,position),
		'ROUTE'(ori,value_changed,'TO',view,orientation)].
              
create_key_list(Length, Key_list) :-
	L is Length * 2 - 1,
	create_key_list(0, L, Key_list).

create_key_list(N,N,[1]).
create_key_list(N, Length, [Val|Rest]) :-
	Val is N / Length,
	Num is N + 1,
	create_key_list(Num, Length, Rest).

create_pos_values(Z, Length, Pos_value_list) :-
	L is Length * 4,
	create_pos_values(Z, 0, L, Pos_value_list).

create_pos_values(_,L,L,[]).
create_pos_values(Z, N, Length, [X,Y,Z|Rest]) :-
	N_next is N + 1,
	X is N/4 + 4,
	Y is 2,
	create_pos_values(Z,N_next,Length,Rest).

create_ori_values(Z, Length, Ori_value_list) :-
	L is Length * 4,
	create_ori_values(Z, 0, L, Ori_value_list).

create_ori_values(_,L,L,[]).
create_ori_values(Z, N, Length, [0,1,0,1.51|Rest]) :-
	N_next is N + 1,
	create_ori_values(Z, N_next, Length, Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

curve(Num, Len, Code) :-
	%%%%create_number_lists(Num, Len, Values),
	curve([[1,2,3],[2,3,4]],Code).


 %% max_test(Nr, Length, Max, Lists) :-
 %% 	create_number_lists(Nr, Length, Lists),
 %% 	get_max_value(Lists, Max).
