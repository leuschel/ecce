:- module( vrml_communication, 
	[vrml_connection/1,
	 vrml_connection/2,
	 view_code/3,
	 close_viewer/2,
	 remove_viewer/3,
	 get_viewer/2,
	 get_viewer/3,
	 change_code/3,
	 rotate_code/3,
	 rotate_back/2,
	 rotate_forth/2,
	 get_viewer_list/2] ).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- use_module(vrml_socket_communication).
:- use_module(internal_types).

:- comment(title,"The VRML interface").

:- comment(module,"This module provides the predicates which makes the
                   interface to the vindow predicates and to the vrml-browser
                   predicates.

                   Implemented by G@..{o}ran Smedb@..{a}ck").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred new_viewer(+Vrml_structure, -Vrml_structure)
   :: vrml * vrml
   # "This predicate will construct a new viewer, in the sense that virtually
      construct it, because we do not construct until we do the command 
      view_code/3.".

new_viewer(vrml(Stream, [viewer(Latest,_)|Viewers]),
	   vrml(Stream, [viewer(New,_),viewer(Latest,_)|Viewers])):-
          New is Latest + 1.

new_viewer(vrml(Stream, []), vrml(Stream, [viewer(0,_)])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred view_code(+Vrml_structure, +VRML_code, -Vrml_structure)
  :: vrml * string * vrml
  # "This predicate will construct a new browser window with the given code,
    and then return the VRML structure.".

view_code(vrml(Stream,Viewers), Code, New_structure) :-
	new_viewer(vrml(Stream, Viewers), New_structure),
	socket_call(Stream, code(Code)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred change_code(+Vrml_structure, +Viewer, +Code)
   :: vrml * viewer * string
   # "This predicate will change code in a viewer, the different scenes
      will be stored and can be changed with rotate_code/3.".

change_code(vrml(Stream,_), viewer(N,_), Code) :-
	socket_call(Stream, change(N, Code)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred rotate_code(+Vrml_structure, +Viewer, +Scene_number)
   :: vrml * viewer * int 
 #  "This predicate will rotate code, changing the displayed code to 
     a scene previously displayed. The number of the scene is required.".

rotate_code(vrml(Stream,_), viewer(N,_), Scene) :-
	socket_call(Stream, rotate(N, Scene)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred rotate_forth(+Vrml_structure, +Viewer)
   :: vrml * viewer 
 #  "This predicate will rotate code to next comming scene.
     Changing the displayed code to 
     a scene previously displayed. It will wrap around.".

rotate_forth(vrml(Stream,_), viewer(N,_)) :-
	socket_call(Stream, rotate_forth(N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred rotate_back(+Vrml_structure, +Viewer)
   :: vrml * viewer 
  #  "This predicate will rotate code to the scene prior.
     Changing the displayed code to 
     a scene previously displayed. It will wrap around.".


rotate_back(vrml(Stream,_), viewer(N,_)) :-
	socket_call(Stream, rotate_back(N)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrml_connection(-Vrml_structure)
   :: vrml(stream, list(viewer))
   # "This predicate opens a connection to a VRML environment, returning
      a structure which gives access to the communication.".

vrml_connection(vrml(Stream,[])) :-
	open_connection(Stream).

vrml_connection(vrml(Stream,[]), Port) :-
	open_connection(Stream, Port).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrml_exit(+Vrml_structure)
   :: vrml(stream, list(viewer))
   # "This predicate terminates the contact to the VRML environment. ".

vrml_exit(vrml(Stream,_)) :-
	socket_call(Stream, exit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_viewer(vrml(Stream, Viewers), viewer(Number,_),vrml(Stream, NewView)) :-
	remove(Viewers, viewer(Number,_), NewView),
	close_viewer(vrml(Stream, _), viewer(Number,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close_viewer(vrml(Stream, _), viewer(Number,_)) :-
	socket_call(Stream, close(Number)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove([], _, _) :-
	!,
	fail.

remove([viewer(Number,_)|Rest], viewer(Number,_), Rest).

remove([Wrong|Rest], viewer(N,_), [Wrong|Prev]) :-
	remove(Rest, viewer(N,_), Prev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch([], _, _) :-
	!,
	fail.

fetch([viewer(Number,C)|_Rest], viewer(Number,_CodeList), viewer(Number,C)).

fetch([_Wrong|Rest], viewer(N,_), Ans) :-
	fetch(Rest, viewer(N,_), Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_stream(vrml(Stream,_), Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_viewer_list(vrml(_,Viewers), Viewers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_viewer( viewer(_, [First|_] ) , First ).

get_viewer(vrml(_,Viewers), viewer(Num,_), Res) :-
	fetch(Viewers, viewer(Num,_), Res).

get_viewer(vrml(_,Viewers), Num, Res) :-
	get_viewer_from_list(Viewers, Num, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_viewer_from_list([],_,_) :-
	!,
	fail.

get_viewer_from_list([viewer(Num,C)|_], Num, viewer(Num,C)).

get_viewer_from_list([_V|R],Num,Ans) :-
	get_viewer_from_list(R,Num,Ans).
