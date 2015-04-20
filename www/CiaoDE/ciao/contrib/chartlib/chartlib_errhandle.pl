:- module(chartlib_errhandle,
	[chartlib_text_error_protect/1, chartlib_visual_error_protect/1],
	[assertions,regtypes,isomodes]).

%:- comment(author, "Isabel Martín").
:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").

:- comment(title,"Error Handler for Chartlib").

:- use_module(library('chartlib/bltclass')).
:- use_module(library('chartlib/install_utils')).

:- meta_predicate(chartlib_text_error_protect(goal)).
:- meta_predicate(chartlib_visual_error_protect(goal)).

:- comment(module,"This module is an error handler. If the format of the
	arguments is not correct in a call to a chartlib predicate an
	exception will be thrown . You can wrap the chartlib predicates
	with the predicates exported by this module to handle automatically
	the errors if any.

").

:- pred chartlib_text_error_protect(+callable).

:- comment(chartlib_text_error_protect(G),"This predicate catches the
	thrown exception and sends it to the appropiate handler. The
	handler will show the error message in the standard output.

").
chartlib_text_error_protect(G):-
	catch(G,E,chartlib_handle_error(E,text)).

:- pred chartlib_visual_error_protect(+callable).

:- comment(chartlib_visual_error_protect(G),"This predicate catches the
	thrown exception and sends it to the appropiate handler. The
	handler will pop up a message box.

").

chartlib_visual_error_protect(G):-
 	catch(G,E,chartlib_handle_error(E,visual)).


:- pred chartlib_handler_error(+term,+handler_type).

:- comment(chartlib_handler_error(Error,HandlerType),"Depending on the
	second paramenter value the error handler will do one of the
	following actions:

        @begin{itemize}

        @item If the handler type is text an error message in the standar
	outputwill be displayed. A briefly explanation about the error
	cause and the predicate in which it has been generated will be
	depicted.

        @item If the handler type is visual a message box will be poped up
	giving the user information about the error cause.  

").

chartlib_handle_error(error(chartlib,E,Predicate),text):-
	display('{ERROR: in '),
	display(Predicate),
	error_message(E,Msg),
	display(Msg),
	display('}').


chartlib_handle_error(error(chartlib,E,_Predicate),visual):-
	new_interp(Interp),
        error_dir(Dir),
	error_file(E,File),
	atom_concat(Dir,File,Path),
	interp_file(Path,Interp),
	fail.

:- comment(doinclude,handler_type/1).

:- regtype handler_type(X).

handler_type(text).
handler_type(visual).

:- comment(handler_type(X),"The library chartlib includes two error 
	handlers already programmed. @includedef{handler_type/1}

").

:- comment(doinclude,error_message/2).

:- pred error_message(+atm,+atm).

:- comment(error_message(ErrorCode,ErrorMessage),"Binds the error code with 
	its corresponding text message.

").
error_message(error1,' -- the vectors must contain the same number of elements').
error_message(error2,' -- one or both vectors are empty').
error_message(error3,' -- the bar attributes are not correct').
error_message(error4,' -- the vector formats are not correct').
error_message(error5,' -- the vector formats in genmultibar is not correct').
error_message(error6,' -- the bar attributes are not correct').
error_message(error7,' -- the element list is not correct').
error_message(error8,' -- the table elements are not in a correct format').
error_message(error9,' -- the color is not a valid color').
error_message(error10,' -- the pattern is not a valid pattern').

:- comment(doinclude,error_file/2).

:- pred error_file(+atm,+atm).

:- comment(error_file(ErrorCode,ErrorFile),"Binds the error code with 
	its corresponding script error file.

").

error_file(error1,'err_vec1').
error_file(error2,'err_vec2').
error_file(error3,'err_vec3').
error_file(error4,'err_vec4').
error_file(error5,'err_genmultibar').
error_file(error6,'err_genmultibar1').
error_file(error7,'err_list').
error_file(error8,'err_table').
error_file(error9,'err_color').
error_file(error10,'err_pattern').
