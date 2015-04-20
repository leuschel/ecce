
:- module(server,_,[persdb]).

:- use_module(library(lists),[length/2,list_concat/2,append/3]).
:- use_module(library(file_utils),[file_to_string/2]).
:- use_module(library('pillow/http_server'),[http_serve_fetch/2]).
:- use_module(library(sockets)).
:- use_module(library('sockets/sockets_io'),[serve_socket/3]).
:- use_module(library(system)).
:- use_module(library(write)).

persistent_dir(db,'./persdb').
dir(".").

:- persistent(received_message/2,db).
:- persistent(cached/1,db).

main([S]):-
	atom_codes(S,Codes),
	number_codes(N,Codes),
	bind_socket(N,5,Socket),
	serve_socket(Socket,socket_serve,catcher).

socket_serve(Stream):-
	http_serve_fetch(Stream,http_serve(Stream)).

http_serve(Request,Stream,Response):-
	writeq(received_message(Stream,Request)),nl,
	assertz_fact(received_message(Stream,Request)),
	gen_response1(Request,Response).

gen_response1(Request,Response):-
	member(post,Request),
	gen_response("/",Response).
gen_response1(Request,Response):-
	member(document(Doc),Request),
	gen_response(Doc,Response).

gen_response("/",Response):- !,
	current_host(Host),
	date_time(Time,Year,Month,Day,WeekDay),
	Response = [
		       message_date(date(WeekDay,Day,Month,Year,Time)),
		       status(success,200,"OK"),
		       http_server("Ciao-Prolog http Server 0.1"),
%		       last_modified(date()),
%		       etag("""2d41cf-4a7-69344c00"""),
%		       accept-ranges("bytes")
		       location(Host),
		       content_length(15),
		       content_type(text,html,[charset='ISO-8859-1']),
		       content("<HTML>ok</HTML>")
		   ].
gen_response(File,Response):-
	dir(Dir),
	append(Dir,File,Paths),
	atom_codes(Path,Paths),
	file_exists(Path),
	file_to_string(Path,Content),
	length(Content,ContentLength),
	!,
	current_host(Host),
	date_time(Time,Year,Month,Day,WeekDay),
	Response = [
		       status(success,200,"OK"),
		       message_date(date(WeekDay,Day,Month,Year,Time)),
		       http_server("Ciao-Prolog http Server 0.1"),
%		       last_modified(date()),
%		       etag("""2d41cf-4a7-69344c00"""),
%		       accept-ranges("bytes")
		       location(Host),
		       content_length(ContentLength),
		       content_type(text,html,[charset='ISO-8859-1']),
		       content(Content)
		   ].
gen_response(_,Response):-
	current_host(Host),
	file_to_string('not_found.html',Content),
	length(Content,ContentLength),
	date_time(Time,Year,Month,Day,WeekDay),
	Response = [
		       status(request_error,404,"Not Found"),
		       message_date(date(WeekDay,Day,Month,Year,Time)),
		       http_server("Ciao-Prolog http Server 0.1"),
%		       last_modified(date()),
%		       etag("""2d41cf-4a7-69344c00"""),
%		       accept-ranges("bytes")
		       location(Host),
		       content_length(ContentLength),
		       content_type(text,html,[charset='ISO-8859-1']),
		       content(Content)
		   ].

catcher(Error):- 
	assertz_fact(cached(Error)),
	writeq(user,Error), nl(user).

date_time(Time,Year,Month,Day,WeekDay):-
	datime(_,Year,MonthNum,Day,_,_,_,WeekDayNum,_),
	datime(datime(Year,MonthNum,Day,HH,MM,SS)),
	http_weekday(WeekDayNum,WeekDay),
	http_month(MonthNum,Month),
	number_codes(HH,H),
	number_codes(MM,M),
	number_codes(SS,S),
	list_concat([H,":",M,":",S],TimeStr),
	atom_codes(Time,TimeStr).

http_weekday(1,'Monday').
http_weekday(2,'Tuesday').
http_weekday(3,'Wednesday').
http_weekday(4,'Thursday').
http_weekday(5,'Friday').
http_weekday(6,'Saturday').
http_weekday(7,'Sunday').

http_month( 1,'January').
http_month( 2,'February').
http_month( 3,'March').
http_month( 4,'April').
http_month( 5,'May').
http_month( 6,'June').
http_month( 7,'July').
http_month( 8,'August').
http_month( 9,'September').
http_month(10,'October').
http_month(11,'November').
http_month(12,'December').
