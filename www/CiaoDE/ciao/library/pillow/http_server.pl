
:- module(http_server,[http_request/4,http_serve_fetch/2],
	[assertions,dcg,hiord]).

%:- use_module(library(dcg_expansion),[phrase/3]).
:- use_module(library(lists),[append/3,select/3]).
:- use_module(library('pillow/pillow_aux'),
	[ http_crlf/2, http_line/3, http_quoted_string/3,
	  http_sp/2, parse_integer/3
	]).
:- use_module(library(strings),[string/3,write_string/2]).
:- use_module(library('pillow/http_server_ll')).

:- comment(bug,"Have to check syntax of HTTP attributes for upper and 
	lowercase.").
:- comment(bug,"Have to check whether numbers are numbers or atoms.").
:- comment(bug,"POST is still under development.").
:- comment(bug,"http_request should not be here!").

http_request(http(Host,Port,Req),Host,Port,Req):-
	atom(Host),
	number(Port),
	Port>0.

:- meta_predicate http_serve_fetch(?,pred(2)).
% Stream: socket stream
% Server: predicate name (without arguments) for processing 
%         requests.
http_serve_fetch(Stream,Server):-
	http_receive_header(Stream,RequestChars,Tail),
	http_parse_request(Doc,Opt,RequestChars,_),
	(member(post,Opt) ->
	 member('Content-length'(Length),Opt),
	 atom_codes(Length,Ls),
	 number_codes(Ln,Ls),
	 http_receive_content(Stream,Ln,Tail,Content),
	 Opt1 = [content(Content)|Opt]
	;
	 Opt1 = Opt
	),
	Server([document(Doc)|Opt1],Response),
	http_response_string(Response,ResponseChars,[]),
	write_string(Stream,ResponseChars),
	flush_output(Stream),
	close(Stream).
	
:- pred http_parse_request(-Document,-Request,+RequestChars,+RequestCharsTail)
   # "Parse a string into an HTTP request, conforming to
      the RFC 1945 guidelines.  Does not use the headers: current date,
      pragma, referer, and entity body (this will have to change if the
      implementation extends beyond the GET and HEAD methods.  cf
      RFC1945 section 7.2)".

http_parse_request(Document,Options) -->
        http_request_method(Options,Options1),
        " ",
        string(Document),
%        " HTTP/1.0",
        " HTTP/", parse_integer(_Major), ".", parse_integer(_Minor),
        http_crlf,
        http_req(Options1), !.

http_request_method(Options,Options1) -->
        "HEAD", !,
        { Options = [head|Options1] }.
http_request_method(Options,Options1) -->
        "POST", !,
        { Options = [post|Options1] }.
http_request_method(Options, Options) -->
        "GET".

http_req([]) -->  http_crlf.
http_req([Option|Options]) -->
        http_request_option(Option), !,
        http_req(Options).

http_request_option(Option) -->
        "User-Agent: ",  !,
        string(AStr),
        http_crlf,
        { Option=user_agent(A),
          atom_codes(A,AStr)
        }.
http_request_option(Option) -->
        "If-Modified-Since: ", !,
        http_internet_date(WkDay,Day,Month,Year,Time),
        http_crlf,
	{ Option=if_modified_since(date(WkDay,Day,Month,Year,Time)) }.
http_request_option(Option) -->
        "Authorization: ", !,
        http_credentials(Scheme, Params),
        http_crlf,
	{ Option=authorization(Scheme, Params) }.
http_request_option(Option) -->
        string(FS),
        ": ", !,
        string(AS),
        http_crlf,
        { atom_codes(F,FS),
          functor(Option,F,1),
          arg(1,Option,A),
          atom_codes(A,AS)
        }.
%% Simply fail!
%% http_request_option(O) --> "",
%%         {warning(['Invalid http_request_param ',O])}.

http_credentials(Scheme, Cookie) -->
        "Basic ", !,
        string(Cookie),
	{ Scheme=basic }.
http_credentials(Scheme,Params) -->
        string(S), " ",
        http_credential_params(Params),
        { atom_codes(Scheme, S) }.

http_credential_params([]) --> "".
http_credential_params([P|Ps]) -->
        http_credential_param(P),
        http_credential_params_rest(Ps).

http_credential_params_rest([]) --> "".
http_credential_params_rest([P|Ps]) -->
        ", ",
        http_credential_param(P),
        http_credential_params_rest(Ps).

http_credential_param(Param) -->
        string(PS), "=""", string(V), """",
	!,
        { atom_codes(P, PS),
	  Param=(P=V)
	}.

http_internet_date(WeekDay,Day,Month,Year,Time) -->
        http_weekday(WeekDay),
        ",",
        http_sp,
        http_day(Day),
        http_sp_or_minus,
        http_month(Month),
        http_sp_or_minus,
        http_year(Year),
        http_sp,
        http_time(Time),
        http_sp,
        "GMT".

http_sp_or_minus --> "-", !.
http_sp_or_minus --> http_sp.

% ============================================================================
% PROLOG BNF GRAMMAR FOR HTTP RESPONSES
%  Based on RFC 1945
%
% ============================================================================

http_response_string(R) -->
	http_full_response(R), !.
http_response_string(R) -->
	http_simple_response(R).

http_full_response(Response) -->
	% only the first one:
	{ select(status(Ty,SC,RP),Response,Head_Body) }, !,
        http_status_line(Ty,SC,RP),
	{ ( select(content(Body),Head_Body,Head)
	  ; Head=Head_Body, Body=[] )
	},
        http_response_headers(Head),
        http_crlf,
        http_entity_body(Body).

http_simple_response(Body) -->
        http_entity_body(Body).

http_response_headers([H|Hs]) -->
        http_response_header(H),
        http_response_headers(Hs).
http_response_headers([]) --> "".

http_entity_body(B,B,[]).

% ----------------------------------------------------------------------------

http_status_line(Ty,SC,RP) -->
        "HTTP/1.0 ",
        http_status_code(Ty,SC),
        " ",
        http_line(RP), !.

http_status_code(Ty,SC) -->
        [X,Y,Z],
        {
            type_of_status_code(X,Ty), !,
            number_codes(SC,[X,Y,Z])
        }.

type_of_status_code(0'1, informational) :- !.
type_of_status_code(0'2, success) :- !.
type_of_status_code(0'3, redirection) :- !.
type_of_status_code(0'4, request_error) :- !.
type_of_status_code(0'5, server_error) :- !.
%??? type_of_status_code(_, extension_code).

% ----------------------------------------------------------------------------

% General header
http_response_header(P) --> http_pragma(P), !.
http_response_header(D) --> http_message_date(D), !.
% Response header
http_response_header(L) --> http_location(L), !.
http_response_header(S) --> http_server(S), !.
http_response_header(A) --> http_authenticate(A), !.
% Entity header
http_response_header(A) --> http_allow(A), !.
http_response_header(E) --> http_content_encoding(E), !.
http_response_header(L) --> http_content_length(L), !.
http_response_header(T) --> http_content_type(T), !.
http_response_header(X) --> http_expires(X), !.
http_response_header(M) --> http_last_modified(M), !.
http_response_header(E) --> http_extension_header(E), !.

% ----------------------------------------------------------------------------

http_pragma(pragma(P)) -->
        http_field("pragma"),
        http_line(P).

http_message_date(message_date(D)) -->
        http_field("date"),
        http_date(D),
        http_crlf.

http_location(location(URL)) -->
        http_field("location"),
        { atom_codes(URL,URLStr) },
        http_line(URLStr).

http_server(http_server(S)) -->
        http_field("server"),
        http_line(S).

http_authenticate(authenticate(C)) -->
        http_field("www-authenticate"),
        http_challenges(C).

http_allow(allow(Methods)) -->
        http_field("allow"),
        http_token_list(Methods),
        http_crlf.

http_content_encoding(content_encoding(E)) -->
        http_field("content-encoding"),
        http_lo_up_token(E),
        http_crlf.

http_content_length(content_length(L)) -->
        http_field("content-length"),
	number_string(L),
        http_crlf.

http_content_type(content_type(Type,SubType,Params)) -->
        http_field("content-type"),
        http_media_type(Type,SubType,Params),
        http_crlf.

http_expires(expires(D)) -->
        http_field("expires"),
        http_date(D),
        http_crlf.

http_last_modified(last_modified(D)) -->
        http_field("last-modified"),
        http_date(D),
        http_crlf.

http_extension_header(T) -->
        { functor(T,Fu,1),
	  atom_codes(Fu,F),
	  arg(1,T,A)
        },
        http_field(F),
        http_line(A).

% ----------------------------------------------------------------------------

http_media_type(Type,SubType,Params) -->
        http_lo_up_token(Type),
        "/",
        http_lo_up_token(SubType),
        http_type_params(Params).

http_type_params([P|Ps]) -->
        ";",
        http_type_param(P),
        http_type_params(Ps).
http_type_params([]) --> "".

http_type_param(A = V) -->
        http_lo_up_token(A),
        "=",
        http_token(V).

% ----------------------------------------------------------------------------

http_date(date(WeekDay,Day,Month,Year,Time)) -->
        http_weekday(WeekDay),
        " ",
        http_month(Month),
        " ",
        http_day(Day),
        " ",
        http_time(Time),
        " ",
        http_year(Year).

http_weekday('Monday') --> "Mon", !.
http_weekday('Tuesday') --> "Tue", !.
http_weekday('Wednesday') --> "Wed", !.
http_weekday('Thursday') --> "Thu", !.
http_weekday('Friday') --> "Fri", !.
http_weekday('Saturday') --> "Sat", !.
http_weekday('Sunday') --> "Sun", !.

http_day(Day) -->
        { number_codes(Day,[D1,D2])
        }, !,
        [D1,D2].
http_day(Day) -->
        { number_codes(Day,[D]) },
        " ",
        [D].

http_month('January') --> "Jan".
http_month('February') --> "Feb".
http_month('March') --> "Mar".
http_month('April') --> "Apr".
http_month('May') --> "May".
http_month('June') --> "Jun".
http_month('July') --> "Jul".
http_month('August') --> "Aug".
http_month('September') --> "Sep".
http_month('October') --> "Oct".
http_month('November') --> "Nov".
http_month('December') --> "Dec".

% Assumes Year > 999
http_year(Year) -->
        { number_codes(Year,[Y1,Y2,Y3,Y4]) },
        [Y1,Y2,Y3,Y4].

http_time(Time) -->
        { atom_codes(Time,Hour),
	  time_field(Hour,[H1,H2,0':|Min]),
          time_field(Min,[M1,M2,0':|Sec]),
          time_field(Sec,[S1,S2])
	},
        [H1,H2,0':,M1,M2,0':,S1,S2].

time_field(Pattern,Pattern):- !.
time_field(Pattern,[0|Pattern]).

% ----------------------------------------------------------------------------

http_challenges([C|CS]) -->
        http_challenge(C),
        http_more_challenges(CS).

http_more_challenges([C|CS]) -->
        ", ",
        http_challenge(C),
        http_more_challenges(CS).
http_more_challenges([]) --> http_crlf.

http_challenge(challenge(Scheme,Realm,Params)) -->
        http_lo_up_token(Scheme),
        " ",
        http_lo_up_token(realm), "=", http_quoted_string(Realm),
        http_auth_params(Params).

http_auth_params([P|Ps]) -->
        ",",
        http_auth_param(P),
        http_auth_params(Ps).
http_auth_params([]) --> "".

http_auth_param(P=V) -->
        http_lo_up_token(P),
        "=",
        http_quoted_string(V).

% ----------------------------------------------------------------------------

http_token_list([T|Ts]) -->
        http_token(T),
        http_token_list0(Ts).

http_token_list0([T|Ts]) -->
        ", ",
        http_token(T),
        http_token_list0(Ts).
http_token_list0([]) --> "".

http_token(T) --> token_string(T).

http_lo_up_token(T) --> token_string(T).

token_string(T,String,More):-
	atom_codes(T,Codes),
	append(Codes,More,String).

number_string(L,String,More):-
	number_codes(L,Codes),
	append(Codes,More,String).

% ----------------------------------------------------------------------------

http_field(T,Field,More):- append(T,": "||More,Field).
