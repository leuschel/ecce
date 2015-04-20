:- module(messages,[
	    error_message/1,
	    error_message/2,
	    error_message/3,
	    warning_message/1,
	    warning_message/2,
	    warning_message/3,
 	    note_message/1,
	    note_message/2,
	    note_message/3,
 	    simple_message/1,
	    simple_message/2,
	    optional_message/2,
	    optional_message/3,
 	    debug_message/1,
	    debug_message/2,
 	    debug_goal/2,
 	    debug_goal/3
	],
        [
            assertions,regtypes,isomodes
        ]).

%% NOTE: if you change the output format of messages you 
%%       will probably also want to change ciao.el

:- use_module(library(format),[sformat/3,format/3,format_control/1]).

% Other libraries
:- use_module(library(lists)).
:- use_module(library(filenames),[no_path_file_name/2]).
:- use_module( library( strings ) , [write_string/1] ).

:- set_prolog_flag(multi_arity_warnings, off).

%% ---------------------------------------------------------------------------

:- comment(title,"Printing status and error messages").

:- comment(author,"The CLIP Group").

:- comment(module,"This is a very simple library for printing status
     and error messages to the console.").

:- comment(bug, "Debug message switching should really be done with an
   expansion, for performance.").

:- comment(doinclude,location/1).
:- regtype location/1 # "Identifies a program source line.".

location(loc(File,L1,L2)):- atm(File), int(L1), int(L2).

%% ---------------------------------------------------------------------------

:- pred error_message(Text) : string 
   # "The text provided in @var{Text} is printed as an ERROR message.".

:- impl_defined(error_message/1).
:- meta_predicate error_message(addmodule).

error_message(Message,Module) :-
	compose("ERROR",Module,Message).

:- pred error_message(Text,ArgList) : format_control * list 
   # "The text provided in @var{Text} is printed as an ERROR message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}.".

:- impl_defined(error_message/2).
:- meta_predicate error_message(?,addmodule).

error_message(Message,A,Module) :-
	compose("ERROR",Module,Message,A).

:- pred error_message(Lc,Text,ArgList) : location * format_control * list 
   # "The text provided in @var{Text} is printed as an ERROR message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}, and
     reporting error location @var{Lc} (file and line numbers).".

:- impl_defined(error_message/3).
:- meta_predicate error_message(?,?,addmodule).

error_message(Loc,Message,A,Module) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	compose("ERROR",Module,File,LB,LE,Message,A).
error_message(_Loc,Message,A,Module) :-
	compose("ERROR",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred warning_message(Text) : string 
   # "The text provided in @var{Text} is printed as a WARNING message.".

:- impl_defined(warning_message/1).
:- meta_predicate warning_message(addmodule).

warning_message(Message,Module) :-
	compose("WARNING",Module,Message).

:- pred warning_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a WARNING message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}.".

:- impl_defined(warning_message/2).
:- meta_predicate warning_message(?,addmodule).

warning_message(Message,A,Module) :-
	compose("WARNING",Module,Message,A).

:- pred warning_message(Lc,Text,ArgList) : location * format_control * list 

   # "The text provided in @var{Text} is printed as a WARNING message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}, and
     reporting error location @var{Lc} (file and line numbers).".

:- impl_defined(warning_message/3).
:- meta_predicate warning_message(?,?,addmodule).

warning_message(Loc,Message,A,Module) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	compose("WARNING",Module,File,LB,LE,Message,A).
warning_message(_Loc,Message,A,Module) :-
	compose("WARNING",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred note_message(Text) : string 
   # "The text provided in @var{Text} is printed as a NOTE.".

:- impl_defined(note_message/1).
:- meta_predicate note_message(addmodule).

note_message(Message,Module) :-
	compose("NOTE",Module,Message).

:- pred note_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a NOTE, using the
     arguments in @var{ArgList} to interpret any variable-related
     formatting commands embedded in @var{Text}.".

:- impl_defined(note_message/2).
:- meta_predicate note_message(?,addmodule).

note_message(Message,A,Module) :-
	compose("NOTE",Module,Message,A).

:- pred note_message(Lc,Text,ArgList) : location * format_control * list 

   # "The text provided in @var{Text} is printed as a NOTE, using the
     arguments in @var{ArgList} to interpret any variable-related
     formatting commands embedded in @var{Text}, and reporting error
     location @var{Lc} (file and line numbers).".

:- impl_defined(note_message/3).
:- meta_predicate note_message(?,?,addmodule).

note_message(Loc,Message,A,Module) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	compose("NOTE",Module,File,LB,LE,Message,A).
note_message(_Loc,Message,A,Module) :-
	compose("NOTE",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred simple_message(Text) : string 
   # "The text provided in @var{Text} is printed.".

simple_message(Message) :-
	simple_message(Message,[]).

:- pred simple_message(Text,ArgList) : format_control * list 
   # "The text provided in @var{Text} is printed as a message,
     using the arguments in @var{ArgList}.".

simple_message(Message,A) :-
	append([0'{ | Message],"}\n",NMessage),
	format(user_error,NMessage,A).

%% ---------------------------------------------------------------------------

:- pred optional_message(Text,Opts) : string * list(atm)
   # "The text provided in @var{Text} is printed as a message, but
     only if the atom @tt{-v} is a member of @var{Opts}. These
     predicates are meant to be used for optional messages, which are
     only to be printed when @em{verbose} output is requested
     explicitly.".

optional_message(Message,Opts) :-
	optional_message(Message,[],Opts).

:- pred optional_message(Text,ArgList,Opts) : format_control * list * list(atm)
   # "The text provided in @var{Text} is printed as a message, using
     the arguments in @var{ArgList}, but only if the atom @tt{-v} is a
     member of @var{Opts}. These predicates are meant to be used for
     optional messages, which are only to be printed when @em{verbose}
     output is requested explicitly.".

optional_message(Message,A,Opts) :-
	member('-v',Opts),
	!,
	simple_message(Message,A).
optional_message(_Message,_A,_Opts).

%% ---------------------------------------------------------------------------

:- pred debug_message(Text) : format_control 

   # "The text provided in @var{Text} is printed as a debugging
      message.  These messages are turned @tt{on} by defining a fact
      of @pred{issue_debug_messages/1} with the module name as
      argument.".

:- impl_defined(debug_message/1).
:- meta_predicate debug_message(addmodule).

debug_message(Message,Module) :-
	(  issue_debug_messages(Module)
	-> compose("DEBUG",Module,Message)
	;  true ).

:- pred debug_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a debugging
      message, using the arguments in @var{ArgList} to interpret any
      variable-related formatting commands embedded in
      @var{Text}. These messages are turned @tt{on} by defining a fact
      of @pred{issue_debug_messages/1} which the module name as
      argument.".

:- impl_defined(debug_message/2).
:- meta_predicate debug_message(?,addmodule).

debug_message(Message,A,Module) :-
	(  issue_debug_messages(Module)
	-> compose("DEBUG",Module,Message,A)
	;  true ).

:- pred issue_debug_messages(Module) => atom

   # "Printing of debugging messages is enabled for module @var{Module}.".

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.

%% ---------------------------------------------------------------------------

:- pred debug_goal(Goal,Text) 

   # "@var{Goal} is called.  The text provided in @var{Text} is then
      printed as a debugging message.  The whole process (including
      running @var{Goal}) is turned @tt{on} by defining a fact of
      @pred{issue_debug_messages/1} with the module name as
      argument.".

:- impl_defined(debug_goal/2).
:- meta_predicate debug_goal(goal,addmodule).

debug_goal(Goal,Message,Module) :-
	(  issue_debug_messages(Module)
	-> call(Goal),
	   compose("DEBUG",Module,Message)
	;  true ).

:- pred debug_goal(Goal,Text,ArgList) 

   # "@var{Goal} is called.  The text provided in @var{Text} is then
      printed as a debugging message, using the arguments in
      @var{ArgList} to interpret any variable-related formatting
      commands embedded in @var{Text}. Note that the variables in
      @var{ArgList} can be computed by @var{Goal}.  The whole process
      (including running @var{Goal}) is turned @tt{on} by defining a
      fact of @pred{issue_debug_messages/1} with the module name as
      argument.".

:- impl_defined(debug_goal/3).
:- meta_predicate debug_goal(goal,?,addmodule).

debug_goal(Goal,Message,A,Module) :-
	(  issue_debug_messages(Module)
	-> call(Goal),
	   compose("DEBUG",Module,Message,A)
	;  true ).

%% ---------------------------------------------------------------------------

:- pred compose(Type,Module,Mess) 
   : string * atm * string

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, with error message @var{Mess}.".

compose(Type,Module,Mess) :-
	append("{~s (~q): ",Mess,T1),
	append(T1,"}~n",CMess),
%       NEW METHOD
	sformat( S ,CMess,[Type,Module]),
	display_long_string( S , user_error ).
%       OLD METHOD
% 	prolog_flag(write_strings, Old, on),
% 	format(user_error,CMess,[Type,Module]),
% 	set_prolog_flag(write_strings, Old).


:- pred compose(Type,Module,Mess,Args) 
   : string * atm * format_control * list

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, with error message @var{Mess} containing arguments
      @var{Args}.".

compose(Type,Module,Mess,Args) :-
	append("{~s (~q): ",Mess,T1),
	append(T1,"}~n",CMess),
	simplify_module(Module,SimplifiedModule),
%       NEW METHOD
	sformat( S , CMess,[Type,SimplifiedModule|Args]),
	display_long_string( S , user_error ).
%       OLD METHOD
% 	prolog_flag(write_strings, Old, on),
% 	format(user_error,CMess,[Type,SimplifiedModule|Args]),
% 	set_prolog_flag(write_strings, Old).


simplify_module(user(Path),SimplifiedModule) :-
	no_path_file_name(Path,SimplifiedModule),
	!.
simplify_module(Path,SimplifiedModule) :-
	no_path_file_name(Path,SimplifiedModule),
	!.
simplify_module(Module,Module).


:- pred compose(Type,Module,File,LB,LE,Mess,Args) 
   : string * atm * atm * int * int * format_control * list

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, while processing file @var{File}, between line
      numbers @var{LB} and @var{LE}, with error message @var{Mess}
      containing arguments @var{Args}.".

compose(Type,Module,File,LB,LE,Mess,Args) :-
	append("{In ~w~n~s (~q): (lns ~w-~w) ",Mess,T1),
	append(T1,"~n}~n",CMess),
%       NEW METHOD
	sformat( S , CMess,[File,Type,Module,LB,LE|Args]),
	display_long_string( S , user_error ).
%       OLD METHOD
%	prolog_flag(write_strings, Old, on),
%	format(user_error,CMess,[File,Type,Module,LB,LE|Args]),
%	set_prolog_flag(write_strings, Old).


%% ---------------------------------------------------------------------------
:- pred space( N ) : num( N )
# "prints @var{N} spaces.".


space( 0 ) :- !.

space( N ) :-
	N > 0,
	!,
	N1 is N - 1,
	display( ' ' ),
	space( N1 ).

space( _ ).




display_long_string( X , S ) :-
	line_position( user_output , NInit ),
	display_long_string_n( X , S , NInit , [79] ).

display_long_string_n( X , S , I , Max ) :-
	append( WORD_WO_S  , [SEP|XE] , X ),
 	member( SEP , " ,[]()" ),
	!,
	append( WORD_WO_S , [SEP] , WORD ),
	nl_if_necessary( WORD , XE , S , I , Max , Current ),
	display__compute_next_space( WORD , Current , I , FI  ),
	write_string( WORD ),
	display_long_string_n( XE , S , FI , Max ).

display_long_string_n( WORD , S , I , Max ) :-
	nl_if_necessary( WORD , "" , S , I , Max , Current ) , 
	display__compute_next_space( WORD , Current , I ,  _ ),
	write_string( WORD ).




% nil
display__compute_next_space( [] , _ , I , I ) :- 
	!.

% push
display__compute_next_space( [SEP|R] , Current , I , FI ) :-
	member( SEP , "[({" ) ,
	!,
	NCurrent is Current + 1,
        NSpace   is Current + 1,
	display__compute_next_space( R , NCurrent , [ NSpace | I ] , FI ).
	
% pop
display__compute_next_space( [SEP|R] , Current , [ _ | NI ] , FI ) :-
	member( SEP , "])}" ) ,
	!,
	NCurrent is Current + 1,
	display__compute_next_space( R , NCurrent , NI , FI ).

% normal char	
display__compute_next_space( [_|R] , Current , I , FI ) :-
	NCurrent is Current + 1,
	display__compute_next_space( R , NCurrent , I , FI ),
	!.

% if it fails (?)
display__compute_next_space( _ , _ , I , I ).


:- use_module( library( write ) ).

nl_if_necessary( WORD , REST , S , I , Max , C ) :-
	line_position( S , Current ),
	word_length( WORD , REST , Current , Max , WL ) , 
	(
	    WL + Current + 1 > Max
	->
	    nl, 
	    I = [ C | _ ] ,
	    space( C )
	;
	    C = Current
	).

nl_if_necessary( _ , _ , S , _ , C ) :- 
	line_position( S , C ).




% The lenght of a "word" is:
% * if it a list -> sumatory of the length of its members
% * lenght( word )
%
% Optimization: We can stop counting if
%  Current + CurrentComputedLen > Max
word_length( [ 0'[ | WORD ] , REST , Current , Max , WL2 ) :-
	!,
	word__count( WORD , REST , 1 , 0 , Current , Max , WL ),
	WL2 is WL - Current + 1.

word_length( [ 0'( | WORD ] , REST , Current , Max , WL2 ) :-
	!,
	word__count( WORD , REST , 0 , 1 , Current , Max , WL ),
	WL2 is WL - Current + 1.

word_length( WORD , REST , Current , Max , WL2 ) :-
	word__count( WORD , REST , 0 , 0 , Current , Max , WL ),
	WL2 is WL - Current.
%word_length( WORD , _ , _ , _ , WL ) :-
%	length( WORD , WL ).




:- pred word__count( _ , _ , _ , _ , Current , Max , Current ) :
	ground * ground * num * num * num * num * var.

% Start counting till balanced separator is reached
% S = number of [ ] (square parentheris) not yet balanced
% P = number of ( ) (parentheris) not yet balanced
word__count( [] , _ , 0 , 0 , WL , _ , WL ) :-
	!.

word__count( _ , [] , 0 , 0 , WL , _ , WL ) :-
	!.


word__count( _ , _ , _ , _ , Current , Max , WL ) :-
	Current > Max,
	WL = Current,
	!.

word__count( [ 0'[ | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	S1 is S + 1,
	C1 is Current + 1,
	word__count( WORD , REST , S1 , P , C1 , Max , WL ).

word__count( [ 0'( | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	P1 is P + 1,
	C1 is Current + 1,
	word__count( WORD , REST , S , P1 , C1 , Max , WL ).

word__count( [ 0'] | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	S1 is S - 1,
	C1 is Current + 1,
	( 
	    S1 >= 0
	->
	    word__count( WORD , REST , S1 , P , C1 , Max , WL )
	;
	    C1 = WL
	).

word__count( [ 0') | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	P1 is P - 1,
	C1 is Current + 1,
	(
	    P1 >= 0
	->
	    word__count( WORD , REST , S , P1 , C1 , Max , WL )
	;
	    C1 = WL
	).

word__count( [ _ | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	C1 is Current + 1,
	word__count( WORD , REST , S , P , C1 , Max , WL ).

word__count( [] , [] , _ , _ , WL , _ , WL ) :-
	!.
	
word__count( [] , REST , S , P , Current , Max , WL ) :-
	!,
	word__count( REST , [] , S , P , Current , Max , WL ).

word__count( [] , _ , 0 , 0 , WL , _ , WL ) :-
	!.
