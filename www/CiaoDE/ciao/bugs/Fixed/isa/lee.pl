:- use_module(library(format)).
:- use_module(library(strings)).
:- use_module(library(lists)).
%%:- use_module(library(term_compare)).


def_ask_file( FileName) :-
	format("~NPlease enter the trace file name. ~n", []),
	get_byte(Ch),
	get_filename(Ch,Chars,_Terminator),
	name(FileName, Chars).

get_filename(Ch, [], Terminator) :-
	is_endfilename(Ch),
	!,
	Terminator = Ch.
get_filename(Ch, [Ch|Line], Terminator) :-
	get_byte(Next),
	get_filename(Next, Line, Terminator).

is_endfilename(C) :-		% line terminator, NOT just newline
	C < 32,			% covers EOF, ^G, ^J, ^K, ^L, ^M, ^Z, ESC, ^_
	C =\= 9 /* ^I */.	% not ^I (TAB) though.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rt:- 
        read_trace(_Trace,_Start,_Stop,_Filename,_Agents_List,_And_or).

read_trace(Trace,Start,Stop,Filename,Agents_List,And_or):-
	format("~NPlease enter the trace file name. ~n", []),
        get_line(File),
        name(Filename, File),
        open(Filename,read,Stream),
        first_non_blank(Stream,And_or),
        get_line(Stream,Line),
%%        get_byte(Line,Ch1),
        read_events(Line,Stream,Trace,Stop,Agents),
        close(Stream),
        no_doubles(Agents,Agents_List).

%%        get_line(Stream, Line),
%%      whitespace(Line, [And_or|_]).
%%        whitespace0(Line, [And_or|_]).

first_non_blank(Stream,And_or):-
        get_line(Stream,Line),
        whitespace0(Line,And_or).

stop_event([T,6|_],T):-!.
stop_event(_,_).

read_events([],Stream,[], _Stop,[]):- !.
read_events(Line,Stream,[Node|Graph],Stop,[A|As]):-
       cons_Node(Line,Node),
       stop_event(Node,Stop),
       agent(Node,A),
       get_line(Stream,NextLine),
       read_events(NextLine,Stream,Graph,Stop,As).

cons_Node(Line1,[A,B,C,D,E,F]):-
%%        whitespace0(Line1,Line2),
        construct_number(Line1,10,[],A,Line2),
        construct_number(Line2,10,[],B,Line3),
        construct_number(Line3,16,[],C,Line4),
        construct_number(Line4,10,[],D,Line5),
        construct_number(Line5,16,[],E,Line6),
        construct_number(Line6,10,[],F,_).


construct_number(Line1, Base, ListChar, Number,Line2):-
	is_terminator(Line1,Line2),
        !,
	conv_number(ListChar, Base, 0, Number).

construct_number([N1|Ns],Base,ListChar1,Number,L2):-
        append(ListChar1,[N1],ListChar2),
        construct_number(Ns,Base,ListChar2,Number,L2).

is_terminator([],[]).
is_terminator(S,R):-
        whitespace(S,R).

%%get_number(_Stream,Ch,[],Ter):- 
%%	is_terminator(Ch),
%%	!,
%%	Ter = Ch.
%%get_number(Line,Ch,[Ch|Chars],Term):-
%%	get0(Stream,Next),
%%        get_byte(Line,Next),
%%        get1_code(Stream,Next),
%%	get_number(Line,Next,Chars,Term).


conv_number([],_Base, Hex,Hex).

conv_number([X|Xs],Base,  Hx,Hex):-
    is_capital_letter(X),!,
    Hx1 is (Hx*Base + (X-55)),
    conv_number(Xs,Base, Hx1,Hex).

conv_number([X|Xs],Base, Hx,Hex):-
    no_capital_letter(X),!,
    Hx1 is (Hx*Base + (X-87)),
    conv_number(Xs,Base, Hx1,Hex).

conv_number([X|Xs],Base, Hx,Hex):-
     Hx1 is (Hx*Base + (X-48)),
     conv_number(Xs,Base, Hx1,Hex).

is_capital_letter(X):-
  X =< 70,
  X >= 65.
no_capital_letter(X):-
  X =< 102,
  X >= 97.

%%member(X,[X|Xs]).
%%member(X,[Y|Ys]):-
%%        member(X,Ys).
nonmember(X,[Y|Ys]):-
        X \== Y,
        nonmember(X,Ys).
nonmember(X,[]).
        

no_doubles([X|Xs],Ys):-
        member(X,Xs),
        no_doubles(Xs,Ys).
no_doubles([X|Xs],[X|Ys]):-
        nonmember(X,Xs),
        no_doubles(Xs,Ys).
no_doubles([],[]).



%%stop_event([T,6|_],T):-!.
%%stop_event(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start_event(+Event,?Time)                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_event([[T,5|_]|_],T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agent(+Event,?Agent)                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
agent([_,_,_,_,_,A],A).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% CALCULAR TIEMPOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%tiempos_vacios(L1,L2)
%%crea una lista L2 a partir de L1 inicializando los tiempos a 0
%%
tiempos_vacios([],[]).
tiempos_vacios([X|Y],[[X,0]|Z]):-
        tiempos_vacios(Y,Z).
        
%%sumar_tiempos(L,[[E1|T1],N)
%% N es la suma de todos los tiempos
%%
sumar_tiempos([X|[]],[E1,T1],N).
sumar_tiempos([[A1,B1,C1,D1,E1,F1],[A2,B2,C2,D2,E2,F2]|R1],[E1,T1],T):-
        T is T1+A2-A1,
        sumar_tiempos([[A2,B2,C2,D2,E2,F2]|R1],[E1,T1],N).

%%calcular_tiempos(T,L1,L2)
%%Calcula en L2 la suma de todos los tiempos de ejecución en cada agente
calcular_tiempos(T,[],[]).
calcular_tiempos(T,[[E1,T1]|R1],[[E1,N]|R2]):-
        sumar_tiempos(T,[E1,T1],N),
        calcular_tiempos(T,R1,R2).

grafica(Trace,Agents_List,Listat2):-
%%        read_trace(Trace,Start,Stop,Filename,Agents_List,And_or),
        tiempos_vacios(Agents_List,Listat1),
        calcular_tiempos(Trace,Listat1,Listat2).
