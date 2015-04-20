:- module(bench2,[eval/0],_).

:- use_module(library(prolog_sys),[statistics/2]).
:- use_module(library(system),[shell/1]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(format),[format/2]).

%fib
:- use_module(fib,[fib/2]).
:- use_module(fib_and_user,[fib/2]).
:- use_module(fib_and,[fib/2]).
:- use_module(fib_and1p1,[fib/2]).

%:- use_module(nodet,[p/1]).
%:- use_module(nodet_and,[p/1]).

%dia_sums
:- use_module(dia_sums,[test/0]).
:- use_module(dia_sums_and,[test/0]).
:- use_module(dia_sums_and_user,[test/0]).
:- use_module(dia_sums_and1p1,[test/0]).

%crypt
:- use_module(crypt,[go/0]).
:- use_module(crypt_and,[go/0]).
:- use_module(crypt_and_user,[go/0]).
:- use_module(crypt_and1p1,[go/0]).

%map
:- use_module(map,[test_all/1]).
:- use_module(map_and,[test_all/1]).
:- use_module(map_and_user,[test_all/1]).
:- use_module(map_and1p1,[test_all/1]).

%money
:- use_module(money,[test/0]).
:- use_module(money_and,[test/0]).
:- use_module(money_and_user,[test/0]).
:- use_module(money_and1p1,[test/0]).

%mqu
%:- use_module(mqu,[test/0]).
:- use_module(mqu_and,[queens/1]).
:- use_module(mqu_and_user,[queens/1]).
:- use_module(mqu_and1p1,[queens/1]).

%mutest
:- use_module(mutest,[test/0]).
:- use_module(mutest_and,[test/0]).
:- use_module(mutest_and_user,[test/0]).
:- use_module(mutest_and1p1,[test/0]).

%qu_evan
:- use_module(qu_evan,[queen/2]).
:- use_module(qu_evan_and,[queen/2]).
:- use_module(qu_evan_and_user,[queen/2]).
:- use_module(qu_evan_and1p1,[queen/2]).

%qu_vitor
:- use_module(qu_vitor,[run/2]).
:- use_module(qu_vitor_and,[run/2]).
:- use_module(qu_vitor_and_user,[run/2]).
:- use_module(qu_vitor_and1p1,[run/2]).


time(T) :- statistics(runtime,[_,T]).


eval:-
%fib
	eval_fib(Tfib),
	eval_fib_and_user(Tfib_and_user),
	eval_fib_and(Tfib_and),
	eval_fib_and1p1(Tfib_and1p1),
%crypt
	eval_crypt(Tcrypt),
	eval_crypt_and(Tcrypt_and),
	eval_crypt_and_user(Tcrypt_and_user),
	eval_crypt_and1p1(Tcrypt_and1p1),
%dia_sums
	eval_dia_sums(Tdia_sums),
	eval_dia_sums_and(Tdia_sums_and),
	eval_dia_sums_and_user(Tdia_sums_and_user),
	eval_dia_sums_and1p1(Tdia_sums_and1p1),
%map
	eval_map(Tmap),
	eval_map_and(Tmap_and),
	eval_map_and_user(Tmap_and_user),
	eval_map_and1p1(Tmap_and1p1),
%money
	eval_money(Tmoney),
	eval_money_and(Tmoney_and),
	eval_money_and_user(Tmoney_and_user),
	eval_money_and1p1(Tmoney_and1p1),
%mqu
%	eval_mqu(Tmqu),
	eval_mqu_and(Tmqu_and),
	eval_mqu_and_user(Tmqu_and_user),
	eval_mqu_and1p1(Tmqu_and1p1),
%mutest
	eval_mutest(Tmutest),
	eval_mutest_and(Tmutest_and),
	eval_mutest_and_user(Tmutest_and_user),
	eval_mutest_and1p1(Tmutest_and1p1),
%qu_evan
	eval_qu_evan(Tqu_evan),
	eval_qu_evan_and(Tqu_evan_and),
	eval_qu_evan_and_user(Tqu_evan_and_user),
	eval_qu_evan_and1p1(Tqu_evan_and1p1),
%qu_vitor
	eval_qu_vitor(Tqu_vitor),
	eval_qu_vitor_and(Tqu_vitor_and),
	eval_qu_vitor_and_user(Tqu_vitor_and_user),
	eval_qu_vitor_and1p1(Tqu_vitor_and1p1),

	open('bench.tex',write,File),
	set_output(File),
	display('
\\documentclass[12pt]{article}
\\title{Performance of Andorra}
\\author{Claudio Vaucheret}
\\begin{document}

\\maketitle

\\begin{table}[htbp]
  \\begin{center}
    \\begin{tabular}{||l||r||r|r|r|r|r||} \\hline \\hline
        program name & ciao & qe\\_andorra & and-user & andorra & ciao/and & ciao/user \\\\ \\hline \\hline 
                       '),

% crypt
 	Tciao_crypt_and is Tcrypt/Tcrypt_and,
 	Tciao_crypt_and_user is Tcrypt/Tcrypt_and_user,
        format("crypt & ~D & ~D & ~D & ~D & ~3f & ~3f \\\\ \\hline ",
	[Tcrypt,Tcrypt_and1p1,Tcrypt_and_user,Tcrypt_and,Tciao_crypt_and,Tciao_crypt_and_user]),

% dia_sums
 	Tciao_dia_sums_and is Tdia_sums/Tdia_sums_and,
 	Tciao_dia_sums_and_user is Tdia_sums/Tdia_sums_and_user,
        format("dia\\_sums & ~D & ~D & ~D & ~D & ~3f & ~3f \\\\ \\hline ",
	[Tdia_sums,Tdia_sums_and1p1,Tdia_sums_and_user,Tdia_sums_and,Tciao_dia_sums_and,Tciao_dia_sums_and_user]),


% fib
 	Tciao_fib_and is Tfib/Tfib_and,		 
 	Tciao_fib_and_user is Tfib/Tfib_and_user,      
        format("fib & ~D & ~D & ~D & ~D & ~3f & ~3f \\\\ \\hline ",
	[Tfib,Tfib_and1p1,Tfib_and_user,Tfib_and,Tciao_fib_and,Tciao_fib_and_user]),


% map
 	Tciao_map_and is Tmap/Tmap_and,
 	Tciao_map_and_user is Tmap/Tmap_and_user,
        format("map & ~D & ~D & ~D & ~D & ~3f & ~3f \\\\ \\hline ",
	[Tmap,Tmap_and1p1,Tmap_and_user,Tmap_and,Tciao_map_and,Tciao_map_and_user]),

% money
 	Tciao_money_and is Tmoney/Tmoney_and,
 	Tciao_money_and_user is Tmoney/Tmoney_and_user,
        format("money & ~D & ~D & ~D & ~D & ~3f & ~3f \\\\ \\hline ",
	[Tmoney,Tmoney_and1p1,Tmoney_and_user,Tmoney_and,Tciao_money_and,Tciao_money_and_user]),

% mqu
% 	Tciao_mqu_and is Tmqu/Tmqu_and,
% 	Tciao_mqu_and_user is Tmqu/Tmqu_and_user,
        format("mqu & $>$ 2 days & ~D & ~D & ~D & ---  & --- \\\\ \\hline ",
	[Tmqu_and1p1,Tmqu_and_user,Tmqu_and]),

% mutest
 	Tciao_mutest_and is Tmutest/Tmutest_and,
 	Tciao_mutest_and_user is Tmutest/Tmutest_and_user,
        format("mutest & ~D & ~D & ~D & ~D & ~3f & ~3f \\\\ \\hline ",
	[Tmutest,Tmutest_and1p1,Tmutest_and_user,Tmutest_and,Tciao_mutest_and,Tciao_mutest_and_user]),

% qu_evan
 	Tciao_qu_evan_and is Tqu_evan/Tqu_evan_and,
 	Tciao_qu_evan_and_user is Tqu_evan/Tqu_evan_and_user,
        format("qu\\_evan & ~D & ~D & ~D & ~D & ~3f & ~3f \\\\ \\hline ",
	[Tqu_evan,Tqu_evan_and1p1,Tqu_evan_and_user,Tqu_evan_and,Tciao_qu_evan_and,Tciao_qu_evan_and_user]),

% qu_vitor
 	Tciao_qu_vitor_and is Tqu_vitor/Tqu_vitor_and,
 	Tciao_qu_vitor_and_user is Tqu_vitor/Tqu_vitor_and_user,
        format("qu\\_vitor & ~D & ~D & ~D & ~D & ~3f & ~3f \\\\ \\hline ",
	[Tqu_vitor,Tqu_vitor_and1p1,Tqu_vitor_and_user,Tqu_vitor_and,Tciao_qu_vitor_and,Tciao_qu_vitor_and_user]),

	display('
        \\hline 
    \\end{tabular}
    \\caption{Execution times in milliseconds}
    
  \\end{center}
\\end{table}

\\end{document}
                 '),
        close(File),
	atom_concat([latex,' ','bench.tex'],Commandlatex),
	atom_concat([xdvi,' ','bench.dvi',' &'],Commanddvi),
	shell(Commandlatex),
	shell(Commanddvi).



%fib
eval_fib(T):- 
	display(eval_fib),nl,
	time(_),
        e_fib,
	time(T).

eval_fib_and_user(T):- 
	display(eval_fib_and_user),nl,
	time(_),
        e_fib_and_user,
	time(T).


eval_fib_and(T):- 
	display(eval_fib_and),nl,
	time(_),
        e_fib_and,
	time(T).

eval_fib_and1p1(T):- 
	display(eval_fib_and1p1),nl,
	time(_),
        e_fib_and1p1,
	time(T).

e_fib:- 
        fib:fib(20,_X),
	fail.
e_fib.

e_fib_and_user:- 
        fib_and_user:fib(20,_X),
	fail.
e_fib_and_user.

e_fib_and:- 
        fib_and:fib(20,_X),
	fail.
e_fib_and.

e_fib_and1p1:- 
        fib_and1p1:fib(20,_X),
	fail.
e_fib_and1p1.

%crypt
eval_crypt(T):-
	display(eval_crypt),nl,
	time(_),
	e_crypt,
	time(T).

eval_crypt_and(T):-
	display(eval_crypt_and),nl,
	time(_),
	e_crypt_and,
	time(T).

eval_crypt_and_user(T):-
	display(eval_crypt_and_user),nl,
	time(_),
	e_crypt_and_user,
	time(T).

eval_crypt_and1p1(T):-
	display(eval_crypt_and1p1),nl,
	time(_),
	e_crypt_and1p1,
	time(T).

e_crypt:-
	crypt:go,
	fail.
e_crypt.

e_crypt_and:-
	crypt_and:go,
	fail.
e_crypt_and.

e_crypt_and_user:-
	crypt_and_user:go,
	fail.
e_crypt_and_user.

e_crypt_and1p1:-
	crypt_and1p1:go,
	fail.
e_crypt_and1p1.

%dia_sums
eval_dia_sums(T):-
	display(eval_dia_sums),nl,
	time(_),
	e_dia_sums,
	time(T).

eval_dia_sums_and(T):-
	display(eval_dia_sums_and),nl,
	time(_),
	e_dia_sums_and,
	time(T).

eval_dia_sums_and_user(T):-
	display(eval_dia_sums_and_user),nl,
	time(_),
	e_dia_sums_and_user,
	time(T).

eval_dia_sums_and1p1(T):-
	display(eval_dia_sums_and1p1),nl,
	time(_),
	e_dia_sums_and1p1,
	time(T).

e_dia_sums:-
	dia_sums:test,
	fail.
e_dia_sums.

e_dia_sums_and:-
	dia_sums_and:test,
	fail.
e_dia_sums_and.

e_dia_sums_and_user:-
	dia_sums_and_user:test,
	fail.
e_dia_sums_and_user.

e_dia_sums_and1p1:-
	dia_sums_and1p1:test,
	fail.
e_dia_sums_and1p1.

%map
eval_map(T):-
	display(eval_map),nl,
	time(_),
	e_map,
	time(T).

eval_map_and(T):-
	display(eval_map_and),nl,
	time(_),
	e_map_and,
	time(T).

eval_map_and_user(T):-
	display(eval_map_user),nl,
	time(_),
	e_map_and_user,
	time(T).

eval_map_and1p1(T):-
	display(eval_map_and1p1),nl,
	time(_),
	e_map_and1p1,
	time(T).

e_map:-
	map:test_all(100),
	fail.
e_map.

e_map_and:-
	map_and:test_all(100),
	fail.
e_map_and.

e_map_and_user:-
	map_and_user:test_all(100),
	fail.
e_map_and_user.

e_map_and1p1:-
	map_and1p1:test_all(100),
	fail.
e_map_and1p1.

%money
eval_money(T):-
	display(eval_money),nl,
	time(_),
	e_money,
	time(T).

eval_money_and(T):-
	display(eval_money_and),nl,
	time(_),
	e_money_and,
	time(T).

eval_money_and_user(T):-
	display(eval_money_and_user),nl,
	time(_),
	e_money_and_user,
	time(T).

eval_money_and1p1(T):-
	display(eval_money_and1p1),nl,
	time(_),
	e_money_and1p1,
	time(T).

e_money:-
	money:test,
	fail.
e_money.

e_money_and:-
	money_and:test,
	fail.
e_money_and.

e_money_and_user:-
	money_and_user:test,
	fail.
e_money_and_user.

e_money_and1p1:-
	money_and1p1:test,
	fail.
e_money_and1p1.

%mqu
%eval_mqu(T):-
%	time(_),
%	mqu:queens(s(s(s(s(s(s(s(s(0))))))))),
%	time(T).

eval_mqu_and(T):-
	display(eval_mpu_and),nl,
	time(_),
	e_mqu_and,
	time(T).

eval_mqu_and_user(T):-
	display(eval_mpu_and_user),nl,
	time(_),
	e_mqu_and_user,
	time(T).

eval_mqu_and1p1(T):-
	display(eval_mpu_and1p1),nl,
	time(_),
	e_mqu_and1p1,
	time(T).

e_mqu_and:-
	mqu_and:queens(s(s(s(s(s(s(s(s(0))))))))),
	fail.
e_mqu_and.

e_mqu_and_user:-
	mqu_and_user:queens(s(s(s(s(s(s(s(s(0))))))))),
	fail.
e_mqu_and_user.

e_mqu_and1p1:-
	mqu_and1p1:queens(s(s(s(s(s(s(s(s(0))))))))),
	fail.
e_mqu_and1p1.

%mutest
eval_mutest(T):-
	display(eval_mutest),nl,
	time(_),
	e_mutest,
	time(T).

eval_mutest_and(T):-
	display(eval_mutest_and),nl,
	time(_),
	e_mutest_and,
	time(T).

eval_mutest_and_user(T):-
	display(eval_mutest_and_user),nl,
	time(_),
	e_mutest_and_user,
	time(T).

eval_mutest_and1p1(T):-
	display(eval_mutest_and1p1),nl,
	time(_),
	e_mutest_and1p1,
	time(T).

e_mutest:-
	mutest:test,
	fail.
e_mutest.

e_mutest_and:-
	mutest_and:test,
	fail.
e_mutest_and.

e_mutest_and_user:-
	mutest_and_user:test,
	fail.
e_mutest_and_user.

e_mutest_and1p1:-
	mutest_and1p1:test,
	fail.
e_mutest_and1p1.

%qu_evan
eval_qu_evan(T):-
	display(eval_qu_evan),nl,
	time(_),
	e_qu_evan,
	time(T).

eval_qu_evan_and(T):-
	display(eval_qu_evan_and),nl,
	time(_),
	e_qu_evan_and,
	time(T).

eval_qu_evan_and_user(T):-
	display(eval_qu_evan_and_user),nl,
	time(_),
	e_qu_evan_and_user,
	time(T).

eval_qu_evan_and1p1(T):-
	display(eval_qu_evan_and1p1),nl,
	time(_),
	e_qu_evan_and1p1,
	time(T).

e_qu_evan:-
	qu_evan:queen(8,_),
	fail.
e_qu_evan.

e_qu_evan_and:-
	qu_evan_and:queen(8,_),
	fail.
e_qu_evan_and.

e_qu_evan_and_user:-
	qu_evan_and_user:queen(8,_),
	fail.
e_qu_evan_and_user.

e_qu_evan_and1p1:-
	qu_evan_and1p1:queen(8,_),
	fail.
e_qu_evan_and1p1.

%qu_vitor
eval_qu_vitor(T):-
	display(eval_qu_vitor),nl,
	time(_),
	e_qu_vitor,
	time(T).

eval_qu_vitor_and(T):-
	display(eval_qu_vitor_and),nl,
	time(_),
	e_qu_vitor_and,
	time(T).

eval_qu_vitor_and_user(T):-
	display(eval_qu_vitor_and_user),nl,
	time(_),
	e_qu_vitor_and_user,
	time(T).

eval_qu_vitor_and1p1(T):-
	display(eval_qu_vitor_and1p1),nl,
	time(_),
	e_qu_vitor_and1p1,
	time(T).

e_qu_vitor:-
	qu_vitor:run(8,_),
	fail.
e_qu_vitor.

e_qu_vitor_and:-
	qu_vitor_and:run(8,_),
	fail.
e_qu_vitor_and.

e_qu_vitor_and_user:-
	qu_vitor_and_user:run(8,_),
	fail.
e_qu_vitor_and_user.

e_qu_vitor_and1p1:-
	qu_vitor_and1p1:run(8,_),
	fail.
e_qu_vitor_and1p1.



% eval_p(T):- 
% 	time(_),
%         nodet:p(_X),
% 	time(T).

% eval_p_and(T):- 
% 	time(_),
%         nodet_and:p(_X),
% 	time(T).
