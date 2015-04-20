:- module(responsible_persons_upm, [responsible/6], [assertions]).

:- comment( responsible(Project,WP,ID,Start,End,MM1,MM2,MM3,MM4),
   "@var{WP} is the workpackage number within @var{Project}. @var{ID}
   is the task identifier within the @var{WP}. @var{Start} is the
   starting month of the task. The first month of the project is month
   0 and the last month in a 3 year project is month 35. @var{End} is
   the month in which the work in the task is suppossed to
   finish. @var{MMX} is the number of MM which partner @var{X} should
   devote to the task.").

%:- trust pred responsible(A,B,C,D,E,F) => ground([A,B,C,D,E,F]).

:- check pred responsible(A,B,C,D,E,F) => ground([A,B,C,D,E,F]).

%WP1 manag
responsible(asap,1,1,1,[german,herme],[0.8,0.2]).
responsible(asap,1,2,1,[german,herme,astrid],[0.4,0.4,0.2]).

%WP2 req
%responsible(asap,2,1,0, 6,0,6,2,1).
%responsible(asap,2,2,3,12,0,4,3,3).

%WP3 basic
responsible(asap,3,1,1,[german,bueno,herme],[0.2,0.4,0.4]).
responsible(asap,3,2,1,[german,pedro],[0.3,0.7]).

%WP4 resou
responsible(asap,4,1,1,[jfran,pedro],[0.75,0.25]).
%responsible(asap,4,2, 6,15,0,0,3,0). %not UPM
responsible(asap,4,3,1,[german],[1]).
responsible(asap,4,4,1,[noone],[1]).
responsible(asap,4,5,1,[noone],[1]).

%WP5 seman
%% responsible(asap,5,1,18,24,0,0,3,1).
%% responsible(asap,5,2,24,30,0,0,2,6).
%% responsible(asap,5,3,21,27,0,2,2,0).

%WP6 valid
responsible(asap,6,1,1,[noone],[1]).
responsible(asap,6,2,1,[noone],[1]).
responsible(asap,6,3,1,[noone],[1]).

%WP7 tool
responsible(asap,7,1,1,[jesus,german,bueno,pedro,herme],[0.0,0.2,0.1,0.5,0.2]).
responsible(asap,7,2,1,[pedro,german,bueno,herme],[0.25,0.25,0.25,0.25]).
responsible(asap,7,3,1,[noone],[1]).

%WP8 assess
responsible(asap,8,1,1,[noone],[1]).
responsible(asap,8,2,1,[noone],[1]).
%responsible(asap,8,3,32,35,0,0,4,2).
responsible(asap,8,4,1,[noone],[1]).

%WP9 dissem
responsible(asap,9,1,1,[noone],[1]).
responsible(asap,9,2,1,[noone],[1]).
responsible(asap,9,3,1,[noone],[1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
responsible(colognet,1,german,1,[german],[1]).
responsible(colognet,1,herme,1,[herme],[1]).
responsible(colognet,1,boris,1,[boris],[1]).
responsible(colognet,1,bardo,1,[bardo],[1]).
responsible(colognet,1,bueno,1,[bueno],[1]).
responsible(colognet,1,claudio,1,[claudio],[1]).

responsible(colognet,2,german,1,[german],[1]).
responsible(colognet,2,herme,1,[herme],[1]).
responsible(colognet,2,boris,1,[boris],[1]).
responsible(colognet,2,bueno,1,[bueno],[1]).
responsible(colognet,2,bardo,1,[bardo],[1]).
responsible(colognet,2,bardo2,1,[bardo],[1]).

responsible(colognet,3,1,1,[noone],[1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
responsible(amos,1,herme,1,[herme],[1]).
responsible(amos,1,german,1,[german],[1]).
responsible(amos,1,boris,1,[boris],[1]).
responsible(amos,1,jesus,1,[jesus],[1]).
%responsible(amos,1,1.2,1,[herme],[1]).
responsible(amos,1,german2,1,[german],[1]).
responsible(amos,1,boris2,1,[boris],[1]).
responsible(amos,1,jesus2,1,[jesus],[1]).

responsible(amos,2,herme,1,[herme],[1]).
responsible(amos,2,german,1,[german],[1]).
responsible(amos,2,boris,1,[boris],[1]).
responsible(amos,2,bardo,1,[bardo],[1]).
responsible(amos,2,jmanuel,1,[jmanuel],[1]).
responsible(amos,2,herme2,1,[herme ],[1]).
responsible(amos,2,german2,1,[german],[1]).
responsible(amos,2,boris2,1,[boris],[1]).
responsible(amos,2,bardo2,1,[bardo],[1]).
responsible(amos,2,jmanuel2,1,[jmanuel],[1]).
