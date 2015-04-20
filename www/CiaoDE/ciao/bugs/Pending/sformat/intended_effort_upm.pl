:- comment(intended_effort(Project,WP,ID,Partner,Effort),
"@var{Effort} is the number of MMs which @var{Partner} intends to
devote to task @var{ID} within @var{WP} of @var{Project}.").

%WP1 manag
intended_effort(asap,1,1,1,2).
intended_effort(asap,1,2,1,7).

%WP2 req
%% intended_effort(asap,2,1,1,0). %not UPM
%% intended_effort(asap,2,2,1,0). %not UPM

%WP3 basic
intended_effort(asap,3,1,1,7). % was 4
intended_effort(asap,3,2,1,1). % was 4

%WP4 resou
intended_effort(asap,4,1,1,4).
%intended_effort(asap,4,2,1,0). %not UPM
intended_effort(asap,4,3,1,1).
intended_effort(asap,4,4,1,6).
intended_effort(asap,4,5,1,5).

%WP5 seman
%% intended_effort(asap,5,1,1,0).
%% intended_effort(asap,5,2,1,0).
%% intended_effort(asap,5,3,1,0).

%WP6 valid
intended_effort(asap,6,1,1,3.33). % was 2!
intended_effort(asap,6,2,1,6).
intended_effort(asap,6,3,1,6).

%WP7 tool
intended_effort(asap,7,1,1,5). % was 4
intended_effort(asap,7,2,1,6). % tool was 5
intended_effort(asap,7,3,1,11).

%WP8 assess
intended_effort(asap,8,1,1,4).
intended_effort(asap,8,2,1,4).
%intended_effort(asap,8,3,1,0). %not UPM
intended_effort(asap,8,4,1,2).

%WP9 dissem
intended_effort(asap,9,1,1,2).
intended_effort(asap,9,2,1,2).
intended_effort(asap,9,3,1,2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
intended_effort(colognet,1,german,1,1.21904761904761904761). %160 hours german
intended_effort(colognet,1,herme,1,0.76190476190476190476). %100 hours herme
intended_effort(colognet,1,boris,1,0.60952380952380952380). % 80 hours boris
intended_effort(colognet,1,bardo,1,1.52380952380952380952). %200 hours bardo
intended_effort(colognet,1,bueno,1,0.38095238095238095238). % 50 hours bueno
intended_effort(colognet,1,claudio,1,0.26666666666666666666). % 35 hours claudio

intended_effort(colognet,2,german,1,X):- X is 210/131.25. 
intended_effort(colognet,2,herme,1,X):-  X is 215/131.25. 
intended_effort(colognet,2,boris,1,X):-  X is 40/131.25.  
intended_effort(colognet,2,bardo,1,X):- X is  100/131.25. 
intended_effort(colognet,2,bardo2,1,X):-  X is 95/131.25. 
intended_effort(colognet,2,jorge,1,X):- X is  80/131.25. 
intended_effort(colognet,2,susana,1,X):-  X is 16/131.25. 

intended_effort(colognet,3,1,1,4). % 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
intended_effort(amos,1,herme,1,X):- X is 150/131.25. %Herme
intended_effort(amos,1,german,1,X):- X is 400/131.25. %German
intended_effort(amos,1,boris,1,X):- X is 990/131.25. %Boris
intended_effort(amos,1,jesus,1,X):- X is 507/131.25. %Jesus
%intended_effort(amos,1,herme2,1,X):- X is 0/131.25. %Herme sin horas en 2003
intended_effort(amos,1,german2,1,X):- X is 30/131.25. %German
intended_effort(amos,1,boris2,1,X):- X is 200/131.25. %Boris
intended_effort(amos,1,jesus2,1,X):- X is 125/131.25. %Jesus

intended_effort(amos,2,herme,1,X):-    X is  300/131.25. %Herme
intended_effort(amos,2,german,1,X):-   X is  250/131.25. %German
intended_effort(amos,2,boris,1,X):-    X is 1200/131.25. %Boris
intended_effort(amos,2,bardo,1,X):-    X is  100/131.25. %Bardo
intended_effort(amos,2,jmanuel,1,X):-  X is  300/131.25. %JManuel
intended_effort(amos,2,herme2,1,X):-   X is  200/131.25. %Herme 
intended_effort(amos,2,german2,1,X):-  X is  100/131.25. %German
intended_effort(amos,2,boris2,1,X):-   X is  275/131.25. %Boris
intended_effort(amos,2,bardo2,1,X):-   X is   75/131.25. %Bardo
intended_effort(amos,2,jmanuel2,1,X):- X is  50/131.25. %JManuel

total_amos(X):-
	Prometidos is 21.7,
	findall(X,intended_effort(amos,2,_,_,X),List_Effort),
	subtract_list(List_Effort,Prometidos,X).

subtract_list([],P,P).
subtract_list([X|Xs],P,NP):-
	subtract_list(Xs,P,Tmp),
	NP is Tmp - X.
	
%intended_effort(amos,2,1,1,21.7). % todos
