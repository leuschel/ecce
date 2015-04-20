:- module(payments,[payment/7,received/7],[assertions]).

%:- use_package(assertions).

:- use_module(dates_and_facts).
:- use_module(planned).
:- use_module(library(aggregates)).

:- check pred payment(A,B,C,D,E,F,G): ground([A,B]) => ground([C,D,E,F,G]).

payment(herme,asap,nov,02,jan,04,1).
payment(herme,amos,mar,02,jan,04,1).
payment(herme,colognet,jan,02,jan,04,1).
%
payment(german,asap,nov,02,nov,03,1).
payment(german,amos,mar,02,jan,04,1).
payment(german,colognet,jan,02,jan,04,1).
%
payment(bueno,asap,nov,02,jan,04,1).
payment(bueno,omnipaper,nov,02,jan,04,1).
%
payment(boris,amos,mar,02,jan,04,1).
%
payment(pedro,asap,nov,02,feb,04,1).
%
payment(bardo,asap,nov,02,jan,04,1).
payment(bardo,colognet,nov,02,jan,04,1).


:- check pred received(Person,Project,SM,SY,EM,EY,Amount): ground([Person,Project,SM,SY,EM,EY]) 
                  => ground(Amount).

%:- entry received(Person,Project,SM,SY,EM,EY,Amount): ground([Person,Project,SM,SY,EM,EY,Amount]).

received(Person,Project,SM,SY,EM,EY,Amount):-
	findall(p(SM1,SY1,EM1,EY1,Total),
	    payment(Person,Project,SM1,SY1,EM1,EY1,Total),L),
	add_wages(L,SM,SY,EM,EY,Amount).

% Error version: 
% add_wages([],_,_,_,_,_). 

% Error-free version:
add_wages([],_,_,_,_,0). 

add_wages([p(M1,Y1,_,_,_)|Ps],SM,SY,EM,EY,Amount):-
	date_less_than(EM,EY,M1,Y1),!,
	add_wages(Ps,SM,SY,EM,EY,Amount).
add_wages([p(_,_,M2,Y2,_)|Ps],SM,SY,EM,EY,Amount):-
	date_less_than(M2,Y2,SM,SY),!,
	add_wages(Ps,SM,SY,EM,EY,Amount).
add_wages([p(M1,Y1,M2,Y2,Total)|Ps],SM,SY,EM,EY,Amount):-
	add_wages(Ps,SM,SY,EM,EY,Tmp),
	convert_date_origin(M1,Y1,S),
	convert_date_origin(M2,Y2,E),
	convert_date_origin(SM,SY,Start),
	convert_date_origin(EM,EY,End),
	intersection(S,E,Start,End,Lower,Upper),
	length(Lower,Upper,Actual_Length),
	length(S,E,Full_Length),
	Amount is Tmp + Total*Actual_Length/Full_Length.
