:- module(dates_and_facts,
	[person/1,
	 get_list_people/1,
	 next_month/4,
	 check_date_and_convert/4,
	 convert_date/4,
	 convert_date_origin/3,
	 month_to_number/2,
	 start_period/2,
	 end_period/2,
	 project/1,
	 get_list_projects/1,
	 max_hours_per_month/1,
	 hours_per_month/1,
	 max_hours_per_year/1,
	 cost_per_hour/2,
	 category/2,
	 date_less_than/4,
	 date_less_or_equal/4
	],
	 [assertions]).

:- use_module(library(aggregates), [findall/3]).

% :- trust pred person(A) => ground(A).
% :- trust pred get_list_people(A) => ground(A).
% :- trust pred max_hours_per_month(A) => ground(A).
% :- trust pred hours_per_month(A) => ground(A).
% :- trust pred max_hours_per_year(A) => ground(A).
% :- trust pred next_month(A,B,C,D) => ground([A,C]), mshare([[B,D]]).
% :- trust pred month_to_number(A,B) => ground([A,B]).
% :- trust pred project(A) => ground(A).
% :- trust pred get_list_projects(A) => ground(A).
% :- trust pred start_period(A,B) => ground([A,B]).
% :- trust pred end_period(A,B) => ground([A,B]).
% :- trust pred cost_per_hour(A,B) => ground([A,B]).
% :- trust pred category(A,B) => ground([A,B]).

:- check pred person(A) => ground(A).
:- check pred get_list_people(A) => ground(A).
:- check pred max_hours_per_month(A) => ground(A).
%:- check pred hours_per_month(A) => ground(A).
%:- check pred max_hours_per_year(A) => ground(A).
:- check pred next_month(A,B,C,D) => ground([A,C]).
:- check pred month_to_number(A,B) => ground([A,B]).
:- check pred project(A) => ground(A).
:- check pred get_list_projects(A) => ground(A).
:- check pred start_period(A,B) => ground([A,B]).
:- check pred end_period(A,B) => ground([A,B]).
:- check pred cost_per_hour(A,B) => ground([A,B]).
:- check pred category(A,B) => ground([A,B]).
:- check pred convert_date_origin(A,B,C):ground([A,B]) => ground(C).
:- check pred convert_date(A,B,C,D): ground([A,B,C]) => ground(D).
:- check pred check_date_and_convert(A,B,C,D): ground([A,B,C]) => ground(D).
:- check pred date_less_than(A,B,C,D): ground([A,B,C,D]).
:- check pred date_less_or_equal(A,B,C,D): ground([A,B,C,D]).

person(german).
person(herme).
person(bueno).
person(bardo).
person(boris).
person(jesus).
person(claudio).
person(pedro).
person(jmanuel).
person(jfran).
person(astrid).
person(susana).
person(jorge).
person(noone).


get_list_people(L):-
	findall(P, person(P), L).


max_hours_per_month(143).


hours_per_month(131).


max_hours_per_year(157).


next_month(jan,Y,feb,Y).
next_month(feb,Y,mar,Y).
next_month(mar,Y,apr,Y).
next_month(apr,Y,may,Y).
next_month(may,Y,jun,Y).
next_month(jun,Y,jul,Y).
next_month(jul,Y,aug,Y).
next_month(aug,Y,sep,Y).
next_month(sep,Y,oct,Y).
next_month(oct,Y,nov,Y).
next_month(nov,Y,dec,Y).
next_month(dec,Y,jan,Y1):-
	Y1 is Y + 1.

check_date_and_convert(asap,M,Y,Month):-!,
	convert_date_origin(M,Y,Date),
	Date >= 10,
	Date < 46, 
	Month is Date -10.
check_date_and_convert(colognet,M,Y,Month):-!,
	convert_date_origin(M,Y,Date),
	Date >= 0,
	Date < 36, 
	Month is Date.
check_date_and_convert(amos,M,Y,Month):-
	convert_date_origin(M,Y,Date),
	Date >= 2,
	Date < 26, 
	Month is Date -2.


convert_date(asap,M,Y,Month):-
	convert_date_origin(M,Y,Date),
	Month is Date -10.
convert_date(colognet,M,Y,Month):-
	convert_date_origin(M,Y,Month).
convert_date(amos,M,Y,Month):-
	convert_date_origin(M,Y,Date),
	Month is Date -2.



convert_date_origin(M,Y,Date):-
	month_to_number(M,M_Num),
	Date is M_Num -1 + 12*(Y-2).


month_to_number(jan,1).
month_to_number(feb,2).
month_to_number(mar,3).
month_to_number(apr,4).
month_to_number(may,5).
month_to_number(jun,6).
month_to_number(jul,7).
month_to_number(aug,8).
month_to_number(sep,9).
month_to_number(oct,10).
month_to_number(nov,11).
month_to_number(dec,12).



project(asap).
project(amos).
project(colognet).


get_list_projects(L):-
	findall(P, project(P), L).


start_period(jan,02).


end_period(nov,05).


cost_per_hour(prof,39.49).
cost_per_hour(asste_prof,23.60).
cost_per_hour(assnt_prof,24.02).
cost_per_hour(astrid,12.86).
cost_per_hour(fpi,7.50).


category(german,asste_prof).
category(herme,prof).
category(bueno,asste_prof).
category(bardo,assnt_prof).
category(boris,assnt_prof).
category(jesus,fpi).
category(claudio,fpi).
category(pedro,assnt_prof).
category(jmanuel,fpi).
category(jfran,fpi).
category(astrid,astrid).
category(susana,assnt_prof).
category(jorge,fpi).
category(noone,_):- fail.

date_less_than(_M1,Y1,_M2,Y2):-
	Y1 < Y2,!.
date_less_than(M1,Year,M2,Year):-
	M1 < M2.

date_less_or_equal(M,Y,M,Y):-!.
date_less_or_equal(M1,Y1,M2,Y2):-
	date_less_than(M1,Y1,M2,Y2).
