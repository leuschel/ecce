:- module(committed_hours,[committed_hours/6],[assertions]).

% :- trust pred committed_hours(Month,Year,Person,Teaching,Other,Vacation) 
% 	=> ground([Month,Year,Person,Teaching,Other,Vacation]).

:- comment(committed_hours(Month,Year,Person,Teaching,Other,Vacation),
"@var{Teaching},@var{Other}, and @var{Vacation} are the number of
hours which @var{Person} devotes to such activities during
@var{Month}, @var{Year}").

:-  pred committed_hours(Month,Year,Person,Teaching,Other,Vacation) :
	ground([Month,Year,Person])
 	=> ground([Month,Year,Person,Teaching,Other,Vacation]).


committed_hours(jan,02,german, 0,2,0).
committed_hours(feb,02,german, 0,2,0).
committed_hours(mar,02,german, 9,2,0).
committed_hours(apr,02,german,12,2,0).
committed_hours(may,02,german, 6,1,0).
committed_hours(jun,02,german, 0,1,0).
committed_hours(jul,02,german, 0,1,0).
committed_hours(aug,02,german, 0,1,80). %100
committed_hours(sep,02,german, 3,1,20). %0
committed_hours(oct,02,german,12,1,0).
committed_hours(nov,02,german,12,1,0).
committed_hours(dec,02,german, 9,1,41).
%
committed_hours(jan,03,german, 9,2,0).
committed_hours(feb,03,german,12,2,0).
committed_hours(mar,03,german, 8,2,0).
committed_hours(apr,03,german, 8,2,0).
committed_hours(may,03,german, 2,1,0).
committed_hours(jun,03,german, 0,1,0).
committed_hours(jul,03,german, 0,1,50).
committed_hours(aug,03,german, 0,1,50).
committed_hours(sep,03,german, 0,1,0). 
committed_hours(oct,03,german, 0,1,0).
committed_hours(nov,03,german, 0,1,0).
committed_hours(dec,03,german, 0,1,41).
%
committed_hours(jan,04,german, 0,0,0).
committed_hours(feb,04,german, 0,0,0).
committed_hours(mar,04,german,12,0,0).
committed_hours(apr,04,german,12,0,0).
committed_hours(may,04,german,12,0,0).
committed_hours(jun,04,german, 0,0,0).
committed_hours(jul,04,german, 0,0,0).
committed_hours(aug,04,german, 0,0,100).
committed_hours(sep,04,german, 3,0,0).
committed_hours(oct,04,german, 0,0,0).
committed_hours(nov,04,german, 0,0,0).
committed_hours(dec,04,german, 0,0,41).
%
committed_hours(jan,05,german, 0,0,0).
committed_hours(feb,05,german, 0,0,0).
committed_hours(mar,05,german,12,0,0).
committed_hours(apr,05,german,12,0,0).
committed_hours(may,05,german,12,0,0).
committed_hours(jun,05,german, 0,0,0).
committed_hours(jul,05,german, 0,0,0).
committed_hours(aug,05,german, 0,0,100).
committed_hours(sep,05,german, 3,0,0).
committed_hours(oct,05,german,12,0,0).
%%%%%%%%%%%%%%%%%%%%

committed_hours(jan,02,herme, 0,143,0).
committed_hours(feb,02,herme, 0,143,0).
committed_hours(mar,02,herme, 0,143,0).
committed_hours(apr,02,herme, 0,143,0).
committed_hours(may,02,herme, 0,143,0).
committed_hours(jun,02,herme, 0,143,0).
committed_hours(jul,02,herme, 0,143,0).
committed_hours(aug,02,herme, 0,143,0).
committed_hours(sep,02,herme, 0,0,0).
committed_hours(oct,02,herme, 0,0,0).
committed_hours(nov,02,herme, 0,0,0).
committed_hours(dec,02,herme, 0,0,41).
%
committed_hours(jan,03,herme, 0,0,0).
committed_hours(feb,03,herme, 8,0,0).
committed_hours(mar,03,herme, 8,0,0).
committed_hours(apr,03,herme, 8,0,0).
committed_hours(may,03,herme, 0,0,0).
committed_hours(jun,03,herme, 0,0,0).
committed_hours(jul,03,herme, 0,0,0).
committed_hours(aug,03,herme, 0,0,100).
committed_hours(sep,03,herme, 8,20,0).  %UNM
committed_hours(oct,03,herme, 8,20,0).  %UNM
committed_hours(nov,03,herme, 8,20,0).  %UNM
committed_hours(dec,03,herme, 8,20,41). %UNM
%
committed_hours(jan,04,herme, 0,0,0).
committed_hours(feb,04,herme, 8,0,0).
committed_hours(mar,04,herme, 8,0,0).
committed_hours(apr,04,herme, 8,0,0).
committed_hours(may,04,herme, 0,0,0).
committed_hours(jun,04,herme, 0,0,0).
committed_hours(jul,04,herme, 0,0,0).
committed_hours(aug,04,herme, 0,0,100).
committed_hours(sep,04,herme, 8,0,0).
committed_hours(oct,04,herme, 8,0,0).
committed_hours(nov,04,herme, 8,0,0).
committed_hours(dec,04,herme, 8,0,41).
%
committed_hours(jan,05,herme, 0,0,0).
committed_hours(feb,05,herme, 8,0,0).
committed_hours(mar,05,herme, 8,0,0).
committed_hours(apr,05,herme, 8,0,0).
committed_hours(may,05,herme, 0,0,0).
committed_hours(jun,05,herme, 0,0,0).
committed_hours(jul,05,herme, 0,0,0).
committed_hours(aug,05,herme, 0,0,100).
committed_hours(sep,05,herme, 8,0,0).
committed_hours(oct,05,herme, 8,0,0).
committed_hours(nov,05,herme, 8,0,0).
committed_hours(dec,05,herme, 8,0,41).


%%%%%%%%%%%%%%%%%%%%%%%
committed_hours(jan,02,pedro, 0,0,0).
committed_hours(feb,02,pedro,18,0,0).
committed_hours(mar,02,pedro,24,0,0).
committed_hours(apr,02,pedro,24,0,0).
committed_hours(may,02,pedro,24,0,0).
committed_hours(jun,02,pedro, 0,0,0).
committed_hours(jul,02,pedro, 0,0,0).
committed_hours(aug,02,pedro, 0,0,100).
committed_hours(sep,02,pedro, 0,0,0).
committed_hours(oct,02,pedro, 0,0,0).
committed_hours(nov,02,pedro, 2,0,0).
committed_hours(dec,02,pedro, 0,0,41).
%
committed_hours(jan,03,pedro, 0,0,0).
committed_hours(feb,03,pedro,18,0,0).
committed_hours(mar,03,pedro,24,0,0).
committed_hours(apr,03,pedro,24,0,0).
committed_hours(may,03,pedro,24,0,0).
committed_hours(jun,03,pedro, 0,0,0).
committed_hours(jul,03,pedro, 0,0,0).
committed_hours(aug,03,pedro, 0,0,100).
committed_hours(sep,03,pedro, 0,0,0).
committed_hours(oct,03,pedro, 0,0,0).
committed_hours(nov,03,pedro, 0,0,0).
committed_hours(dec,03,pedro, 0,0,41).
%
committed_hours(jan,04,pedro, 0,0,0).
committed_hours(feb,04,pedro,18,0,0).
committed_hours(mar,04,pedro,24,0,0).
committed_hours(apr,04,pedro,24,0,0).
committed_hours(may,04,pedro,24,0,0).
committed_hours(jun,04,pedro, 0,0,0).
committed_hours(jul,04,pedro, 0,0,0).
committed_hours(aug,04,pedro, 0,0,100).
committed_hours(sep,04,pedro, 0,0,0).
committed_hours(oct,04,pedro, 0,0,0).
committed_hours(nov,04,pedro, 0,0,0).
committed_hours(dec,04,pedro, 0,0,41).
%
committed_hours(jan,05,pedro, 0,0,0).
committed_hours(feb,05,pedro,18,0,0).
committed_hours(mar,05,pedro,24,0,0).
committed_hours(apr,05,pedro,24,0,0).
committed_hours(may,05,pedro,24,0,0).
committed_hours(jun,05,pedro, 0,0,0).
committed_hours(jul,05,pedro, 0,0,0).
committed_hours(aug,05,pedro, 0,0,100).
committed_hours(sep,05,pedro, 0,0,0).
committed_hours(oct,05,pedro, 0,0,0).
committed_hours(nov,05,pedro, 0,0,0).
committed_hours(dec,05,pedro, 0,0,41).
%%%%%%%%%%%%%%%%%%%%%%%
committed_hours(jan,02,bueno, 0,143,0).
committed_hours(feb,02,bueno, 0,143,0).
committed_hours(mar,02,bueno, 0,143,0).
committed_hours(apr,02,bueno, 0,143,0).
committed_hours(may,02,bueno, 0,143,0).
committed_hours(jun,02,bueno, 0,143,0).
committed_hours(jul,02,bueno, 0,143,0).
committed_hours(aug,02,bueno, 0,143,100).
committed_hours(sep,02,bueno, 0,143,0).
committed_hours(oct,02,bueno, 0,113,0).
committed_hours(nov,02,bueno, 0,0,0).
committed_hours(dec,02,bueno, 0,0,41).
%
committed_hours(jan,03,bueno, 6,62,0).
committed_hours(feb,03,bueno,20,55,0).
committed_hours(mar,03,bueno,20,55,0).
committed_hours(apr,03,bueno,20,55,0).
committed_hours(may,03,bueno,20,55,0).
committed_hours(jun,03,bueno,20,55,0).
committed_hours(jul,03,bueno, 0,65,0).
committed_hours(aug,03,bueno, 0,65,100).
committed_hours(sep,03,bueno, 0,65,0).
committed_hours(oct,03,bueno, 0,65,0).
committed_hours(nov,03,bueno, 0,65,0).
committed_hours(dec,03,bueno, 0,65,41).
%
committed_hours(jan,04,bueno, 6,62,0).
committed_hours(feb,04,bueno,20,55,0).
committed_hours(mar,04,bueno,20,55,0).
committed_hours(apr,04,bueno,20,55,0).
committed_hours(may,04,bueno,20,55,0).
committed_hours(jun,04,bueno,20,55,0).
committed_hours(jul,04,bueno, 0,65,0).
committed_hours(aug,04,bueno, 0,65,100).
committed_hours(sep,04,bueno, 0,65,0).
committed_hours(oct,04,bueno, 0,65,0).
committed_hours(nov,04,bueno, 0,65,0).
committed_hours(dec,04,bueno, 0,65,41).
%
committed_hours(jan,05,bueno, 6,62,0).
committed_hours(feb,05,bueno,20,55,0).
committed_hours(mar,05,bueno,20,55,0).
committed_hours(apr,05,bueno,20,55,0).
committed_hours(may,05,bueno,20,55,0).
committed_hours(jun,05,bueno,20,55,0).
committed_hours(jul,05,bueno, 0,65,0).
committed_hours(aug,05,bueno, 0,65,100).
committed_hours(sep,05,bueno, 0,65,0).
committed_hours(oct,05,bueno, 0,65,0).
committed_hours(nov,05,bueno, 0,65,0).
committed_hours(dec,05,bueno, 0,65,41).
%%%%%%%%%%%%%%%%%%%%%%%
committed_hours(jan,02,boris, 11,0,0).
committed_hours(feb,02,boris, 3,0,0).
committed_hours(mar,02,boris, 0,0,0).
committed_hours(apr,02,boris, 0,0,0).
committed_hours(may,02,boris, 0,1,0).
committed_hours(jun,02,boris, 0,1,0).
committed_hours(jul,02,boris, 0,1,0).
committed_hours(aug,02,boris, 0,1,100).
committed_hours(sep,02,boris, 3,1,0).
committed_hours(oct,02,boris,24,1,0).
committed_hours(nov,02,boris,28,1,0).
committed_hours(dec,02,boris, 15,1,41).
%
committed_hours(jan,03,boris, 6,1,0).
committed_hours(feb,03,boris, 4,1,0).
committed_hours(mar,03,boris, 0,1,0).
committed_hours(apr,03,boris, 0,1,0).
committed_hours(may,03,boris, 0,1,0).
committed_hours(jun,03,boris, 0,1,0).
committed_hours(jul,03,boris, 0,1,0).
committed_hours(aug,03,boris, 0,1,100).
committed_hours(sep,03,boris, 3,1,0).
committed_hours(oct,03,boris, 24,1,0).
committed_hours(nov,03,boris, 28,1,0).
committed_hours(dec,03,boris, 15,1,41).
%
committed_hours(jan,04,boris, 6,1,0).
committed_hours(feb,04,boris, 4,1,0).
committed_hours(mar,04,boris, 0,1,0).
committed_hours(apr,04,boris, 0,1,0).
committed_hours(may,04,boris, 0,1,0).
committed_hours(jun,04,boris, 0,1,0).
committed_hours(jul,04,boris, 0,1,0).
committed_hours(aug,04,boris, 0,1,100).
committed_hours(sep,04,boris, 3,1,0).
committed_hours(oct,04,boris, 24,1,0).
committed_hours(nov,04,boris, 28,1,0).
committed_hours(dec,04,boris, 15,1,41).
%
committed_hours(jan,05,boris, 6,1,0).
committed_hours(feb,05,boris, 4,1,0).
committed_hours(mar,05,boris, 0,1,0).
committed_hours(apr,05,boris, 0,1,0).
committed_hours(may,05,boris, 0,1,0).
committed_hours(jun,05,boris, 0,1,0).
committed_hours(jul,05,boris, 0,1,0).
committed_hours(aug,05,boris, 0,1,100).
committed_hours(sep,05,boris, 3,1,0).
committed_hours(oct,05,boris, 24,1,0).
committed_hours(nov,05,boris, 28,1,0).
committed_hours(dec,05,boris, 15,1,41).
%%%%%%%%%%%%%%%%%%%%%%%
committed_hours(jan,02,bardo, 12,96,0).
committed_hours(feb,02,bardo, 8,96,0).
committed_hours(mar,02,bardo, 8,96,0).
committed_hours(apr,02,bardo,12,96,0).
committed_hours(may,02,bardo, 6,96,0).
committed_hours(jun,02,bardo, 0,96,0).
committed_hours(jul,02,bardo, 0,96,0).
committed_hours(aug,02,bardo, 0,96,100).
committed_hours(sep,02,bardo, 3,96,0).
committed_hours(oct,02,bardo,12,96,0).
committed_hours(nov,02,bardo,12,96,0).
committed_hours(dec,02,bardo, 8,96,31). % split Xmas holidays
%
committed_hours(jan,03,bardo, 9,96,10). % to avoid negative availability
committed_hours(feb,03,bardo,11,96,0).
committed_hours(mar,03,bardo,12,96,0).
committed_hours(apr,03,bardo,13,96,0).
committed_hours(may,03,bardo,12,96,0).
committed_hours(jun,03,bardo, 0,96,0).
committed_hours(jul,03,bardo, 0,96,0).
committed_hours(aug,03,bardo, 0,96,100).
committed_hours(sep,03,bardo, 0,96,0).
committed_hours(oct,03,bardo, 0,96,0).
committed_hours(nov,03,bardo, 0,96,0).
committed_hours(dec,03,bardo, 0,96,41). 
%
committed_hours(jan,04,bardo, 9,96,0).
committed_hours(feb,04,bardo,11,96,0).
committed_hours(mar,04,bardo,12,0,0).
committed_hours(apr,04,bardo,13,0,0).
committed_hours(may,04,bardo,12,0,0).
committed_hours(jun,04,bardo, 0,0,0).
committed_hours(jul,04,bardo, 0,0,0).
committed_hours(aug,04,bardo, 0,0,100).
committed_hours(sep,04,bardo, 0,0,0).
committed_hours(oct,04,bardo, 0,0,0).
committed_hours(nov,04,bardo, 0,0,0).
committed_hours(dec,04,bardo, 0,0,41).
%
committed_hours(jan,05,bardo, 9,0,0).
committed_hours(feb,05,bardo,11,0,0).
committed_hours(mar,05,bardo,12,0,0).
committed_hours(apr,05,bardo,13,0,0).
committed_hours(may,05,bardo,12,0,0).
committed_hours(jun,05,bardo, 0,0,0).
committed_hours(jul,05,bardo, 0,0,0).
committed_hours(aug,05,bardo, 0,0,100).
committed_hours(sep,05,bardo, 0,0,0).
committed_hours(oct,05,bardo, 0,0,0).
committed_hours(nov,05,bardo, 0,0,0).
committed_hours(dec,05,bardo, 0,0,41).
%%%%%%%%%%%%%%%%%%%%%%%
committed_hours(jan,02,jesus, 0,57,0).
committed_hours(feb,02,jesus, 15,57,0).
committed_hours(mar,02,jesus, 14,15,0).
committed_hours(apr,02,jesus, 21,15,0).
committed_hours(may,02,jesus, 17,1,0).
committed_hours(jun,02,jesus, 0,57,0).
committed_hours(jul,02,jesus, 12,34,0).
committed_hours(aug,02,jesus, 0,47,100).
committed_hours(sep,02,jesus, 0,1,0).
committed_hours(oct,02,jesus, 0,1,0).
committed_hours(nov,02,jesus, 0,1,0).
committed_hours(dec,02,jesus, 0,47,41).
%
committed_hours(jan,03,jesus, 0,1,0).
committed_hours(feb,03,jesus, 0,143,0).
committed_hours(mar,03,jesus, 0,143,0).
committed_hours(apr,03,jesus, 0,143,0).
committed_hours(may,03,jesus, 0,143,0).
committed_hours(jun,03,jesus, 0,143,0).
committed_hours(jul,03,jesus, 0,143,0).
committed_hours(aug,03,jesus, 0,143,100).
committed_hours(sep,03,jesus, 0,143,0).
committed_hours(oct,03,jesus, 0,143,0).
committed_hours(nov,03,jesus, 0,143,0).
committed_hours(dec,03,jesus, 0,143,41).
%
committed_hours(jan,04,jesus, 0,143,0).
committed_hours(feb,04,jesus, 0,143,0).
committed_hours(mar,04,jesus, 0,143,0).
committed_hours(apr,04,jesus, 0,143,0).
committed_hours(may,04,jesus, 0,143,0).
committed_hours(jun,04,jesus, 0,143,0).
committed_hours(jul,04,jesus, 0,143,0).
committed_hours(aug,04,jesus, 0,143,100).
committed_hours(sep,04,jesus, 0,143,0).
committed_hours(oct,04,jesus, 0,143,0).
committed_hours(nov,04,jesus, 0,143,0).
committed_hours(dec,04,jesus, 0,143,41).
%
committed_hours(jan,05,jesus, 0,143,0).
committed_hours(feb,05,jesus, 0,143,0).
committed_hours(mar,05,jesus, 0,143,0).
committed_hours(apr,05,jesus, 0,143,0).
committed_hours(may,05,jesus, 0,143,0).
committed_hours(jun,05,jesus, 0,143,0).
committed_hours(jul,05,jesus, 0,143,0).
committed_hours(aug,05,jesus, 0,143,100).
committed_hours(sep,05,jesus, 0,143,0).
committed_hours(oct,05,jesus, 0,143,0).
committed_hours(nov,05,jesus, 0,143,0).
committed_hours(dec,05,jesus, 0,143,41).
%%%%%%%%%%%%%%%%%%%%%%%
committed_hours(jan,02,claudio, 0,0,0).
committed_hours(feb,02,claudio, 0,0,0).
committed_hours(mar,02,claudio, 0,0,0).
committed_hours(apr,02,claudio, 0,0,0).
committed_hours(may,02,claudio, 0,0,0).
committed_hours(jun,02,claudio, 0,0,0).
committed_hours(jul,02,claudio, 0,0,0).
committed_hours(aug,02,claudio, 0,0,0).
committed_hours(sep,02,claudio, 0,0,0).
committed_hours(oct,02,claudio, 0,0,0).
committed_hours(nov,02,claudio, 0,0,0).
committed_hours(dec,02,claudio, 0,0,0).
%
committed_hours(jan,03,claudio, 0,0,0).
committed_hours(feb,03,claudio, 0,143,0).
committed_hours(mar,03,claudio, 0,143,0).
committed_hours(apr,03,claudio, 0,143,0).
committed_hours(may,03,claudio, 0,143,0).
committed_hours(jun,03,claudio, 0,143,0).
committed_hours(jul,03,claudio, 0,143,0).
committed_hours(aug,03,claudio, 0,143,100).
committed_hours(sep,03,claudio, 0,143,0).
committed_hours(oct,03,claudio, 0,143,0).
committed_hours(nov,03,claudio, 0,143,0).
committed_hours(dec,03,claudio, 0,143,41).
%
committed_hours(jan,04,claudio, 0,143,0).
committed_hours(feb,04,claudio, 0,143,0).
committed_hours(mar,04,claudio, 0,143,0).
committed_hours(apr,04,claudio, 0,143,0).
committed_hours(may,04,claudio, 0,143,0).
committed_hours(jun,04,claudio, 0,143,0).
committed_hours(jul,04,claudio, 0,143,0).
committed_hours(aug,04,claudio, 0,143,100).
committed_hours(sep,04,claudio, 0,143,0).
committed_hours(oct,04,claudio, 0,143,0).
committed_hours(nov,04,claudio, 0,143,0).
committed_hours(dec,04,claudio, 0,143,41).
%
committed_hours(jan,05,claudio, 0,143,0).
committed_hours(feb,05,claudio, 0,143,0).
committed_hours(mar,05,claudio, 0,143,0).
committed_hours(apr,05,claudio, 0,143,0).
committed_hours(may,05,claudio, 0,143,0).
committed_hours(jun,05,claudio, 0,143,0).
committed_hours(jul,05,claudio, 0,143,0).
committed_hours(aug,05,claudio, 0,143,100).
committed_hours(sep,05,claudio, 0,143,0).
committed_hours(oct,05,claudio, 0,143,0).
committed_hours(nov,05,claudio, 0,143,0).
committed_hours(dec,05,claudio, 0,143,41).

%%%%%%%%%%%%%
committed_hours(nov,02,jmanuel, 0,0,0).
committed_hours(dec,02,jmanuel, 0,0,41).
%
committed_hours(jan,03,jmanuel, 0,0,0).
committed_hours(feb,03,jmanuel, 0,0,0).
committed_hours(mar,03,jmanuel, 0,0,0).
committed_hours(apr,03,jmanuel, 0,0,0).
committed_hours(may,03,jmanuel, 0,0,0).
committed_hours(jun,03,jmanuel, 0,0,0).
committed_hours(jul,03,jmanuel, 0,0,50).
committed_hours(aug,03,jmanuel, 0,0,50).
committed_hours(sep,03,jmanuel, 0,0,0).
committed_hours(oct,03,jmanuel, 0,0,0).
committed_hours(nov,03,jmanuel, 0,0,0).
committed_hours(dec,03,jmanuel, 0,0,41).
%
committed_hours(jan,04,jmanuel, 0,0,0).
committed_hours(feb,04,jmanuel, 0,0,0).
committed_hours(mar,04,jmanuel, 0,0,0).
committed_hours(apr,04,jmanuel, 0,0,0).
committed_hours(may,04,jmanuel, 0,0,0).
committed_hours(jun,04,jmanuel, 0,0,0).
committed_hours(jul,04,jmanuel, 0,0,0).
committed_hours(aug,04,jmanuel, 0,0,100).
committed_hours(sep,04,jmanuel, 0,0,0).
committed_hours(oct,04,jmanuel, 0,0,0).
committed_hours(nov,04,jmanuel, 0,0,0).
committed_hours(dec,04,jmanuel, 0,0,41).
%
committed_hours(jan,05,jmanuel, 0,0,0).
committed_hours(feb,05,jmanuel, 0,0,0).
committed_hours(mar,05,jmanuel, 0,0,0).
committed_hours(apr,05,jmanuel, 0,0,0).
committed_hours(may,05,jmanuel, 0,0,0).
committed_hours(jun,05,jmanuel, 0,0,0).
committed_hours(jul,05,jmanuel, 0,0,0).
committed_hours(aug,05,jmanuel, 0,0,100).
committed_hours(sep,05,jmanuel, 0,0,0).
committed_hours(oct,05,jmanuel, 0,0,0).

%%%%%%%%%%%%%
committed_hours(nov,02,jfran, 0,0,0).
committed_hours(dec,02,jfran, 0,0,41).
%
committed_hours(jan,03,jfran, 0,0,0).
committed_hours(feb,03,jfran, 0,0,0).
committed_hours(mar,03,jfran, 0,0,0).
committed_hours(apr,03,jfran, 0,0,0).
committed_hours(may,03,jfran, 0,143,0).
committed_hours(jun,03,jfran, 0,143,0).
committed_hours(jul,03,jfran, 0,143,50).
committed_hours(aug,03,jfran, 0,143,50).
committed_hours(sep,03,jfran, 0,143,0).
committed_hours(oct,03,jfran, 0,143,0).
committed_hours(nov,03,jfran, 0,143,0).
committed_hours(dec,03,jfran, 0,143,41).
%
committed_hours(jan,04,jfran, 0,143,0).
committed_hours(feb,04,jfran, 0,143,0).
committed_hours(mar,04,jfran, 0,143,0).
committed_hours(apr,04,jfran, 0,143,0).
committed_hours(may,04,jfran, 0,143,0).
committed_hours(jun,04,jfran, 0,143,0).
committed_hours(jul,04,jfran, 0,143,0).
committed_hours(aug,04,jfran, 0,143,0).
committed_hours(sep,04,jfran, 0,143,0).
committed_hours(oct,04,jfran, 0,143,0).
committed_hours(nov,04,jfran, 0,143,0).
committed_hours(dec,04,jfran, 0,143,0).
%
committed_hours(jan,05,jfran, 0,143,0).
committed_hours(feb,05,jfran, 0,143,0).
committed_hours(mar,05,jfran, 0,143,0).
committed_hours(apr,05,jfran, 0,143,0).
committed_hours(may,05,jfran, 0,143,0).
committed_hours(jun,05,jfran, 0,143,0).
committed_hours(jul,05,jfran, 0,143,0).
committed_hours(aug,05,jfran, 0,143,0).
committed_hours(sep,05,jfran, 0,143,0).
committed_hours(oct,05,jfran, 0,143,0).

%%%%%%%%%%%%%
committed_hours(jan,02,noone, 0,0,143).
committed_hours(feb,02,noone, 0,0,143).
committed_hours(mar,02,noone, 0,0,143).
committed_hours(apr,02,noone, 0,0,143).
committed_hours(may,02,noone, 0,0,143).
committed_hours(jun,02,noone, 0,0,143).
committed_hours(jul,02,noone, 0,0,143).
committed_hours(aug,02,noone, 0,0,143).
committed_hours(sep,02,noone, 0,0,143).
committed_hours(oct,02,noone, 0,0,143).
committed_hours(nov,02,noone, 0,0,143).
committed_hours(dec,02,noone, 0,0,143).
%
committed_hours(jan,03,noone, 0,0,143).
committed_hours(feb,03,noone, 0,0,143).
committed_hours(mar,03,noone, 0,0,143).
committed_hours(apr,03,noone, 0,0,143).
committed_hours(may,03,noone, 0,0,143).
committed_hours(jun,03,noone, 0,0,143).
committed_hours(jul,03,noone, 0,0,143).
committed_hours(aug,03,noone, 0,0,143).
committed_hours(sep,03,noone, 0,0,143).
committed_hours(oct,03,noone, 0,0,143).
committed_hours(nov,03,noone, 0,0,143).
committed_hours(dec,03,noone, 0,0,143).
%
committed_hours(jan,04,noone, 0,0,143).
committed_hours(feb,04,noone, 0,0,143).
committed_hours(mar,04,noone, 0,0,143).
committed_hours(apr,04,noone, 0,0,143).
committed_hours(may,04,noone, 0,0,143).
committed_hours(jun,04,noone, 0,0,143).
committed_hours(jul,04,noone, 0,0,143).
committed_hours(aug,04,noone, 0,0,143).
committed_hours(sep,04,noone, 0,0,143).
committed_hours(oct,04,noone, 0,0,143).
committed_hours(nov,04,noone, 0,0,143).
committed_hours(dec,04,noone, 0,0,143).
%
committed_hours(jan,05,noone, 0,0,143).
committed_hours(feb,05,noone, 0,0,143).
committed_hours(mar,05,noone, 0,0,143).
committed_hours(apr,05,noone, 0,0,143).
committed_hours(may,05,noone, 0,0,143).
committed_hours(jun,05,noone, 0,0,143).
committed_hours(jul,05,noone, 0,0,143).
committed_hours(aug,05,noone, 0,0,143).
committed_hours(sep,05,noone, 0,0,143).
committed_hours(oct,05,noone, 0,0,143).
%
committed_hours(_,_,susana, 9,1,41):-!. %suffices for the time being
committed_hours(_,_,jorge, 9,1,41):-!. %suffices for the time being
