:- module(workplan, [task/6], [assertions]).

:- comment(module, "This modules contains the basic information
   regarding the taks of @var{Project} as described in the
   contract. Though in principle only one fact of @pred{task/6} is
   needed per task (a pair of @var{WP} and @var{ID}), more than one
   may be introduced if more sophisticated distribution of workload is
   desired. For example, if we are not happy about even distribution
   of load during the lifetime of a task we may split a task into two
   or more subtasks with their corresponding load and start and end
   times.

   It is important to note that the start and end of tasks is given at
   the level of months. The first month of the project is month 0. A
   task which is active during all the life-time of a 3-year project
   has 0 and 36 (not 35!) as start and end dates. This should be
   interpreted as 36 is the first month in which the work in the task
   has finished.").

:- comment( task(Project,WP,ID,Start,End,Name), "@var{WP} is the
     workpackage number within @var{Project}. @var{ID} is the task
     identifier within the @var{WP}. @var{Start} is the starting month
     of the task.  @var{End} is the month in which the work in the
     task is suppossed to finish. @var{Name} is the title of the
     task.").

% :- trust pred task(A,B,C,D,E,F) => ground([A,B,C,D,E,F]).

:- check pred task(A,B,C,D,E,F) => ground([A,B,C,D,E,F]).

%WP1 manag
task(asap,1,1,0,36,"Internal Management").
task(asap,1,2,0,36,"Coordination Management").

%WP2 req
task(asap,2,1,0, 6,"Requirements").
task(asap,2,2,3,12,"Case Studies Definition").

%WP3 basic
task(asap,3,1,0,6,"Analysis Domains for Specialization").
task(asap,3,2,0,6,"Off-line Specialization: precision and Efficiency").

%WP4 resou
task(asap,4,1, 6,15,"Reduzing the Size of Programs"). 
%task(asap,4,1, 0,6,"Reduzing the Size of Programs").
task(asap,4,2, 6,15,"Studying Platform-dependent Issues").
task(asap,4,3, 9,18,"Self-tuning Specialization System").
task(asap,4,4,15,24,"Cost Analysis for CLP").
task(asap,4,5,15,24,"Specialization at Abstract Machine Level").

%WP5 seman
task(asap,5,1,18,24,"Analysis of Process Languages").
task(asap,5,2,24,30,"Analysis and Specialization of Low Level Abstract Machines").
task(asap,5,3,21,27,"Analysis of High Level Specification Languages").

%WP6 valid
task(asap,6,1,12,18,"Safety Conditions in Pervasive Computing").
%task(asap,6,1,6,12,"Safety Conditions in Pervasive Computing").
task(asap,6,2,18,24,"Combined Static and Dynamic Checking").
task(asap,6,3,24,30,"Run-time Checking in Pervasive Computing").

%WP7 tool
task(asap,7,1, 6, 9,"Specialization of Real CLP Languages").
task(asap,7,2, 6,12,"Implementation of First Prototype").
task(asap,7,3,24,32,"Final Integrated Tool").

%WP8 assess
task(asap,8,1,12,21,"Case Studies: First Cycle").
task(asap,8,2,29,35,"Case Studies: Second Cycle").
task(asap,8,3,32,35,"Evaluation").
task(asap,8,4,34,36,"Proposed Development Method").

%WP9 dissem
task(asap,9,1,12,15,"Initial Web Site").
task(asap,9,2,12,36,"Dissemination of Results").
task(asap,9,3,33,36,"Final Project Web Site").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task(colognet,1,german,0,12,"German").
task(colognet,1,herme,8,12,"Herme").
task(colognet,1,boris,0,12,"Boris").
task(colognet,1,bardo,0,12,"Bardo").
task(colognet,1,bueno,0,12,"Bueno").
task(colognet,1,claudio,0,12,"Claudio").

task(colognet,2,german,12,24,"German").
task(colognet,2,herme,12,24,"Herme").
task(colognet,2,boris,12,24,"Boris").
task(colognet,2,bueno,12,24,"Bueno").
task(colognet,2,bardo,12,18,"Bardo").
task(colognet,2,bardo2,18,24,"Bardo").

task(colognet,3,1,24,36,"noone").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task(amos,1,herme,6,10,"Herme").
task(amos,1,german,0,10,"German").
task(amos,1,boris,0,10,"Boris").
task(amos,1,jesus,0,10,"Jesus").
%task(amos,1,herme2,10,12,"Herme"). % no le hemos asignado horas en 2003
task(amos,1,german2,10,12,"German").
task(amos,1,boris2,10,12,"Boris").
task(amos,1,jesus2,10,11,"Jesus").  % en febrero no disponible

task(amos,2,herme,12,22,"Herme").
task(amos,2,german,12,22,"German").
task(amos,2,boris,12,22,"Boris").
task(amos,2,bardo,12,22,"Bardo").
task(amos,2,jmanuel,12,22,"JManuel").
task(amos,2,herme2,22,24,"Herme ").
task(amos,2,german2,22,24,"German").
task(amos,2,boris2,22,24,"Boris").
task(amos,2,bardo2,22,24,"Bardo").
task(amos,2,jmanuel2,22,24,"JManuel").
