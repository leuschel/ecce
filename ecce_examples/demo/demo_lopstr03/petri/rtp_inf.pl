/* added one reset_counter to RTP

Example : rtp
          RTP (Receiver part of Telecommunication Protocol) 

vars
  X1 X2 X3 X4 X5 X6 X7 X8 X9 
rules
  X1>=1        ->  X1'=X1-1,X2'=X2+1;
  X2>=1        ->  X2'=X2-1,X3'=X3+1;
  X3>=1        ->  X3'=X3-1,X4'=X4+1;
  X4>=1        ->  X4'=X4-1,X5'=X5+1;
  x4>=1        ->  X4'=X4-1,X9'=X9+1;
  X5>=1        ->  X5'=X5-1,X6'=X6+1;
  X6>=1        ->  X6'=X6-1,X9'=X9+1;
  X6>=1        ->  X6'=X6-1,X7'=X7+1;
  X6>=1        ->  X6'=X6-1,X8'=X8+1;
  X7>=1        ->  X7'=X7-1,X9'=X9+1;
  X8>=1        ->  X8'=X8-1,X9'=X9+1;
  X9>=1        ->  X9'=X9-1,X2'=X2+1;
init
  X1=1,X2=0,X3=0,X4=0,X5=0,X6=0,X7=0,X8=0,X9=0
target
  X7>=1,X8>=1

target in prolog : sat([X1,X2,X3,X4,X5,X6,s(X7),s(X8),X9])

*/

trans(start_net,[s(X1),X2,X3,X4,X5,X6,X7,X8,X9,X10],[X1,s(X2),X3,X4,X5,X6,X7,X8,X9,X10]).
trans(alert,[X1,s(X2),X3,X4,X5,X6,X7,X8,X9,X10],[X1,X2,s(X3),X4,X5,X6,X7,X8,X9,X10]).
trans(do,[X1,X2,s(X3),X4,X5,X6,X7,X8,X9,X10],[X1,X2,X3,s(X4),X5,X6,X7,X8,X9,X10]).
trans(off_hook,[X1,X2,X3,s(X4),X5,X6,X7,X8,X9,X10],[X1,X2,X3,X4,s(X5),X6,X7,X8,X9,X10]).
trans(no_signal,[X1,X2,X3,s(X4),X5,X6,X7,X8,X9,X10],[X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10]).
trans(do2,[X1,X2,X3,X4,s(X5),X6,X7,X8,X9,X10],[X1,X2,X3,X4,X5,s(X6),X7,X8,X9,X10]).
trans(on_hook,[X1,X2,X3,X4,X5,s(X6),X7,X8,X9,X10],[X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10]).
trans(abort,[X1,X2,X3,X4,X5,s(X6),X7,X8,X9,X10],[X1,X2,X3,X4,X5,X6,s(X7),X8,X9,X10]).
trans(disconnect_tone,[X1,X2,X3,X4,X5,s(X6),X7,X8,X9,X10],[X1,X2,X3,X4,X5,X6,X7,s(X8),X9,X10]).
trans(on_hook2,[X1,X2,X3,X4,X5,X6,s(X7),X8,X9,X10],[X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10]).
trans(on_hook3,[X1,X2,X3,X4,X5,X6,X7,s(X8),X9,X10],[X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10]).
trans(repeat,[X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10],[X1,s(X2),X3,X4,X5,X6,X7,X8,X9,s(X10)]).


start([s(0),0,0,0,0,0,0,0,0,0]).
start(X,[X,0,0,0,0,0,0,0,0,0]).

prop([X1,X2,X3,X4,X5,X6,s(X7),s(X8),X9,X10],unsafe).

/* model checking tasks with K process */
k_processes_model_check(SK,Formula) :- start(s(0),S),
     Formula = sat(S,ef(p(unsafe))).