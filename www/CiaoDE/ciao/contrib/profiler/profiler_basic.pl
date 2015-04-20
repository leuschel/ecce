:- module(profiler_basic,[],[]).

% :- multifile cost_center/2.
% :- data cost_center/2.


% cost_center_item(CostCenter,Item) :-
% 	cost_center_item_(0,CostCenter,Item).

% cost_center_item_(Index, cost_center(PredName,Arity),Item) :-
% 	cost_center_c_item(Index,PredName,Arity,Item)
%  ;
% 	Index1 is Index+1,
% 	cost_center_item(Index1,cost_center(PredName,Arity),Item).

% :- true pred cost_center_c_item(in(Index), in(PredName), in(Arity),
%    go(Calls), go(Skips), go(NSkips), go(Cuts), go(SCuts), go(Retrys),
%    go(TimeSpent)) :: int * atom * int * int * int * int * int * int *
%    int * number + (foreign).

:- foreign_inline(profile_flat_open/1,
"
int profile_flat_open(int prev_index) {
}
").
