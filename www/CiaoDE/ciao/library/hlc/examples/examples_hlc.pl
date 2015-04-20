:- module(examples_hlc, [main/0], []).

:- include(library(hlc)).

:- use_module(library(system)).
:- use_module(library(format)).

main:-
        hlc1, 
        pause(2),
        hlc2,
        pause(2),
        hlc3.

%% Data flow: <&&/1 indicates when the data is needed

hlc1:-
        format("X = ~w, Y = ~w~n", [X, Y]),
        p(X) &&> H1,
        q(Y) &&> H2,
        format("X = ~w, Y = ~w~n", [X, Y]),
        H1 <&& ,
        format("X = ~w, Y = ~w~n", [X, Y]),
        H2 <&& ,
        format("X = ~w, Y = ~w~n", [X, Y]).

p(1).
q(a).




%% The first and second calls start at the same time, because we generate
%% two threads for them.  Calls 3 and 4 start after them.

hlc2:-
        do_something(1) &&> H1,
        do_something(2) &&> H2,
        do_something(3) &> H3,
        do_something(4) &> H4,
        H1 <&&, H2 <&&, H3 <&&, H4 <&& .


%% Now, we ask for "gas" for all the tasks; all of them start at the same time

hlc3:-
        do_something(1) &&> H1,
        do_something(2) &&> H2,
        do_something(3) &&> H3,
        do_something(4) &&> H4,
        H1 <&&, H2 <&&, H3 <&&, H4 <&& .


hlc4:- hlc2.

do_something(Call):-
        format("Doing something: call number ~w~n", [Call]),
        pause(3),
        format("Finished doing ~w~n", [Call]).
