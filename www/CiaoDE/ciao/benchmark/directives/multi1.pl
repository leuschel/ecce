
:- multifile q/0.

main(_) :- q, fail.

q :- display(1), nl.
