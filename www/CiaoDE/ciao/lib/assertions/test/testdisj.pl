
:- use_package([assertions]).
:- use_module(library(write)).

doit :- 
	norm_body(X),
	write_canonical(X), nl,
	fail.

norm_body((PD::DP:CP=>AP+GP;CO)).
norm_body((PD::DP:CP=>AP   ;CO)).
norm_body((PD::DP:CP    +GP;CO)).
norm_body((PD::DP:CP       ;CO)).
norm_body((PD::DP   =>AP+GP;CO)).
norm_body((PD::DP   =>AP   ;CO)).
norm_body((PD::DP       +GP;CO)).
norm_body((PD::DP          ;CO)).
norm_body((PD    :CP=>AP+GP;CO)).
norm_body((PD    :CP=>AP   ;CO)).
norm_body((PD    :CP    +GP;CO)).
norm_body((PD    :CP       ;CO)).
norm_body((PD       =>AP+GP;CO)).
norm_body((PD       =>AP   ;CO)).
norm_body((PD           +GP;CO)).
norm_body((PD              ;CO)).
