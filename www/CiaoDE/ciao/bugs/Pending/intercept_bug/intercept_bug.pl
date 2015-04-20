% intercept_bug.pl - bug in exceptions:intercept/3
%
% test/0 does a segmentation violation after 14000 iterations (WRONG).
% test_2/0 loops forever (OK).
%
% The reason is that intercept(Goal, Error, Handler) leaves choice points
% even if Goal is determinist.
% 
% Solution: copy the solution that was implemented for catch/3 (which
% does a cut when Goal execution is deterministic).

:- module(_, _, []).

test :-
	intercept(true, _E, true),
	'$metachoice'(A),
	display(c(A)), nl,
	test.
test_2 :-
	catch(true, _E, true),
	'$metachoice'(A),
	display(c(A)), nl,
