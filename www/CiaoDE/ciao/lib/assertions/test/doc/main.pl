
:- module(main,[foo/3],[assertions,isomodes]).
:- use_module(native_props).

:- pred foo(X,Y,Z) : 
	( var(X)
	, gnd(Y)
	, ground(Z)
	, linear(Z)
	, indep(X,Y)
	, indep([[X,Y],[K,Z]])
	, mshare([[X],[X,Y,Z],[Y]])
        , fails(X)
        , not_fail(X)
        , possible_fail(X)
        , covered(X) 
        , not_covered(X) 
        , is_det(X)
        , possible_nondet(X)
        , disjoint(X)
        , not_disjoint(X)
        , lower_size(X,Y)
        , upper_size(X,Y)
        , lower_time(X,Y) 
        , upper_time(X,Y) 
	)

# "Nice pred.".

foo(_,_,_).
