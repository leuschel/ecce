:- module(clp_reify,[ reify_clp_var/2 ],[assertions]).

:- use_module(library('clpr/clpr_dump')).
:- use_module(library(idlists),[memberchk/2]).
:- use_module(library(terms_vars),[varset/2]).
%:- use_module(reify_attr,[attr_vars/2]).

:- multifile dump/3.
:- comment(dump/3,"The one from @tt{library('clpr/clp_dump')} is used.
    Be careful not to define one in your program!").

:- comment(title,"Reification of constraints").
:- comment(author,"F. Bueno").
:- comment(module,"This library provides a predicate that reifies
    the constraints related to some constrained variable on CLP(R/Q).").

:- true pred reify_clp_var(X,Cons) : var(X) => list(Cons,constraint)
      # "@var{Cons} are the constraints imposed on @var{X} or on any
         other variable related to @var{X} by a constraint.".

reify_clp_var(X,Cons):-
    var(X),
%    attr_vars(X,Vs),    % dump/3 doesn't like this!! (fails)
    collect([X],[],Vs),  % it also fails with this one sometimes!
    dump([X|Vs],[X|_],Cons).

collect(X,Vs0,Vs):-
    var(X), !,
    Vs=Vs0.
collect([],Vs,Vs).
collect([X|Xs],Vs0,Vs):-
    collect_one(X,Vs0,Vs1),
    collect(Xs,Vs1,Vs).

collect_one(X,Vs0,Vs):-
    memberchk(X,Vs0), !,
    Vs=Vs0.
collect_one(X,Vs0,Vs):-
    get_attribute(X,At), !,
    collect_at(At,[X|Vs0],Vs).
collect_one(X,Vs,[X|Vs]).

collect_at(eqn_var(_Self,_,_Lin,Ref,Nl),Vs0,Vs):-
    get_attribute(Ref,At),
    collect_ref(At,Vs0,Vs1),
    ( get_attribute(Nl,Nt)
    -> varset(Nt,N),
       collect(N,Vs1,Vs)
     ; Vs=Vs1 ).
collect_at(clpr_frozen(_Self,System,User),Vs0,Vs):-
    varset((System,User),Vars),
    collect(Vars,Vs0,Vs).
collect_at(term_wrap(_Self,T),Vs0,Vs):-
    varset(T,Vars),        % does T have attributes?
    collect(Vars,Vs0,Vs).
collect_at(float(_),Vs,Vs).
collect_at(rat(_,_),Vs,Vs).

collect_ref(Ref,Vs0,Vs):- 
    var(Ref), !,
    Vs=Vs0.
collect_ref(p(_,A,_,B,_),Vs0,Vs):-
    collect(A,Vs0,Vs1),
    collect(B,Vs1,Vs).
