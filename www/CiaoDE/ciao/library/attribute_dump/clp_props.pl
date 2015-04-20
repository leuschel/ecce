:- module(clp_props,[ clprq/1, def/1, dom/2, free/1, indep/1 ],
	  [assertions]).

:- use_module(library('assertions/doc_props')).
:- use_module(library('clpr/clpr_dump')).
:- use_module(library(lists),[intersection/3]).
:- use_module(library(terms_vars),[varset/2]).

:- multifile dump/3.
:- comment(dump/3,"The one from @tt{library('clpr/clp_dump')} is used.
    Be careful not to define one in your program!").

:- comment(title,"Properties of constrained variables").
:- comment(author,"F. Bueno").
:- comment(module,"This library provides several predicates to check
    properties of variables involved in constraints on CLP(R/Q).").
:- comment(bug,"Still under construction.").

:- true prop clprq(X)
      # "@var{X} is a constrained variable.".

clprq(X):- get_attribute(X,clpr_frozen(_Self,_System,_User)), !.
clprq(X):- get_attribute(X,float(_)), !.
clprq(X):- get_attribute(X,term_wrap(_Self,_)), !.
clprq(X):- get_attribute(X,eqn_var(_Self,_,_Lin,_Ref,_Nl)), !.
clprq(X):- get_attribute(X,rat(_,_)), !.

:- comment(def(X),"True iff @var{X} is constrained to a unique value.
     It subsumes @var{term_typing:ground/1}.").
:- true prop def(X)
      # "@var{X} has a definite value.".

def(X):- ground(X), !.
def(X):- get_attribute(X,float(_)), !.
def(X):- get_attribute(X,rat(_,_)).

:- true prop dom(X,D) :: clpvar * clpdom
      # "The domain of possible values of @var{X} is @var{D}.".

dom(X,D):-
    number(X), !,
    D=X.
dom(X,D):-
    dom_(X,L,D),
    conform_l(L,X),
    conform_h(D,L).

dom_(X,L,H):-
    var(X),
    dump(X,X,Cons),
    inspect(Cons,X,L,H).

inspect([],_,_,_).
inspect([C|Cons],T,L,H):-
    functor(C,F,2),
    arg(2,C,N),
    i(F,T,N,L,H),
    inspect(Cons,T,L,H).

i(.>.,T,N,(N<T),_).
i(.<.,_,N,X,X<N).
i(.>=.,T,N,(N=<T),_).
i(.=<.,_,N,X,X=<N).
i(.=.,_,N,L,H):- number(N), !, L=N, H=N.

conform_l(_<T,T):- !.
conform_l(_,_).

conform_h(T<_,T):- !.
conform_h(_,_).

:- comment(free(X),"True iff @var{X}, although constrained, might still
     take any value. It subsumes @var{term_typing:var/1}.").
:- true prop free(X)
      # "The domain of values of @var{X} is free.".

free(X):-
    dom_(X,L,H),
    var(L),
    var(H).

:- comment(indep(Vs),"The constrained variables in @var{Vs} are all
     independent, i.e., further constraining any number of them will
     not further constrain any of the others. Note, however, that 
     @var{Vs} might not be independent even though some subsets of it
     are. For example, with @tt{X.=.Y+Z}, it is true that 
     @tt{indep([X,Y])} and @tt{indep([X,Z])} and @tt{indep([Y,Z])} but
     it is false that @tt{indep([X,Y,Z])}.").
:- true prop indep(Vs) : list(var)
      + doc_incomplete
      # "All variables in @var{Vs} are independent.".

indep(Vs):-
	dump(Vs,Vs,Cons),
	varset(Cons,Vars),
	intersection(Vs,Vars,[]).

:- comment(doinclude,clpvar/1).
:- comment(clpvar/1,"Defined by: @includedef{clpvar/1}").
:- true prop clpvar(X) # "@var{X} is a CLP(R/Q) variable.".

clpvar(X):- number(X).
clpvar(X):- clprq(X).

:- comment(doinclude,clpdom/1).
:- comment(clpdom/1,"Defined by: @includedef{clpdom/1}").
:- true prop clpdom(X)
      # "@var{X} represents the domain of a CLP(R/Q) variable.".

clpdom((M<X)<N):- bound(M), var(X), bound(N).
clpdom((M<X)=<N):- bound(M), var(X), bound(N).
clpdom((M=<X)<N):- bound(M), var(X), bound(N).
clpdom((M=<X)=<N):- bound(M), var(X), bound(N).
clpdom(N):- number(N).

:- comment(doinclude,bound/1).
:- comment(bound/1,"Defined by: @includedef{bound/1}").
:- true prop bound(X) 
      # "@var{X} is the upper or lower bound of the domain of a 
         CLP(R/Q) variable.".

bound(X):- number(X).
bound(X):- var(X).
