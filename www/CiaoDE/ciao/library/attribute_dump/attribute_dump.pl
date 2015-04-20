:- module(attribute_dump, 
        [   copy_extract_attr/3, cp_attr/3,
	    reify_attribute/2, reify_attribute/1,
	    smash_attribute/1, smash_attribute/2,
	    attribute/1, functor/3, attr_vars/2,
            reinstall_attributes/1
        ], [dcg,assertions]).

:- use_module(library(dict), [
        dic_lookup/4,
        dic_lookup/3,
        dictionary/5,
        dictionary/1
                             ]).

:- comment(title,"Reification of attributed variables").
:- comment(module,"This library has several predicates for the 
    @index{reification} of the attributes of a term, i.e., for
    converting them in terms themselves, so that they can be 
    manipulated as usual terms.").
:- comment(bug,"The reification predicates don't work with cyclic 
    (rational) input terms.").
:- comment(bug,"Also, they may cause for example @tt{clp_dump:dump/3}
    to fail. Reason unknown.").

:- comment(copy_extract_attr(X,T,L),"Returns in @var{L} the dictionary
    of terms which represent an attribute of a variable of @var{X},
    for each such variable plus the attributed variables appearing in 
    the attributes themselves. @var{T} is a copy of @var{X} with fresh, 
    non-attributed variables, and the attributes in @var{L} are
    related to each variable in @var{X} by referring to the
    corresponding variable of @var{T} in the same position. E.g.:
@begin{verbatim}
copy_extract_attr(a(Y),a(B),[B=...])
@end{verbatim}").
:- true pred copy_extract_attr(X,T,L) => list(L,var=term).

copy_extract_attr(Term, Copy, AttrList) :-
        cp_attr(Term, Copy, Dict),
        attrlist(Dict, AttrList, []).

:- true pred cp_attr(X,T,D) => dictionary(D)
      # "Same as @tt{copy_extract_attr(X,T,L)} except for the last
         argument.".

cp_attr(Term, Copy, Dict) :-
        type(Term, Type),
        cp_attr_t(Type, Term, Copy, Dict).

cp_attr_t(attv,      Cva,    Copy,     Dict) :-
        dic_lookup(Dict, Cva, cva(Copy,Attcopy), Stat),
        ( Stat = new ->
            get_attribute(Cva, Attrib),
            cp_attr(Attrib, Attcopy, Dict)
        ; true
        ).
cp_attr_t(var,       V,      Copy,     Dict) :-
        dic_lookup(Dict, V, v(Copy), _).
cp_attr_t(integer,   I,      I,        _).
cp_attr_t(float,     F,      F,        _).
cp_attr_t(atom,      A,      A,        _).
cp_attr_t(list,      [X|Xt], [Xc|Xct], Dict) :-
        cp_attr(X, Xc, Dict),
        cp_attr(Xt, Xct, Dict).
cp_attr_t(structure, Term,   Copy,     Dict) :-
        term_basic:functor(Term, N, A),
        term_basic:functor(Copy, N, A),
        cp_attr_args(A, Term, Copy, Dict).

cp_attr_args(0, _,    _,    _   ) :- !.
cp_attr_args(N, Term, Copy, Dict) :-
        N1 is N-1,
        arg(N, Term, At),
        arg(N, Copy, Ac),
        cp_attr(At, Ac, Dict),
        cp_attr_args(N1, Term, Copy, Dict).

attrlist(Dict) --> {var(Dict)}, !, [].
attrlist(Dict) --> {dictionary(Dict,_,Val,L,R)},
        attrlist(L),
        attr(Val),
        attrlist(R).

attr(cva(Copy,Constr)) --> [Copy = Constr].
attr(v(_)            ) --> [].

:- true pred reinstall_attributes(L) : list(var=term)
      # "Attaches to each variable the corresponding attribute as
         by @var{L}.".

reinstall_attributes([]).
reinstall_attributes([Copy = Constr|Rest]):-
        attach_attribute(Copy, Constr),
        reinstall_attributes(Rest).

:- true pred reify_attribute(X,T)
      # "Term @var{T} is like term @var{X}, but the original variables
         of @var{X} which had an attribute are replaced in @var{T} by 
         the term that represents that attribute. The same for variables
         appearing in the attributes themselves. Term @var{T} can be 
         built from @tt{copy_extract_attr(X,T,L)} by applying the 
         equations of @var{L} as if it was a substitution.".

reify_attribute(X,T):- reify_attribute0_(X,n,T).

reify_attribute0_(X,F,T):- reify0(X,F,[],_,T).

reify0(X,_,V0,V1,T):-
    dic_lookup(X,V0,V), !,
    V1=V0, T=V.
reify0(X,S,V0,V1,T):-
    get_attribute(X,A), !,
    term_basic:functor(A,F,N),
    smashing(S,at,F,N,T),
    reify0_(N,S,[(X,T)|V0],V1,A,T).
reify0(X,_,V0,V1,T):-
    var(X), !,
    V1=V0, T=X.
reify0(X,S,V0,V1,T):-
    term_basic:functor(X,F,N),
    smashing(S,fn,F,N,T),
    reify0_(N,S,V0,V1,X,T).

reify0_(0,_,V0,V1,_,_):- !,
    V1=V0.
reify0_(N,S,V0,V2,X,T):-
    N > 0, 
    arg(N,X,ArgX),
    arg(N,T,ArgT),
    reify0(ArgX,S,V0,V1,ArgT),
    N1 is N-1,
    reify0_(N1,S,V1,V2,X,T).

smashing(y,U,F,N,T):-
    smash(U,F,FT),
    term_basic:functor(T,FT,N).
smashing(n,_,F,N,T):-
    term_basic:functor(T,F,N).

:- true pred smash_attribute(X,T)
      # "Like @tt{reify_attribute(X,T)} but the terms representing
         attributes have an additional @tt{'$'} in front of their 
         functor name.".

smash_attribute(X,T):- reify_attribute0_(X,y,T).

:- true pred reify_attribute(X)
      # "Term @var{X} is replaced by the term that corresponds to 
         its attributes. It loses the attributes. It is equivalent to
         unifying @var{X} with @var{T} where @tt{reify_attribute(X,T)},
         after dropping the original attributes of the variables in
         term @var{X}.".

reify_attribute(X):- reify_attribute_(X,n).

reify_attribute_(X,F):-
    reify_attribute0_(X,F,T),
    detach_all_attributes(X),
    X=T.

detach_all_attributes(X):-
    get_attribute(X,_), !,
    detach_attribute(X).
detach_all_attributes(X):-
    var(X), !.
detach_all_attributes(X):-
    term_basic:functor(X,_,N),
    detach_all(N,X).

detach_all(0,_):- !.
detach_all(N,X):-
    N > 0,
    arg(N,X,Arg),
    detach_all_attributes(Arg),
    N1 is N-1,
    detach_all(N1,X).

:- true pred smash_attribute(X)
      # "Like @tt{reify_attribute(X)} but the terms representing
         attributes have an additional @tt{'$'} in front of their 
         functor name. This is useful for distinguishing subterms of
         @var{X} which correspond to attributes from those which were
         already subterms of @var{X}, not attributes (see @tt{attr/1}).".

smash_attribute(X):- reify_attribute_(X,y).

:- true pred functor(T,F,N)
      # "Like @tt{term_basic:functor(T,F,N)}, but it works with the
         original functor name of @var{T} instead of the smashed name.
         If @var{T} does not come from a reification that smashes functor
         names, the result may be incoherent.".

functor(X,F,N):-
    nonvar(X), !,
    term_basic:functor(X,F0,N),
    unsmash(F0,F,_).
functor(X,F,N):-
    term_basic:functor(X,F,N).

:- true pred attribute(X)
      # "True iff the functor name of the term @var{X} corresponds to
         the name of an attribute. It works only if @var{X} comes from
         a reification that smashes functor names, i.e., adds @tt{'$'}
         in front. Otherwise the result may be incoherent.".

attribute(T):-
    nonvar(T),
    term_basic:functor(T,F,_),
    unsmash(F,_,at).

unsmash(X,Y,U):-
    atom_concat('$$',X1,X), !,
    unsmash(X1,Y1,U),
    atom_concat('$',Y1,Y).
unsmash(X,Y,U):-
    atom_concat('$',X1,X), !,
    Y=X1, U=at.
unsmash(X,X,fn).

smash(U,X,Y):-
    smash_fn(X,Y0),
    prefix(U,Y0,Y).

prefix(at,X,Y):-
    atom_concat('$',X,Y).
prefix(fn,X,X).

smash_fn(X,Y):-
    atom_concat('$',X1,X), !,
    smash_fn(X1,Y1),
    atom_concat('$$',Y1,Y).
smash_fn(X,X).

:- true pred attr_vars(X,Vs) => list(Vs,var)
      # "@var{Vs} is the list of the variables related to the attributes
         of @var{X}. This includes all attributed variables within the term
         @var{X} and all attributed variables in such attributes, and in
         the attributes of these, and so on.".

attr_vars(X,Vs):- collect(X,[],Vs).

memberchk(X,[Y|_]):- X==Y, !.
memberchk(X,[_|L]):- memberchk(X,L).

collect(X,V0,V1):-
    memberchk(X,V0), !,
    V1=V0.
collect(X,V0,V1):-
    get_attribute(X,A), !,
    collect(A,[X|V0],V1).
collect(X,V0,V1):-
    var(X), !,
    V1=V0.
collect(X,V0,V1):-
    term_basic:functor(X,_,N),
    collect_(N,X,V0,V1).

collect_(0,_,V0,V1):- !,
    V1=V0.
collect_(N,X,V0,V2):-
    N > 0, 
    arg(N,X,ArgX),
    collect(ArgX,V0,V1),
    N1 is N-1,
    collect_(N1,X,V1,V2).

:- comment(appendix,"When a term resulting from the reification of 
        attributes is printed, you may find that the printing hangs
        at some point. This can be due to the fact that some printing 
        predicates call @tt{multifile:portray_attribute/2} (see
        @ref{Term output}); this predicate finds a term that should
        correspond to an attribute and tries to print it, but, if the
        term comes from a reification, it is not really an attribute, and
        thus the call @var{multifile:portray_attribute/2} might get
        confused, possibly causing an infinite loop.

        In such a case, it is better to print the term @tt{T} that comes
        from a reification with @tt{write_term(T,[portrayed(false)])}
        to avoid calling @tt{multifile:portray_attribute/2}.").
