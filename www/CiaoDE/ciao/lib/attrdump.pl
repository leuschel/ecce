:- module(attrdump, 
        [
            copy_extract_attr/3, 
            cp_attr/3,
            reinstall_attributes/1
        ], [dcg,assertions]).

:- use_module(library(dict), [dic_lookup/4,dictionary/5]).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Copy is a copy of Term with fresh, non-attributed variables, and
 %% AttrList is a list containing a representation of the attributes
 %% which where associated with each attributed variables in Term.
 %% The variables contained in AttrList are the same (in the sense of
 %% ==/2) as those in Copy. Thus, Copy plus OrdAttr convey the
 %% same information as Term, but expanded.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy_extract_attr(Term, Copy, AttrList) :-
        cp_attr(Term, Copy, Dict),
        attrlist(Dict, AttrList, []).

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
        functor(Term, N, A),
        functor(Copy, N, A),
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

attr(cva(Copy,Constr)) --> [attach_attribute(Copy, Constr)].
attr(v(_)            ) --> [].


reinstall_attributes([]).
reinstall_attributes([attach_attribute(Copy, Constr)|Rest]):-
        attach_attribute(Copy, Constr),
        reinstall_attributes(Rest).
