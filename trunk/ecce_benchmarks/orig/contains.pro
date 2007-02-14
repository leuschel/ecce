/* file: contains.pro  */

contains( _pat, _str ) :-
    con( _str, pi([],_pat) ).

/* con(X,Y) :- print(string(X)),print(pattern(Y)),nl,fail. */
con( _, pi(_,[]) ).
con( [_t|_rem_str], _pat_info ) :-
    new( _t, _pat_info, _new_pat_info ),
    con( _rem_str, _new_pat_info ).

new( _t, pi(_prefix,[_t|_rem_postfix]), pi(_new_prefix,_rem_postfix) ) :-
    append( _prefix, [_t], _new_prefix ).
new( _t, pi(_prefix,[_dift|_rem_postfix]), pi(_new_prefix,_new_postfix) ) :-
    _t \= _dift,
    append( _prefix, [_t], _temp ),
    append( _new_prefix, _rest, _prefix ),
    append( _, _new_prefix, _temp ),
    append( _rest, [_dift|_rem_postfix], _new_postfix ).

append( [], _L, _L ).
append( [_X|_Xs], _Ys, [_X|_Zs] ) :-
    append( _Xs, _Ys, _Zs ).


