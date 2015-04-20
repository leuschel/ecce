
 %% Misc functions -- MCL

substring(String1, Position, Length, String2):-
        skip_string_to(Position, String1, Str1skipped),
        skip_and_build(Length, Str1skipped, String2).

skip_and_build(N, Str, Prefix):-
        number(N) ->
        build_prefix(N, Str, Prefix)
 ;
        build_prefix_len(N, Str, Prefix).

skip_string_to(N, Str, StrSkipped):-
        number(N) ->
        skip_string_to_number(N, Str, StrSkipped)
 ;
        skip_number_to_string(N, Str, StrSkipped).

skip_string_to_number(0, S, S).
skip_string_to_number(N, [_|S], Res):-
        N > 0, N1 is N - 1,
        skip_string_to(N1, S, Res).

skip_number_to_string(0, S, S).
skip_number_to_string(N, [_|S], Res):-
        skip_number_to_string(N1, S, Res),
        N is N1 + 1.

build_prefix(0, _Skipped, []).
build_prefix(N, [S|Ss], [S|Rest]):-
        N > 0, N1 is N - 1,
        build_prefix(N1, Ss, Rest).

build_prefix_len(0, _Skipped, []).
build_prefix_len(N, [S|Ss], [S|Rest]):-
        build_prefix_len(N1, Ss, Rest),
        N is N1 + 1.


compound(X):-
        functor(X, _Name, Arity),
        Arity > 0.
