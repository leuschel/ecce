
foo(A,B,C,D) :- 
        (   integer(D),
            ground(C),
            nonvar(A) ->
            hash_term(C,E),
            functor(A,F,G),
            hash_term(F,H),
            I is\(D,\(E,H)),
            '** foo/4 index 1 **'(I,J)
        ;   integer(D) ->
            '** foo/4 index 2 **'(D,J)
        ;   true
        ),
        '** foo/4 **'(J,A,B,C,D).

'** foo/4 **'(0,a,b,c(d),9).
'** foo/4 **'(1,e,f,g(h),11) :- 
        foo(a,b,c(d),9).
'** foo/4 **'(2,A,z,B,C) :- 
        baz.

'** foo/4 index 1 **'(165,0).
'** foo/4 index 1 **'(179,1).
'** foo/4 index 1 **'(A,2).

'** foo/4 index 2 **'(9,0).
'** foo/4 index 2 **'(11,1).
'** foo/4 index 2 **'(A,2).

baz.

end_of_file.
