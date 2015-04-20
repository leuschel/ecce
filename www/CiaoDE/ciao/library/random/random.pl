:- module(random, [random/1, random/3, srandom/1], [assertions, isomodes]).

:- comment(title, "Random numbers").
:- comment(author, "Daniel Cabeza").

:- comment(module, "This module provides predicates for generating
        pseudo-random numbers").

:- impl_defined([random/1, random/3, srandom/1]).

:- comment(random(Number), "@var{Number} is a (pseudo-) random number in
        the range [0.0,1.0]").

:- true pred random(-float).

:- comment(random(Low, Up, Number), "@var{Number} is a (pseudo-) random
        number in the range [@var{Low}, @var{Up}]").

:- true pred random(+int,+int,-int)
        # "If @var{Low} and @var{Up} are integers, @var{Number} is an
          integer.".

:- true pred random(+flt,+num,-flt).
:- true pred random(+int,+flt,-flt).

:- comment(srandom(Seed), "Changes the sequence of pseudo-random
        numbers according to @var{Seed}.  The stating sequence of
        numbers generated can be duplicated by calling the predicate
        with @var{Seed} unbound (the sequence depends on the OS).").

:- true pred srandom(?int).
