:- module(atomic_basic, [
        name/2, atom_codes/2, number_codes/2, number_codes/3, atom_number/2,
        atom_length/2, atom_concat/3, sub_atom/4],
        [assertions, isomodes]).

%:- use_module(engine(internals), ['$prolog_radix'/2]).

:- comment(title, "Basic predicates handling names of constants").

:- comment(author, "The CLIP Group").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module, "The Ciao system provides builtin predicates which
   allow dealing with names of constants (atoms or numbers).  Note that
   sometimes strings (character code lists) are more suitable to handle
   sequences of characters.").

% Defined in C

:- impl_defined([
        name/2, atom_codes/2, number_codes/2, number_codes/3,
        atom_length/2, atom_concat/3, sub_atom/4]).

:- comment(name(Const,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Const}.  Note
   that if @var{Const} is an atom whose name can be interpreted as a
   number (e.g. '96'), the predicate is not reversible, as that atom
   will not be constructed when @var{Const} is uninstantiated.  Thus it
   is recommended that new programs use the ISO-compliant predicates
   @pred{atom_codes/2} or @pred{number_codes/2}, as these predicates do
   not have this inconsistency.").

:- true pred name(+constant,?string) + eval.
:- true pred name(-constant,+string) + eval
   # "If @var{String} can be interpreted as a number, @var{Const} is unified
      with that number, otherwise with the atom whose name is @var{String}.".
:- true comp name/2 + ( sideff(free), native ).

:- comment(atom_codes(Atom,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Atom}.").

:- true pred atom_codes(+atm,?string) + eval.
:- true pred atom_codes(-atm,+string) + eval.
:- true comp atom_codes/2 + ( sideff(free), native, iso ).

:- comment(number_codes(Number,String), "@var{String} is the list of the
   ASCII codes of the characters comprising a representation of
   @var{Number}.").

:- true pred number_codes(+num,?string) + eval.
:- true pred number_codes(-num,+string) + eval.
:- true comp number_codes/2 + ( sideff(free), native, iso ).

:- trust success number_codes(A,B) => (num(A), list(B,num_code)).

:- comment(number_codes(Number,Base,String), "@var{String} is the list
   of the ASCII codes of the characters comprising a representation of
   @var{Number} in base @var{Base}.").

:- true pred number_codes(+num,+int,?string) + eval.
:- true pred number_codes(-num,+int,+string) + eval.
:- true comp number_codes/3 + ( sideff(free), native ).

:- comment(atom_number(Atom,Number), "@var{Atom} can be read as a
   representation of @var{Number}.").

:- true pred atom_number(+atm,?num) + eval.
:- true pred atom_number(-atm,+num) + eval.
:- true comp atom_number/2 + ( sideff(free), native ).

atom_number(A, N) :-
        atom(A), number(N), !,
        atom_codes(A, S),
        number_codes(N0, S),
        N = N0.               % So that atom_number('2.3e1',23.0) succeeds
atom_number(A, N) :-
        atom(A), var(N), !,
        atom_codes(A, S),
        number_codes(N, S).
atom_number(A, N) :-
        var(A), number(N), !,
        number_codes(N, S),
        atom_codes(A, S).
atom_number(A, N) :-
        ( var(A) ->
          ( var(N) ->
            throw(error(instantiation_error, atom_number/2-1))
          ; throw(error(type_error(number, N), atom_number/2-2))
          )
        ; atom(A) ->
            throw(error(type_error(number, N), atom_number/2-2))
        ; throw(error(type_error(atom, A), atom_number/2-1))
        ).

:- comment(atom_length(Atom,Length), "@var{Length} is the number of
   characters forming the name of @var{Atom}.").

:- true pred atom_length(+atm,?int) + eval.
:- true comp atom_length/2 + ( sideff(free), native, iso ).

:- comment(atom_concat(Atom_1,Atom_2,Atom_12), "@var{Atom_12} is the
   result of concatenating @var{Atom_1} followed by @var{Atom_2}.").

:- true pred atom_concat(+atom,+atom,?atom) + eval
   # "Concatenate two atoms.".
:- true pred atom_concat(-atom,-atom,+atom) + eval
   # "Non-deterministically split an atom.".
:- true pred atom_concat(-atom,+atom,+atom) + eval
   # "Take out of an atom a certain suffix (or fail if it cannot be done).".
:- true pred atom_concat(+atom,-atom,+atom) + eval
   # "Take out of an atom a certain prefix (or fail if it cannot be done).".
:- true comp atom_concat/3 + ( sideff(free), native, iso ).

:- comment(sub_atom(Atom,Before,Length,Sub_atom), "@var{Sub_atom} is
   formed with @var{Length} consecutive characters of @var{Atom}
   after the @var{Before} character.  For example, the goal
   @tt{sub_atom(summer,1,4,umme)} succeeds.").

:- true pred sub_atom(+atm,+int,+int,?atm) + eval.
:- true comp sub_atom/4 + ( sideff(free), native ).
