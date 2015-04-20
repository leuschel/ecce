:- use_module(upd_arrays).

 %% :- data number/1.
 %% 
 %% number(0).

check_arrays1(Dims):-
        make_array(Dims, Array1, 0),
        make_array(Dims, Array2, 0),
        make_array(Dims, Array3, 0),
        fill_it(Dims, Dims, 1, Array1),
        fill_it(Dims, Dims, 1, Array2),
        display('---------------------------'), nl,
        print_array(Dims, Dims, Array1),
        display('---------------------------'), nl,
        print_array(Dims, Dims, Array2),
        display('---------------------------'), nl,
        print_array(Dims, Dims, Array3),
        sum_them(Dims, Dims, Array1, Array2, Array3),
        display('---------------------------'), nl,
        print_array(Dims, Dims, Array3).
        
check_arrays2(Dims):-
        make_array(Dims, Array1, 2),
        display('---------------------------'), nl,
        print_array(Dims, Dims, Array1),
        process(Dims, Dims, Array1),
        display('---------------------------'), nl,
        print_array(Dims, Dims, Array1).

:- use_package(hiord).

process(Now, _Original, A):- 
        all_one(Now),
        access_update_array_ho(A, Now, Old, New, (_(X,Y):- Y is X * 2)).
process(Now, Original, A):-
        access_update_array_ho(A, Now, _Old, _New, (_(X,Y):- Y is X * 2)),
        next_index(Now, Original, Now1),
        process(Now1, Original, A).


fill_it(Now, _Original, N, A):- 
        all_one(Now),
        update_array(A, Now, N).
fill_it(Now, Original, N, A):-
        update_array(A, Now, N),
        next_index(Now, Original, Now1),
        N1 is N + 1,
        fill_it(Now1, Original, N1, A).


sum_them(Now, _Original, A1, A2, A3):- 
        all_one(Now),
        access_array(A1, Now, E1),
        access_array(A2, Now, E2),
        E3 is E1 + E2,
        update_array(A3, Now, E3).
sum_them(Now, Original, A1, A2, A3):- 
        access_array(A1, Now, E1),
        access_array(A2, Now, E2),
        E3 is E1 + E2,
        update_array(A3, Now, E3),
        next_index(Now, Original, Now1),
        sum_them(Now1, Original, A1, A2, A3).


all_one([]).
all_one([1|Zs]):- all_one(Zs).

%next_index([], [], _).
next_index([1|Is], [Orig|Origs], [Orig|Rest]):-
        next_index(Is, Origs, Rest).
next_index([N|Is], _, [N1|Is]):-
        N > 1,
        N1 is N - 1.

print_array(Now, _Original, A):- 
        all_one(Now), !,
        access_array(A, Now, N),
        write('A'), write(Now), write(' = '), write(N), nl.
print_array(Now, Original, A):-
        access_array(A, Now, N),
        write('A'), write(Now), write(' = '), write(N), nl,
        next_index(Now, Original, Now1),
        print_array(Now1, Original, A).
