/*             Copyright (C)1990-2002 UPM-CLIP				*/

%----------------------------------------------------------------------------
% Support CIAO builtins in standard Prolog systems (e.g., SICStus).
%----------------------------------------------------------------------------

:- module(builtin, [
%        ','/2, ';'/2, '->'/2, '^'/2, (\+)/1, if/3, !/0,
%        true/0, % do not change this!
%        fail/0, call/1,
%        halt/0, halt/1, abort/0,
%        'C'/3, (=)/2, (<)/2, (=<)/2, (>)/2, (>=)/2, (=:=)/2, (=\=)/2,
%        (==)/2, (\==)/2, (@<)/2, (@>=)/2, (@>)/2, (@=<)/2, (=..)/2,
%        arg/3, atom/1, atomic/1, (is)/2, nonvar/1, number/1,
%        integer/1, float/1,
%        ground/1,
%        var/1, arg/3, compare/3, functor/3,
%        type/2,	
%        get_attribute/2, attach_attribute/2, update_attribute/2,
%        detach_attribute/1,
        catch/3, throw/1, intercept/3,
        asserta_fact/1, asserta_fact/2, assertz_fact/1, assertz_fact/2,
        current_fact/1, current_fact/2, retract_fact/1, retractall_fact/1,
        set_fact/1,
%        erase/1,
        inform_user/1, display_list/1,
%        gc/0, nogc/0, fileerrors/0, nofileerrors/0,
        set_prolog_flag/2, current_prolog_flag/2,
%        prolog_flag/2, prolog_flag/3, prompt/2,
        % module related
%        primitive_meta_predicate/1,
	term_to_meta/2, % Should go to internals?
%        module_concat/3,
        % C predicates
%        name/2,
	atom_codes/2, number_codes/2,
	atom_length/2, atom_concat/3, % sub_atom/4,
%        copy_term/2,
%        stream_code/2, close/1, current_input/1, set_input/1,
%        current_output/1, set_output/1,
%        character_count/2, line_position/2, line_count/2,
%        code_class/2, flush_output/0, flush_output/1,
%        getct/2, getct1/2,
	get_code/1, get_code/2, get1_code/1, get1_code/2,
        peek_code/1, peek_code/2, put_code/1, put_code/2, 
        skip_code/1, skip_code/2, % nl/0, nl/1, tab/1, tab/2,
%        display/1, displayq/1, displayq/2,
	display/2,
%        current_executable/1, working_directory/2,
%        current_host/1, getenvstr/2, pause/1, get_pid/1, get_arch/1,
        walltime/1,
%        statistics/0, garbage_collect/0, repeat/0,
%        current_atom/1, current_stream/3, current_predicate/2,
%        absolute_file_name/2,
	absolute_file_name/7
%        current_module/1, % implicit
        %% Launching threads, concurrency
%        launch_goal/1, launch_goal/2, launch_goal/3,
%        kill_thread/1, thread_self/1, tasks_status/0, 
%        lock_atom/1, unlock_atom/1,
%        close_predicate/1, open_predicate/1,
%        ciaolibdir/1
        ]).

%----------------------------------------------------------------------------
% CIAO builtins not in SICStus
%----------------------------------------------------------------------------
% (very incomplete!)

%------ errors ------%

:- data catching/3, thrown/1.

:- meta_predicate
        catch(:, ?, :),
        intercept(:, ?, :).

catch(Goal, Error, _) :-
        prolog:'$metachoice'(Choice),
        asserta_undo(catching(Choice, Error, [])),
        'SYSCALL'(Goal),
        retract_undo(catching(Choice, Error, [])).
catch(_, Error, Handler) :-
        retract_fact(thrown(Error)), !,
        'SYSCALL'(Handler).

intercept(Goal, Error, Handler) :-
        prolog:'$metachoice'(Choice),
        asserta_undo(catching(Choice, Error, Handler)),
        'SYSCALL'(Goal),
        retract_undo(catching(Choice, Error, Handler)).

throw(Error) :-
        var(Error), !,
        throw(error(instantiation_error, throw/1-1)).
throw(Error) :-
        current_fact(catching(C, E, H)),
        E = Error, !,
        throw_action(H, E, C).
throw(Error) :-
        display(user_error, '{ERROR: No handle found for thrown error '),
        display(user_error, Error),
        display(user_error, '}'),
        nl(user_error),
        abort.

throw_action([], Error, Choice) :-
        asserta_fact(thrown(Error)),
        cut_to(Choice), % This cuts also next clause
        fail.
throw_action(Handler, _, _) :-
        'SYSCALL'(Handler).

cut_to(Choice) :-
        retract_fact(catching(C,_,_)),
        C = Choice,
        prolog:'$metacut'(Choice).

'SYSCALL'(Goal):- call(Goal).

%------ facts ------%

:- meta_predicate(asserta_fact(:)).
asserta_fact(X)    :- asserta(X).

:- meta_predicate(asserta_fact(:,?)).
asserta_fact(X,Y)  :- assert(X,Y).

:- meta_predicate(assert_fact(:)).
assert_fact(X)     :- assert(X).

:- meta_predicate(assert_fact(:,?)).
assert_fact(X,Y)   :- assert(X,Y).

:- meta_predicate(assertz_fact(:)).
assertz_fact(X)    :- assertz(X).

:- meta_predicate(assertz_fact(:,?)).
assertz_fact(X,Y)  :- assertz(X,Y).

:- meta_predicate(current_fact(:)).
current_fact(X)    :- clause(X,_).

:- meta_predicate(current_fact(:,?)).
current_fact(X,Y)  :- clause(X,Y).

:- meta_predicate(retract_fact(:)).
retract_fact(X)    :- retract(X).

:- meta_predicate(retractall_fact(:)).
retractall_fact(X) :- retractall(X).

:- meta_predicate(set_fact(:)).
set_fact(Fact) :-
        term_to_meta(Fact_t, Fact),
        functor(Fact_t, F, A),
        functor(Template_t, F, A),
        term_to_meta(Template_t, Template),
        retractall_fact(Template),
        asserta_fact(Fact).

asserta_undo(X) :- asserta_fact(X).
asserta_undo(X) :- retract_fact(X), fail. % Beware! cannot use undo/1

retract_undo(X) :- retract_fact(X).
retract_undo(X) :- asserta_fact(X), fail. % Beware! cannot use undo/1

%------ messages --------%

inform_user(MessL) :-
%        '$quiet_flag'(off, off), !,
        current_output(S),
        set_output(user_error),
        display_list(MessL), nl,
        set_output(S).
%inform_user(_).

display_list([M|Ms]) :- !,
        display(M),
        display_list(Ms).
display_list([]) :- !.
display_list(M) :-
        display(M).

%-------- FLAGS ----------%

% Does not work: we lack flags like e.g. write_strings
% set_prolog_flag(X,Y) :- prolog_flag(X,_,Y).
set_prolog_flag(_,_).

current_prolog_flag(_,on).

%-------- TERMS ----------%

term_to_meta(X, X).

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

atom_codes(X,Y) :- atom_chars(X,Y).

number_codes(X,Y) :- number_chars(X,Y).

atom_concat(A,B,C) :-
        atom(A), atom(B), !,
	atom_chars(A,AList),
	atom_chars(B,BList),
	append(AList,BList,CList),
	atom_chars(C,CList).
atom_concat(A,B,C) :-
        atom(C), !,
        (atom(A) -> atom_chars(A,AS) ; var(A)),
        (atom(B) -> atom_chars(B,BS) ; var(B)),
        atom_chars(C, CS),
        append(AS, BS, CS),
        (var(A) -> atom_chars(A,AS) ; !),
        (var(B) -> atom_chars(B,BS) ; !).
atom_concat(_,_,C) :-
        var(C), !, throw(error(instantiation_error, atom_concat/3-3)).
atom_concat(_,_,C) :-
        throw(error(type_error(atom,C), atom_concat/3-3)).

atom_length(A, L) :-
        atom(A), !,
        atom_chars(A,S), length(S,L).
atom_length(V, _) :-
        var(V), !, throw(error(instantiation_error, atom_length/2-1)).
atom_length(N, _) :-
        throw(error(type_error(atom,N), atom_length/2-1)).

%-------- FILES ----------%

peek_code(X)   :- peek_char(X).

peek_code(X,Y) :- peek_char(X,Y).

put_code(X)   :- put(X).

put_code(X,Y) :- put(X,Y).

get_code(X) :- get0(X).

get_code(X,Y) :- get0(X,Y).

get1_code(X) :- get(X).

get1_code(X,Y) :- get(X,Y).

skip_code(X)   :- skip(X).

skip_code(X,Y) :- skip(X,Y).


% A raw approximation (PBC)
absolute_file_name(engine(Name),Opt,Suffix,CurrDir,AbsFile,AbsBase,AbsDir) :-
	absolute_file_name(library(Name),Opt,Suffix,CurrDir,AbsFile,AbsBase,AbsDir).
absolute_file_name(Spec,_Opt,Suffix,CurrDir,AbsFile,AbsBase,AbsDir) :-
	prolog:absolute_file_name(Spec,Suffix,CurrDir,AbsFile,AbsDir,true),
	!,
	name(Suffix,SList),
	name(AbsFile,FList),
	( append(BList,SList,FList)
	-> name(AbsBase,BList)
	 ; AbsBase=AbsFile
	).
absolute_file_name(Spec,_Opt,_Suffix,_CurrDir,_AbsFile,_AbsBase,_AbsDir) :-
	prolog:'$ferror_flag'(on, on),
	throw(error(existence_error(source_sink,Spec),
                            absolute_file_name/7-1)).

%-------- others ----------%

walltime(Time):- statistics(runtime,[_,Time]).

display(Stream,Mess) :-
        current_output(S),
        set_output(Stream),
        display(Mess),
        set_output(S).
