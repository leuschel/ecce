% ----------------------------------------------------------------------
%
% Distributed CIAO
% (C) UPM-CLIP 1997
%
% ----------------------------------------------------------------------

:- module(ciaod,[
        boot_worker/0,
        add_worker/1,
        add_worker/2,
        delete_worker/1,
        delete_workers/0,
        '&>'/2,
        '<&'/2,
        '&'/2,
        '&'/1,
        '&&'/1,
        '@'/2,
        sync/0,
        wait/1,
        ask/1,
        askl/1,
        use_active_module/2,
        activate_module/2,
        save_active_module/3,
        remote_call/2,
        psave/2,
        shutdown/0,
        worker/1,
        worker_list/1,
        '=>'/2,
        ground/1,
        indep/2,
        x_mode/1,
        global_silent/1,
        create_lock/1,
        lock/1,
        unlock/1,
        dump_bb_data/0,
        flush_output/0,
        string_append/2,
        execute_global/1,
        % These are of linda
        out/1,
        in/1,
        in/2,
        in_noblock/1,
        rd/1,
        rd/2,
        rd_noblock/1,
        rd_findall/3]).

:- meta_predicate
        : &> ?,
        : & : ,
        : &   ,
        : &&  ,
        : @ ? ,
        execute_global(:),
        use_active_module(?,:),
        activate_module(?,:).

:- use_module(library(unix)).
:- use_module(library(read)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(errhandle)).

:- include(ciaod_h).        

:- multifile 
        verify_attribute/2,
        combine_attributes/2,
        portray_attribute/2.

:- data
        blackboard_address/1,
        worker_number/1,
        query_counter/1,
        shv_counter/1,
        global_counter/1,
        global_builtin/2,
        '$inside_a_global_call'/0,
        x_mode_state/1,
        global_silent_stream/1.

:- include(exec_data).

/**********************************************************************\ 
 System data in the blackboard:
 
 '$l'(Id)     - Id is the identifier of last worker started.
 '$w'(Id)     - Id is the identifier of a running worker.
 '$i'(Id)     - Worker Id is idle.
 '$gc'(N)     - Counter of global commands.
 '$gh'(Id,H)  - Initial global commands for worker Id are H.
 '$h'(Id)     - Halt worker Id.
 '$c'(G,P)
 '$c'(Id,G,P) - Concurrent goal (for worker Id).
 '$cs'        - A concurrent goal has started.
 '$ce'        - A concurrent goal has ended.
 '$g'(Id,N,G) - Global command N for worker Id.
 '$d'(Id,N)   - Done global command N by worker Id.
 '$q'(N,G)
 '$q'(Id,N,G) - Query N is G (for worker Id).
 '$a'(N,A)    - Answers for query N are A.
 '$rc'(N)     - Counter of remote queries.
 '$rq'(N,G)   - Remote query N is G.
 '$ra'(N,A)   - Answers for remote query N are A.
 '$lk'(X)     - Lock X.
 '$b'(V,T)    - Binding of shared variable V is T.
\**********************************************************************/

% ----------------------------------------------------------------------
% Teams
% ----------------------------------------------------------------------

add_worker(Id) :-
        get_blackboard_address(BBAddr),
        new_worker_id(Id),
        start_worker(Id,BBAddr).

add_worker(Host,Id) :-
        is_current_host(Host), !,
        add_worker(Id).
add_worker(Host,Id) :-
        get_blackboard_address(BBAddr),
        new_worker_id(Id),
        start_worker(Id,BBAddr,Host).

get_blackboard_address(BBAddr) :-
        blackboard_address(BBAddr), !.
get_blackboard_address(BBAddr) :-
        start_blackboard(BBAddr),
        % Connect me with the blackboard
        initialize_client(0,BBAddr),
        out('$w'(0)),
        out('$l'(0)),
        retract_fact(global_counter(C)),
        out('$gc'(C)).

new_worker_id(Id) :-
        in('$l'(N)),
        Id is N+1,
        out('$l'(Id)).

% Start the blackboard process returning its address in BBAddr
start_blackboard(Host:Port) :-
        current_host(Host),
        ciao_blackboard(CiaoBB),
        unix(popen(CiaoBB, read, S)),
        read(S, Port).
%	assertz_fact(blackboard_address(Host:Port)).

start_worker(Id,BBHost:Port):-
        current_executable(Exec),
        ( x_mode_state(on) -> xterm_executable(XShell) ; XShell = ""),
        ( global_silent_stream(_) -> SOp = " s" ; SOp = " "),
        atom_append([XShell, Exec," worker ",Id," ",BBHost," ",Port,SOp," &"],
                    StartCommand),
        start_worker_executable(Id,StartCommand).

start_worker(Id,BBHost:Port,Host):-
        executable(Exec),
        ( x_mode_state(on) -> xterm_executable(XShell) ; XShell = ""),
        ( global_silent_stream(_) -> SOp = " s" ; SOp = " "),
        atom_append(["rsh -n ",Host," exec /bin/sh -c ""\\""DISPLAY=$DISPLAY ",
                     XShell, Exec," worker ",Id," ",BBHost," ",Port,SOp,
                     "\\"""" &"],
                    StartCommand),
        start_worker_executable(Id,StartCommand).

start_worker_executable(Id,StartCommand) :-
        % start worker...
        unix(system(StartCommand)),
        findall(global_builtin(N,B), global_builtin(N,B), Bs),
        out('$gh'(Id,Bs)),
        % ... and wait until it is ready
        rd('$w'(Id)).

initialize_client(Id,BBAddr) :-
        % connect to the blackboard
        linda_client(BBAddr),
        assertz_fact(blackboard_address(BBAddr)),
        assertz_fact(worker_number(Id)),
        assertz_fact(query_counter(Id)),
        assertz_fact(shv_counter(Id)).

delete_worker(Id) :-
        var(Id), !,
        rd_noblock('$i'(Id)), % Choose an idle worker
        in('$w'(Id)),
        out('$h'(Id)).
delete_worker(Id) :-
        in_noblock('$w'(Id)),
        out('$h'(Id)).

delete_workers :-
        other_workers_list(Ws),
        delete_workers(Ws).

delete_workers([]).
delete_workers([Id|Ids]) :-
        delete_worker(Id),
        delete_workers(Ids).

worker(Id) :-
        get_blackboard_address(_),
        worker_number(Id).

% ----------------------------------------------------------------------
% Executable generator, toplevel, & worker loop
% ----------------------------------------------------------------------

boot_worker :- 
        prolog_flag(argv, Args),
        boot1(Args).

boot1([worker,Id,BBHost,Port|SOp]) :- !,
        atom_codes(Port,SPort), number_codes(NPort,SPort),
        atom_codes(Id,SId), number_codes(NId,SId),
        ( SOp = [s] -> global_silent(on) ; global_silent(off) ),
        initialize_client(NId,BBHost:NPort),
        in('$gh'(NId,Builtins)),
        execute_global_history(Builtins,NId),
        out('$w'(NId)),
        idle_worker_loop(NId).
boot1(_) :-
        working_directory(WD,WD),
        % other workers start in the same directory
        assertz_fact(global_builtin(0,working_directory(_,WD))),
        assertz_fact(global_counter(1)).

idle_worker_loop(Id) :-
        out('$i'(Id)),
        in([
               '$c'(Id,_,_),  % concurrent goal for me
               '$q'(Id,_,_),  % query for me
               '$h'(Id),      % halt
               '$g'(Id,_,_),  % global call
               '$c'(_,_),     % concurrent goal
               '$q'(_,_),     % query
               '$rq'(_,_)     % remote query
           ], Command),
        ( 
            in_noblock('$i'(Id)) ->
                process_command(Command,Id)
        ;   locked_command(Command) ->
                process_command(Command,Id)
        ;   out(Command),
            in([
                   '$c'(Id,_,_),
                   '$q'(Id,_,_)
               ], NCommand),
            process_command(NCommand, Id)
        ),
        fail.
idle_worker_loop(Id) :- idle_worker_loop(Id).

locked_command('$c'(_,_,_)).
locked_command('$q'(_,_,_)).

process_command('$h'(_),_) :- !,
        % close connection to blackboard
        close_client,
        halt.
process_command('$g'(Id,N,Q),_) :- !,
        catch(reexecute_global(N,Q), Ex, global_excp(Ex,Id)),
        out('$d'(Id,N)).
process_command('$c'(Q,Ps),Id) :- !,
        assign_ids(Ps),
        catch(maybe(Q), Ex, worker_excp(Ex,Id)),
        out('$ce').
process_command('$c'(_,Q,Ps),Id) :- !,
        assign_ids(Ps),
        catch(maybe(Q), Ex, worker_excp(Ex,Id)),
        out('$ce').
process_command('$q'(N,Q),Id) :- !,
        catch(findall(Q,Q,Answers), Ex,
                     (worker_excp(Ex,Id), Answers=[])),
        out('$a'(N,Answers)).
process_command('$q'(_,N,Q),Id) :- !,
        catch(findall(Q,Q,Answers), Ex,
                     (worker_excp(Ex,Id), Answers=[])),
        out('$a'(N,Answers)).
process_command('$rq'(N,Q), Id) :- !,
        catch(findall(Q,Q,Answers), Ex,
                     (worker_excp(Ex,Id), Answers=[])),
        out('$ra'(N,Answers)).

worker_excp(ERROR,Id) :- !,
        inform_user(['[Worker ',Id,' ERROR:]']),
        worker_excp1(ERROR).

worker_excp1(error(Error,Where)) :- !,
	( handle_error(Error, Where) ; true).
worker_excp1(Exc) :-
        inform_user(['{ERROR: ',Exc,'}']).

maybe(Q) :- call(Q), !.
maybe(_).

top :-
        initial_message,
        repeat,
        prompt(_,'[ciao(H)] ?- '),
        on_exception(Ex, top(Query), catch(Ex,Query)),
        Query = end_of_file, !,
        ( blackboard_address(_BBAddr) ->
              delete_workers,
              halt_server
        ; true),
%        write(user,ciao),
%        nl(user),
	halt.

initial_message :-
	ciao_version(Version),
        ciao_version_date(Date),
	format(user_error,"CIAO (distributed) V~a (C) UPM-CLIP, ~a~n",[Version,Date]).

catch(reserved(3),continue):- !.
catch(reserved(_),end_of_file):- !.
catch(Excp,continue):-
        prolog:puncaught(Excp).

top(Query):-
	  nl(user),
          prolog:read_top_level(user,RawQuery,Vars),
	  flatten_dic(Vars,D,[]),
          nonvar(RawQuery),
          (
              prolog:call_user_def(query_expansion(RawQuery, Query), user) ->
                  true
          ;   RawQuery = Query
          ),
          top_process_query(Query,D),
          nl(user).

top_process_query(V,_) :- var(V), !, fail. % For now simply do nothing
top_process_query(end_of_file,_) :- !.
top_process_query(Query,D) :-
        answer(Query,D), !,         % dispose choice-points
        ciaod_cleanup,
        write(user,yes).
top_process_query(_,_) :-
        ciaod_cleanup,
        write(user,no).

flatten_dic(X,D,D):- var(X), !.
flatten_dic(dic(Name,[Var|_],Left,Right),[Var=AtomName|D],D_):-
	name(AtomName,Name),
	flatten_dic(Left,D,D0),
	flatten_dic(Right,D0,D_).

answer(Query,D):-
	call(user:Query),
        sync,
        ( D = [] -> true
        ; print_query_vars(D),
          get_code(user,C),
          get_newline(C),
          C = 10
        ).

print_query_vars([Val=Var]) :- !,
	format(user,"~a = ~p ? ",[Var,Val]).
print_query_vars([Val=Var|Variables]) :-
	format(user,"~a = ~p,~n",[Var,Val]),
	print_query_vars(Variables).

get_newline(10) :- !.
get_newline(_) :-
        get_code(user,C),
        get_newline(C).

sync :-
        in_noblock('$cs'), !,
        in([
               '$c'(_,_),
               '$ce'
           ], Data),
        ( Data = '$c'(_,_) -> process_command(Data,0) ; true ),
        sync.
sync.

ciaod_cleanup :-
        get_blackboard_address(_), !,
        clean_bb_bindings,
        worker(Id),
        retract_fact(query_counter(_)),
        assertz_fact(query_counter(Id)),
        retract_fact(shv_counter(_)),
        assertz_fact(shv_counter(Id)).
ciaod_cleanup.

clean_bb_bindings :-
        in_noblock('$b'(_,_)), !,
        clean_bb_bindings.
clean_bb_bindings.

% ----------------------------------------------------------------------
% Parallel & concurrent calls
% ----------------------------------------------------------------------

Q &> H :-
        get_blackboard_address(_),
        new_query_id(N), 
        % clean blackboard on backtracking
        undo(in([
                    '$a'(N,_),
                    '$q'(N,_)
                ], _)),
        out('$q'(N,Q)),
        H = query(N,Q).

query(N,Q) <& :-
        in([
               '$a'(N,_),
               '$q'(N,_)
           ], Data),
        ( Data = '$q'(_,Qr) -> findall(Qr,Qr,As) ; Data = '$a'(_,As) ),
        % restore data in blackboard on backtracking
        undo(out('$a'(N,As))),
        member(Q,As).

new_query_id(N) :-
        retract_fact(query_counter(N)),
        N1 is N+100, % This limits the number of workers to 100
        assertz_fact(query_counter(N1)).

% A & B :- B &> H, call(A), H <& .

A & Module:Bs :- 
        send_parallel(Bs, Module, Hs),
        call(A),
        receive_parallel(Hs).

send_parallel((B & Bs), Module, [H|Hs]) :- !,
        send_parallel(Bs, Module,  Hs),
        Module:B &> H.
send_parallel(B, Module, [H]) :-
        Module:B &> H.

receive_parallel([]).
receive_parallel([H|Hs]) :-
        H <&,
        receive_parallel(Hs).

Q & :-
        get_blackboard_address(_),
        get_var_ids(Q,Ps),
        local_assign_ids(Ps),
        out('$cs'),
        out('$c'(Q,Ps)).

Q && :-
        get_blackboard_address(BBAddr),
        get_var_ids(Q,Ps),
        local_assign_ids(Ps),
        (
            in_noblock('$i'(Id)) ->
                out('$cs'),
                out('$c'(Id,Q,Ps))
        ;   new_worker_id(Id),
            out('$cs'),
            out('$c'(Id,Q,Ps)),
            start_worker(Id,BBAddr)
        ).

Q @ Id & :- !,
        (
            worker(Id) -> maybe(Q)
        ;   rd_noblock('$w'(Id)),
            get_var_ids(Q,Ps),
            local_assign_ids(Ps),
            out('$cs'),
            out('$c'(Id,Q,Ps))
        ).

Q @ Id :- !,
        (
            worker(Id) -> call(Q)
        ;   rd_noblock('$w'(Id)),
            new_query_id(N), 
            out('$q'(Id,N,Q)),
            in('$a'(N,As)),
            member(Q,As)
        ).

% ----------------------------------------------------------------------
% Shared variables by attributed variables
% ----------------------------------------------------------------------

get_var_ids(Term, Pairs) :-
        get_vars(Term,[],Vars),
        make_pairs(Vars,Pairs).

get_vars(V, Aux, Vars) :-
        var(V), !,
        insert_var(V, Aux, Vars).
get_vars(T, Aux, Vars) :-
        functor(T, _F, A),
        get_vars_args(A, T, Aux, Vars).

get_vars_args(0, _, Vars, Vars) :- !.
get_vars_args(N, T, Aux, Vars) :-
        N > 0,
        arg(N, T, An),
        get_vars(An, Aux, Aux1),
        N1 is N-1,
        get_vars_args(N1, T, Aux1, Vars).

insert_var(X, [], [X]) :- !.
insert_var(X, L, L) :- L = [Y|_], X == Y, !.
insert_var(X, [Y|Ys], [Y|Zs]) :-
        insert_var(X, Ys, Zs).

make_pairs([], []).
make_pairs([X|Xs], [(X,Id)|Ps]) :-
        shv_id(X,Id),
        make_pairs(Xs, Ps).

shv_id(X, Id) :-
        get_attribute(X,shv(_,Id)), !.
shv_id(X, Id) :-
        new_shv_id(Id),
        attach_attribute(X,shv(X,Id)).

new_shv_id(N) :-
        retract_fact(shv_counter(N)),
        N1 is N+100, % This limits the number of workers to 100
        assertz_fact(shv_counter(N1)).

assign_ids([]).
assign_ids([(X,Id)|Ps]) :-
        attach_attribute(X,shv(X,Id)),
        assign_ids(Ps).

local_assign_ids([]).
local_assign_ids([(X,Id)|Ps]) :-
        (
            get_attribute(X, shv(_,_)) -> true
        ;   attach_attribute(X,shv(X,Id))
        ),
        local_assign_ids(Ps).

wait(X) :-
        get_attribute(X, shv(_,Ix)), !,
        wait_bind(Ix,B),
        wait_nonvar(B,Tbb),
        detach_attribute(X),
        trans_term_from_bb(Tbb,X).
wait(_).

wait_nonvar(X,T) :-
        X = '$shv'(Ix), !,
        wait_bind(Ix,B),
        wait_nonvar(B,T).
wait_nonvar(T,T).

wait_bind(X,B) :- rd('$b'(X,B)).

:- op(700, xfx, '<>').

ask(nonvar(X)) :- !, wait(X).
ask(X=Y) :- askl([X=Y]).
ask(X<>Y) :- \+ askl([X=Y]).

askl(L) :-
        bindings_to_wait(L,L1,[],B,[]),
        (
            B = [] -> true
        ;   wait_one_binding(B),
            askl(L1)
        ).

bindings_to_wait([],L,L,B,B).
bindings_to_wait([X=Y|R],L,L_,B,B_) :-
        import_bb_value(X),
        import_bb_value(Y),
        bindings_to_wait1(X,Y,L,L0,B,B0),
        bindings_to_wait(R,L0,L_,B0,B_).

bindings_to_wait1(X,Y,L,L_,B,B_) :-
        get_attribute(X,shv(_,Ix)), !,
        bindings_to_wait_shv(Y,X,Ix,L,L_,B,B_).
bindings_to_wait1(X,Y,L,L_,B,B_) :-
        var(X), !,
        X = Y, % unify existential variables
        L = L_, B = B_.
bindings_to_wait1(X,Y,L,L_,B,B_) :-
        bindings_to_wait_str(Y,X,L,L_,B,B_).

bindings_to_wait_shv(Y,X,Ix,L,L_,B,B_) :-
        get_attribute(Y,shv(_,Iy)), !,
        (
            Ix = Iy ->
                L = L_, B = B_
        ;   L = [X=Y|L_],
            B = ['$b'(Ix,_),'$b'(Iy,_)|B_]
        ).
bindings_to_wait_shv(Y,X,_Ix,L,L_,B,B_) :-
        var(Y), !,
        Y = X, % unify existential variables
        L = L_, B = B_.
bindings_to_wait_shv(Y,X,Ix,L,L_,B,B_) :-
        L = [X=Y|L_],
        B = ['$b'(Ix,_)|B_].

bindings_to_wait_str(Y,X,L,L_,B,B_) :-
        get_attribute(Y,shv(_,Iy)), !,
        L = [Y=X|L_],
        B = ['$b'(Iy,_)|B_].
bindings_to_wait_str(Y,X,L,L_,B,B_) :-
        var(Y), !,
        Y = X, % unify existential variables
        L = L_, B = B_.
bindings_to_wait_str(Y,X,L,L_,B,B_) :-
        functor(X,F,A),
        functor(Y,F,A),
        bindings_to_wait_args(A,X,Y,L,L_,B,B_).

bindings_to_wait_args(0,_,_,L,L,B,B) :- !.
bindings_to_wait_args(N,X,Y,L,L_,B,B_) :-
        N > 0,
        N1 is N-1,
        arg(N,X,Ax),
        arg(N,Y,Ay),
        import_bb_value(Ax),
        import_bb_value(Ay),
        bindings_to_wait1(Ax,Ay,L,L0,B,B0),
        bindings_to_wait_args(N1,X,Y,L0,L_,B0,B_).

wait_one_binding(B) :- rd(B,_).

verify_attribute(shv(X,Ix), Term) :-
        (
            import_bb_value(X,Ix,_) ->
                true
        ;   detach_attribute(X),
            trans_term_to_bb(Term, BBTerm),
            bb_bind(Ix,BBTerm)
        ),
        X = Term.

combine_attributes(shv(X,Ix),shv(Y,Iy)) :-
        (
            import_bb_value(X,Ix,BBValx) ->
                (
                    import_bb_value(Y,Iy,_) ->
                        true
                ;   detach_attribute(Y),
                    bb_bind(Iy,BBValx)
                )
        ;   detach_attribute(X),
            (
                import_bb_value(Y,Iy,BBValy) ->
                    bb_bind(Ix,BBValy)
            ;   bb_bind(Ix,'$shv'(Iy))
            )
        ),
        X = Y.

portray_attribute(shv(_,Ix),X) :-
        import_bb_value(X,Ix,_) ->
            print(X)
        ;
        write('_s'), write(Ix).

actualize_from_bb(X) :-
        get_attribute(X,shv(_,Ix)),
        read_bb_bind(Ix,BBVal0x), !,
        bb_dereference(BBVal0x,BBValx),
        trans_term_from_bb(BBValx,Valx),
        detach_attribute(X),
        X = Valx.
actualize_from_bb(X) :-
        var(X), !.
actualize_from_bb(X) :-
        functor(X,_,A),
        actualize_from_bb_args(A,X).

actualize_from_bb_args(0,_) :- !.
actualize_from_bb_args(N,X) :-
        N > 0,
        arg(N,X,Ax),
        actualize_from_bb(Ax),
        N1 is N-1,
        actualize_from_bb_args(N1,X).

% Like actualize_from_bb(X), but do not check structures
import_bb_value(X) :-
        get_attribute(X,shv(_,Ix)),
        read_bb_bind(Ix, BBVal0x), !,
        bb_dereference(BBVal0x,BBValx),
        trans_term_from_bb(BBValx,Valx),
        detach_attribute(X),
        X = Valx.
import_bb_value(_).

% Fails if not exists a binding in the blackboard
import_bb_value(X, Ix, BBValx) :-
        read_bb_bind(Ix, BBVal0x),
        bb_dereference(BBVal0x, BBValx),
        trans_term_from_bb(BBValx,Valx),
        detach_attribute(X),
        X = Valx.

trans_term_to_bb(V,BBV) :-
        var(V), !,
        shv_id(V,Iv),
        BBV = '$shv'(Iv).
trans_term_to_bb(Term,BBTerm)  :- 
	functor(Term,F,N),
	functor(BBTerm,F,N),
	trans_term_to_bb_args(N,Term,BBTerm).

trans_term_to_bb_args(0,_,_) :- !.
trans_term_to_bb_args(N,Term,BBTerm) :-
	N > 0,
	arg(N,Term,Arg),
	arg(N,BBTerm,BBArg),
	trans_term_to_bb(Arg,BBArg),
	N1 is N-1,
	trans_term_to_bb_args(N1,Term,BBTerm).

% First argument must be dereferenced
trans_term_from_bb('$shv'(Iv),V) :- !,
        attach_attribute(V,shv(V,Iv)).
trans_term_from_bb(BBTerm,Term) :-
	functor(BBTerm,F,N),
	functor(Term,F,N),
        trans_term_from_bb_args(N,BBTerm,Term).

trans_term_from_bb_args(0,_,_) :- !.
trans_term_from_bb_args(N,BBTerm,Term) :-
	N > 0,
	arg(N,BBTerm,BBArg0),
        bb_dereference(BBArg0, BBArg),
	arg(N,Term,Arg),
	trans_term_from_bb(BBArg,Arg),
	N1 is N-1,
	trans_term_from_bb_args(N1,BBTerm,Term).

bb_dereference(X, V) :-
        X = '$shv'(Ix),
        read_bb_bind(Ix,B), !,
        bb_dereference(B, V).
bb_dereference(V,V).

read_bb_bind(V,T) :-
	rd_noblock('$b'(V,T)).

bb_bind(V,T) :-
	out('$b'(V,T)).	

write_bb_bindings :-
        rd_findall('$shv'(V)/T,'$b'(V,T),Bindings),
        write_list(Bindings), flush_output.

% ----------------------------------------------------------------------
% Parallelism support
% ----------------------------------------------------------------------

:- op(975,xfx,[(=>)]).

A => B :-
	call(A),
	!,
	call(B).
_A => B :-
	ciao_make_sequential(B,S),
	call(S).

ciao_make_sequential(&(A,&(C,D)),','(A,B)) :- !,
	ciao_make_sequential(&(C,D),B).
ciao_make_sequential(&(A,B),','(A,B)).

% Ground & Indep code

ground(T) :- mark(T, yes).

indep([]) :- !.
indep([[X,Y]|More]) :-
	indep(X,Y),
	indep(More).

% indep if not dependent: so that variables are untouched upon success
% [ i.e. dep(A,B) :- mark(A), marked(B). ]

indep(A,B) :- 
	mark(A,Ground),  % Ground is var if A ground
	nonvar(Ground),  % If 1st argument was ground, no need to proceed
	marked(B),
	!,fail.
indep(_,_).

mark('$$Mark', no )  :- !.
mark( Atom   , _  )  :- atomic(Atom),!.
mark(Complex , GR)  :- mark(Complex,1,GR).

mark(Args,Mth,GR) :-
	arg(Mth,Args,ThisArg),!,
	mark(ThisArg,GR),
	Nth is Mth+1,
	mark(Args,Nth,GR). 	
mark(_,_,_).

marked( Term )  :-
	functor(Term,F,A),
	(  A > 0 -> marked(Term,1)
	;  F = '$$Mark' ).

marked(Args,Mth) :-
	arg(Mth,Args,ThisArg),!,
        (  marked(ThisArg)
	;  Nth is Mth+1,
	   marked(Args,Nth)).

% ----------------------------------------------------------------------
% Global calls
% ----------------------------------------------------------------------

execute_global_history([],_).
execute_global_history([global_builtin(N,B)|RestBuiltins],Id) :-
        on_exception(Ex, reexecute_global(N,B), global_excp(Ex,Id)),
        execute_global_history(RestBuiltins,Id).

reexecute_global(N,B) :-
        global_silent_stream(Null) ->
            prolog_flag(user_error,SE,Null),
            execute_global(N,B),
            prolog_flag(user_error,_,SE)
        ;
        execute_global(N,B).

execute_global(Call) :-
        '$inside_a_global_call', !,
        call(Call).
execute_global(Call) :-
        blackboard_address(_BBAddr), !,
        next_global_number_BB(N),
        other_workers_list(OWs),
        send_global(OWs,N,Call),
        execute_global(N,Call),
        receive_global(OWs,N).
execute_global(Call) :-
        next_global_number(N),
        execute_global(N,Call).

execute_global(N,B) :-
        assertz_fact(global_builtin(N,B)),
        assertz_fact('$inside_a_global_call'),
        maybe(B),
        retract_fact('$inside_a_global_call').

next_global_number(N) :-
        retract_fact(global_counter(N)),
        N1 is N+1,
        assertz_fact(global_counter(N1)).

next_global_number_BB(N) :-
        in('$gc'(N)),
        N1 is N+1,
        out('$gc'(N1)).

other_workers_list(OWs) :-
        worker_list(Ws),
        worker_number(MyId),
        delete_one(Ws, MyId, OWs), !.
other_workers_list([]).

worker_list(Ws) :-
        rd_findall(W, '$w'(W), Ws).

send_global([],_N,_Global).
send_global([W|Ws],N,Global) :-
        out('$g'(W,N,Global)),
        send_global(Ws,N,Global).

receive_global([],_N).
receive_global([W|Ws],N) :-
        in('$d'(W,N)),
        receive_global(Ws,N).

global_excp(Excp,Id) :-
          global_silent_stream(_) -> true ; worker_excp(Excp,Id).

% ----------------------------------------------------------------------
% Remote modules
% ----------------------------------------------------------------------


use_active_module(RemoteModule, CurrentModule:Predicates) :-
        define_remote_predicates(Predicates, RemoteModule, CurrentModule).

define_remote_predicates([],_,_).
define_remote_predicates([F/A|Ps], Mod, CM) :-
        functor(P,F,A),
	retractall(CM:P),
        assert(CM:(P :- module_address(Mod,Add), remote_call(Add, user:P))),
        define_remote_predicates(Ps, Mod, CM).

remote_call(Address, Q) :-
        remote_call_stream(Address,Stream),
        in_stream(Stream, '$rc'(N)),
        N1 is N+1,
        out_stream(Stream, '$rc'(N1)),
        out_stream(Stream, '$rq'(N, Q)),
        in_stream(Stream, '$ra'(N, As)),
%        close(Stream), % ?
        member(Q, As).

:- dynamic '$remote_stream'/2.

remote_call_stream(Address,Stream) :-
        '$remote_stream'(Address,Stream),
        current_stream(_N,socket,Stream), !.
remote_call_stream(Address,Stream) :-
        open_client(Address, Stream),
        assertz_fact('$remote_stream'(Address,Stream)).

activate_module(Address, Hook) :-
        Address = Host:Port,
        var(Port), !,
        var(Host),
        get_blackboard_address(Address),
        worker_number(0), % Only worker 0 must activate a module
        call(Hook),
        out('$rc'(0)),
        idle_worker_loop(0).
activate_module(Address, Hook) :-
        Address = Host:Port,
        integer(Port),
        ciao_blackboard(CiaoBB),
        (
            current_host(Host) ->
                atom_append([CiaoBB," ",Port," &"], CiaoBBport)
        ;   atom_append(["rsh ",Host," ",CiaoBB," ",Port," &"], CiaoBBport)
        ),
        unix(system(CiaoBBport)),
	assertz_fact(blackboard_address(Address)),
        initialize_client(0,Address),
        out('$w'(0)),
        out('$l'(0)),
        retract_fact(global_counter(C)),
        out('$gc'(C)),
        call(Hook),
        out('$rc'(0)),
        idle_worker_loop(0).

save_active_module(Name, Address, Hook) :-
        psave(Name, Return),
        boot_active_module(Return, Address, Hook).

boot_active_module(0, _, _).
boot_active_module(1, Address, Hook) :-
        activate_module(Address, Hook).

% Not to call interactively
shutdown :-
	delete_workers,
	halt_server,
	halt.
        
% ----------------------------------------------------------------------
% Support predicates
% ----------------------------------------------------------------------

x_mode_state(off).

x_mode(X) :-
        retractall_fact(x_mode_state(_)),
	assertz_fact(x_mode_state(X)).

global_silent(off) :-
        retract_fact(global_silent_stream(Null)) -> close(Null) ; true.
global_silent(on) :- inform_user(['Option not available (yet)']).
%        global_silent_stream(_) -> true
%        ;
%        open_null_stream(Null),
%	assertz_fact(global_silent_stream(Null)).

%% LOCKS

create_lock(X) :-
	rd_noblock('$lk'(X)),
	!.
create_lock(X) :-
	out('$lk'(X)).

lock(X) :-
	in('$lk'(X)).

unlock(X) :-
	out('$lk'(X)).

dump_bb_data :-
	rd_findall(T,T,Tuples),
	write_list(Tuples).

write_list([]).
write_list([T|Ts]):-
	write(T),
	nl,
	write_list(Ts).

member(X, [X|_]).
member(X, [_|L]) :- member(X, L).

delete_one([], _, []).
delete_one([X|Xs], X, Xs) :- !.
delete_one([X|Xs], Y, [X|Zs]) :-
        delete_one(Xs, Y, Zs).

append([], X, X).
append([X|Xs], Y, [X|Zs]) :-
	append(Xs,Y,Zs).

string_append([], []).
string_append([L|Ls], R) :-
        string_append(Ls, R0),
        to_string(L, S),
        append(S, R0, R).

atom_append(L, A) :-
        string_append(L, S),
        atom_codes(A,S).

to_string([],"") :- !. % empty string
to_string(A, S) :-
        atomic(A), !,
        name(A, S).
to_string(S, S).


is_current_host(H) :-
        current_host(Host),
        ( H = Host, !
        ; atom_codes(Host, HostS),
          append(HS, "."|| _, HostS), !,
          atom_codes(H, HS)
        ).
