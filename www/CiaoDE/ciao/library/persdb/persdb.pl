:- use_module(library('persdb/persdbrt'), [ % redefine builtins
        asserta_fact/1, 
        assertz_fact/1, 
        retract_fact/1,
	retractall_fact/1,
        init_persdb/0, 
        initialize_db/0,
        make_persistent/2,
        update_files/0,
        update_files/1]).

:- redefining(asserta_fact/1).
:- redefining(assertz_fact/1).
:- redefining(retract_fact/1).
:- redefining(retractall_fact/1).

:- multifile('$is_persistent'/2).
:- data '$is_persistent'/2.
:- meta_predicate('$is_persistent'(spec,?)).

:- multifile persistent_dir/2.
:- data persistent_dir/2.

:- multifile persistent_dir/4.
:- data persistent_dir/4.

:- initialization(init_persdb).

:- load_compilation_module(library('persdb/persdbtr')).
:- add_sentence_trans(persistent_tr/2).
:- add_term_trans(persistent_term_tr/3).
