:- use_module(library('persdb/persdbrt'), [ % do not redefine builtins
        passerta_fact/1, 
        passertz_fact/1, 
        pretract_fact/1,
        init_persdb/0, 
        initialize_db/0,
        make_persistent/2,
        update_files/0,
        update_files/1]).

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
