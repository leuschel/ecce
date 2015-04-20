:- multifile define_flag/3.

define_flag(format,[shell,posix,struct],posix).
define_flag(exact,[on,off],on).

:- load_compilation_module(library('regexp/regexp_trans')).

:- add_sentence_trans(pattern_unification/2).

:- op(   200,   fy,   (=~)).   % Like unary '+'

:- use_module(library('regexp/regexp_code')).
