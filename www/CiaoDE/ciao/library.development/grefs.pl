
sicstus_system_library('/usr/local/lib/sicstus3.6/library/system').

:- consult(compat).

:- use_module(library(grefs)).

:- sicstus_system_library(S), load(davinci:S).

save:-
	save(grefs),
	write(user,'Use: grefs/1, wgrefs/2, set_files/1, set_flag/1, quit/0'),
	nl(user),
	write(user,'Other predicates available (with xrefs:): '),
	write(user,'xrefs/0, xrefs/1, xrefs/2, xrefsg/2, xrefsg/3'),
	nl(user),
	write(user,'Remember to first compile a file with all '),
	write(user,'op declarations you will use'),
	nl(user).
