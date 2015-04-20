
:- use_module(library('compiler/c_itf'),
	[process_file/7,exports/5,def_multifile/4,clause_of/7]).
:- use_module(library(format),[format/2]).
:- use_module(library(lists),[append/3]).
:- use_module(library(system)).

main([Mod,Path]):- !,
	atom_concat(Mod,'_opt.pl',OutLink),
	( file_exists(OutLink)
	-> delete_file(OutLink)
	 ; true
	),
	process_file(Mod,ams,any,actmod(Mod,Path),false,false,true),
	file_name(OutFile),
	link_output(OutLink,OutFile).
main(_):-
	warning('I need module name and its http path (starting / from the http root)').

false(_):- fail.
true(_).

actmod(Base,Mod,Path):-
	Suffix='.pl',
	absolute_file_name(Base,'',Suffix,'.',_AbsFile,AbsBase,_AbsDir),
	atom_concat('_ams_client',Suffix,Suffix1),
	atom_concat(AbsBase,Suffix1,OutFile),
	open(OutFile,write,Stream),
	set_output(Stream),
	findall(F/A,exports(Base,F,A,_DefType,_Meta),Exports),
	findall(F/A,def_multifile(Base,F,A,_DynType),Multipreds),
	format(":- module(~q,~q).~n",[Mod,[shut_down/0|Exports]]),
	format(":- use_module(library('ams/amsrt')).~n",[]),
	format(":- multifile '$ams$actmod'/3,'$ams$app'/1.~n",[]),
	[X]="/",
	name(AbsBase,AbsBaseStr),
	strip(AbsBaseStr,X,DescStr),
	name(Desc,[X|DescStr]),
	atom_concat(Desc,Suffix,File),
	atom_concat(Path,File,Suffix2),
	current_host(CurrHost),
	atom_concat(CurrHost,Suffix2,Suffix3),
	atom_concat('http://',Suffix3,Mid),
	name(Mid,MidStr),
	set_prolog_flag(write_strings, on),
	format(":- initialization(start_up(~q)).~n",[MidStr]),
	format(":- on_abort(shut_down).~n",[]),
	format(":- data my_sid/1.~n~n",[]),
	format("start_up(Mid):-~n",[]),
	format("    '$ams$app'(App),~n",[]),
	format("    '$ams$actmod'(~q,Mode,Host),~n",[Mod]),
	format("    absolute_file_name(App,FilePath),~n",[]),
	format("    atom_concat(~q,FilePath,Suffix1),~n",[CurrHost]),
	format("    atom_concat('file://',Suffix1,Aid),~n",[]),
	format("    ams_startup(Host,Aid,~q,Mid,Mode,Sid),~n",[Mod]),
	format("    asserta_fact(my_sid(Sid)).~n~n",[]),
	format("shut_down:-~n",[]),
	format("    my_sid(Sid),~n",[]),
	format("    ams_shutdown(Sid).~n~n",[]),
	write_directives(Base),
	write_clauses(Exports),
	write_clauses(Multipreds),
	close(Stream),
	asserta_fact(file_name(OutFile)).

:- data file_name/1.

write_clauses([]).
write_clauses([F/A|Exports]):-
	functor(H,F,A),
	format("~q :-~n",[H]),
	format("    my_sid(Sid),~n",[]),
	format("    ams_call(Sid,~q).~n~n",[H]),
	write_clauses(Exports).

write_directives(Base):-
	clause_of(Base,Head,Body,_VarNames,_Source,_Line0,_Line1),
	number(Head),
	format(":- ~q.~n",[Body]),
	fail.
write_directives(_):-
	nl.

strip(Path,X,File):-
	append(_Path,[X|Path1],Path), !,
	strip(Path1,X,File).
strip(File,_,File).

link_output(LinkName,OutFileName):-
	( file_exists(LinkName)
	-> delete_file(LinkName)
	 ; true
	),
	atom_concat('ln -s ',OutFileName,Comm0),
	atom_concat(Comm0,' ',Comm1),
	atom_concat(Comm1,LinkName,Comm),
	system(Comm).
