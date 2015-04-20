%% Program to check that sformat works properly when 
%% printing long warning messages.  
%% This program uses CiaoPP.

:- module(_,[main/0],[]).

:- use_module(ciaopp(ciaopp)).
:- use_module(plai(intermod)).
%:- use_module(library(system),[mktemp/2, make_dirpath/1, delete_directory/1]).


main:-
	set_pp_flag(success_policy,botall),
% 	mktemp('regXXXXXX',Dirname),
% 	atom_concat('/tmp/',Dirname,TmpDir),
% 	make_dirpath(TmpDir),
%	set_pp_flag(tmp_dir,TmpDir),
	set_pp_flag(tmp_dir,'/tmp'),
%	cleanreg,
	set_pp_flag(fixpoint,di),
	set_pp_flag(intermod,on),
	set_pp_flag(verbose_ctchecks,on),
	set_pp_flag(ass_not_stat_eval,warning),
	set_pp_flag(pred_ctchecks, new_all_succ),
	set_pp_flag(use_check_assrt,on),
	inductive_ctcheck(shfr, managing_project),
%	cleanreg,
%	delete_directory(TmpDir).
	true.
