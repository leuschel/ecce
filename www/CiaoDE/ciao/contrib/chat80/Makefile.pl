%% ---------------------------------------------------------------------------
%% Makefile used to build and install the webchat executable
%% ---------------------------------------------------------------------------
:- module(_,_,[assertions,make,functions]).
:- use_module(library('make/system_extra')).
:- use_module(library(format),[format/2,format/3]).
:- use_module(library(terms),[atom_concat/2]).
%% ---------------------------------------------------------------------------
%% CONFIG overloads common with current conventions...
:- use_module('SETTINGS'). 
%% ---------------------------------------------------------------------------
:- discontiguous comment/2.

%% ---------------------------------------------------------------------------
comment(install,['Installation of (static) ',~basemain,' executable.']).
%% ---------------------------------------------------------------------------

install <- [~target] :-
	true.

%% ---------------------------------------------------------------------------
comment(~target,['Installation of (static) ',~basemain,' executable.']).
%% ---------------------------------------------------------------------------

target := ~atom_concat([~bin,'/',~basemain,'.cgi']).

~target <- [~basemain] :-
	copy_file(~basemain,~target),
	set_perms(~target,~execpermissions).

%% ---------------------------------------------------------------------------
comment(~basemain,['Compiling (static) ',~basemain,' executable.']).
%% ---------------------------------------------------------------------------

%% ~basemain   <-    [ ~atom_concat([~basemain,'.pl']) ]     :-
%% To force ciaoc to always follow dependencies (temporary)
~basemain   <-  :-
	basemain(BM),
        ciaoc(C),
	do([C,' -s ',BM],nofail).

%% ---------------------------------------------------------------------------
comment(clean,['Cleaning of ',~basemain,' source dir.']).
%% ---------------------------------------------------------------------------

distclean   <-    :-
	delete_tmp_files_in('.'),
	delete_tmp_files_in(db),
	delete_tmp_files_in(nl),
	delete_tmp_files_in(top),
        del_files_nofail([webchat]).

delete_tmp_files_in(Dir) :-
	working_directory(WD,WD),
	cd(Dir),
	del_files_nofail(~ls('*_co.pl|*_opt.pl|*~|*.asr|*.mpp|*.itf|*.po')),
	cd(WD).

%% ---------------------------------------------------------------------------
%% Support
%% ---------------------------------------------------------------------------

target_comment(Target) :- 
	comment(Target,Comment),
	display(~atom_concat([
'*** ----------------------------------------------------------------------\n',
'*** ', ~atom_concat(Comment), '\n',
'*** ----------------------------------------------------------------------\n'
	])).
