:- module(_,_,[functions]).

:- use_module(ciaosrc('CIAOSETTINGS'),[srcbindir/1,vers/1]).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library('make/system_extra'),[no_tr_nl/2]).
:- use_module(library(distutils),[atom_concat/2]).

:- function(arith(false)).

revision_file := ~atom_concat(~ciaodesrc,'/REVISION').

% Note: svnversion is done only over etc to avoid overhead

get_svn_revision(Revision) :-
	svn_revision_string(~atom_concat(~ciaodesrc,'/etc'), Revision).

ciaode_revision_string(Revision) :-
	get_svn_revision(Revision),
	Revision \== "exported",
	!.
ciaode_revision_string(Revision) :-
	revision_file(RevisionFile),
	file_exists(RevisionFile),
	!,
	no_tr_nl(~file_to_string(RevisionFile), Revision).
ciaode_revision_string("Unknown").

versionfileabs(ciaode, Value) :-
	!,
	versionfileabs(ciao, Value).
versionfileabs(Component) := ~atom_concat([~ciaodesrc, '/', Component,
	'/version/GlobalVersion']).
vers(Component, Version) :-
	no_tr_nl(~file_to_string(~versionfileabs(Component)),VS),
	atom_codes(Version,VS).

patchfileabs(ciaode, Value) :-
	!,
	patchfileabs(ciao, Value).
patchfileabs(Component) := ~atom_concat([~ciaodesrc, '/', Component,
	'/version/GlobalPatch']).

patch(Component, Patch) :-
	no_tr_nl(~file_to_string(~patchfileabs(Component)),PS),
	atom_codes(Patch,PS).

componentversion(Component) := ~atom_concat([~vers(Component), '.',
	~patch(Component)]).

versionmain(Component) := ~atom_concat([Component, '-', ~vers(Component)]).

vpmain(Component) := ~atom_concat([~versionmain(Component), '.',
	~patch(Component)]).

win32vpmain(Component) := atom_concat([~vpmain(Component), 'Win32']).

engine := ~atom_concat(['ciao-',V,'.',P])
       :- file_to_string(~versionfileabs(ciao),VS),
	  file_to_string(~patchfileabs(ciao),PS),
	  atom_codes(V,~no_tr_nl(VS)), 
	  atom_codes(P,~no_tr_nl(PS)).


% CiaoDE stuff

versionpack(Component) := ~atom_concat([~packname(Component), '-',
	~componentversion(Component)]).

ciaodeversionmain := ~versionmain(~wholesystem).
ciaodevpmain      := ~vpmain(~wholesystem).

% given a binary file name, return the default main source file
plsource(FileName, SourceFile) :-
	atom_concat([~srcbindir, '/', BaseFile, '-', ~vers, ~get_ciao_ext],
	    FileName),
	atom_concat(BaseFile, '.pl', SourceFile).

version(Component) := ~atom_concat([~versionpack(Component), '-',
	~svn_revision_atom]).
versionpure(Component) := ~atom_concat([~componentversion(Component), '-',
	~svn_revision_atom]).

svn_revision_atom(A) :-
	ciaode_revision_string(S),
	atom_codes(A,S).

svn_revision_string(Path, String) :-
	no_tr_nl(~stream_to_string(~popen(~atom_concat([
	    'which svnversion > /dev/null 2>&1 && svnversion ', Path]),
	    read)), String0),
	(
	    String0 == "" ->
	    String = "exported"
	;
	    String = String0
	).

:- multifile component_description/4.

wholesystem(Name) :-
	component_description(Name, _, whole, _).

ciaodesrc        := ~component_description(ciaode, _, _).
ciaodebase       := ~packname(~wholesystem).

packname(Name, Pack) :-
	component_description(Name,  Pack, _Type, _Path).

basiccomponent(Name) :-
	component_description(Name, _Pack, basic, _Path).
extracomponent(Name) :-
	component_description(Name, _Pack, extra, _Path).
component(Name) :-
	component_description(Name, _Pack, Type, _Path),
	Type \== whole.
