% ===========================================================================
:- module(_,_,[make, functions, assertions]).
% ===========================================================================
:- comment(title, "Ciao Distribution SETTINGS").
:- comment(author, "Edison Mera").
:- comment(module, "These are SETTINGS for building the distribution").
% ===========================================================================

:- reexport(ciaodesrc('dist/DISTSHARED')).

% This indicates the directories that are not distributed with the given 
% distributions.

unixnodist := '../Win32/bin/NODISTRIBUTE'.
win32nodist := '../engine/NODISTRIBUTE'.

% The site when the main documentation source resides
docsrc := '../doc/reference'.

% The configuration file used to install main documentation files
docset := '../../dist/common/WWWLPDOCSET'.

% The configuration file used to install distribution documentation files
distlpdocset := '../../dist/common/DISTLPDOCSET'.

% The place where the readme source files are.
readmesrc := '../doc/readmes'.

%-----------------------------------------------------------------------------
% END
