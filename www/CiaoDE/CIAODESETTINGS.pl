:- module(_,_,[functions,make,assertions]).

:- reexport(ciaodesrc('CIAODESHARED')).

% *** This is here so that it can be seen by dist and doc
tardocformat :=   pdf | manl | info | infoindex.

% *** This is the default formats for the manuals
docformat := ps | pdf | manl | info | infoindex | html | htmlindex.
