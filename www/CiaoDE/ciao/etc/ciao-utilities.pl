
:- use_package(assertions).

:- comment(filetype, part).

:- comment(title,"PART X - Miscellaneous standalone utilities").

:- comment(subtitle,"@bf{The Ciao System Documentation Series}").
:- comment(subtitle,"Technical Report CLIP X/98.1").
:- comment(subtitle,"@em{Draft printed on:} @today{}").

:- include(library('ClipAddress')).
:- include(library('Copyright')).

:- comment(summary,"@include{ciao-utilities.lpdoc}").

:- comment(module,"@include{ciao-utilities.lpdoc}").

%% This is a dummy definition of main to force documenter to produce 
%% application-type documentation (rather than library-type).

main.
