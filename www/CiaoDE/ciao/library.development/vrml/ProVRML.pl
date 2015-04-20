:- module(VRML,[main/0],
	       [assertions,
		basicmodes,
		regtypes]).

:- use_module(library(basicprops)).


:- comment(title,"ProVRML a Prolog interface for VRML").

:- comment(subtitle,"@bf{The CIAO System Documentation Series}").
:- comment(subtitle,"Technical Report CLIP XXXXXX").
:- comment(subtitle,"@em{Draft printed on:} @today{}").

:- comment(author, "G@..{o}ran Smedb@..{a}ck").
:- comment(author, "@tt{clip@@dia.fi.upm.es}").
:- comment(author, "@tt{http://www.clip.dia.fi.upm.es/}").
:- comment(author, "The CLIP Group").
:- comment(author, "Facultad de Inform@'{a}tica").
:- comment(author, "Universidad Polit@'{e}cnica de Madrid").

:- comment(copyright,"
Copyright @copyright{} 1997-2002 The Clip Group.

@include{Copyright.Manuals}
").

:- comment(summary,"@apl{ProVRML} is Prolog library to handle VRML code. An 
interface to produce Prolog terms from VRML code and generate VRML code 
from terms.

").

%:- comment(module,"@include{ProVRML_intro}").
 
main.
