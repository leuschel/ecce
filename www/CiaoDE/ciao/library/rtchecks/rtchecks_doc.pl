:- use_package([assertions,regtypes]).

:- comment(nodoc,assertions).
:- comment(nodoc,regtypes).

:- comment(filetype,package).

:- comment(title,"Run-time checking of assertions").

:- comment(author, "David Trallero Mena").

:- comment( module , 
"This library package can be used to perform run-time checking of
assertions. Properties are checked during execution of the program and
errors found (when the property does not hold) are reported."
).

:- use_module(library('assertions/assertions_props'),[property_conjunction/1]).

:- comment(check/1,"See @ref{The Ciao assertion package}.").
:- trust pred check( Prop ) : property_conjunction
   # "@var{Prop} is checked. If it fails, an exception is raised.".
:- impl_defined(check/1).
