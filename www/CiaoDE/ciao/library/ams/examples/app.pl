
:- use_package(ams).
:- ams_application(main).
:- active_module(file,'clip.dia.fi.upm.es',private).

:- use_module(file,[p/2]).

main([X]):- p(X,Y), display(Y), nl, fail.
main(_).
