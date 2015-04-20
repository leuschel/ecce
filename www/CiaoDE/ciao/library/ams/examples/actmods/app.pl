
:- use_package(ams).
:- ams_application(main).
:- active_module(active,'clip.dia.fi.upm.es',public).

:- use_module(active,[q/2]).

main([X]):- q(X,Y), display(Y), nl, fail.
main(_).
