:- use_module(library('ams/test/example/file'),[p/2]).

main([X]):- p(X,Y), display(Y), nl, fail.
main(_).
