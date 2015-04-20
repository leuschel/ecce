% ----------------------------------------------------------------------
%
% Blackboard for Distributed Ciao, for Ciao
% Based on library file linda/server.pl of SICStus 2.1
% (C) UPM-CLIP 1997
%
% ----------------------------------------------------------------------

:- use_module(builtin).
:- use_module(library(sockets)).

% Uses from Library dummy, lists, setof, intrinsics1_attr intrinsics2, read

:- dynamic linda_trace/0.

% Tq = Th-Tt : queue of tuples
% Wq = Wh-Wt : queue of waiting streams

main([Port]) :- bind_socket(Port, 5, Socket).
