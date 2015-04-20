% Multifile sub-predicates are not renamed correctly??
%
%
% Execute:
%   ciaoc-1.11 -c multi_reload; ciaosh-1.11 -u multi_reload
% (and execute bar in the shell)
%
% The problem is that foo/0 uses disjuntions, which are translated
% to subpredicates

:- module(_, [bar/0], []).
:- multifile foo/0.
foo :- display('hello\n') ; true.
bar :- foo.
