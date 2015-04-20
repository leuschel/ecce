:- module(fd_tr, [fd_tr/2], ['fd/fd_syntax']).
:- include(library('fd/fd_translation')).

fd_tr((A .=. B), parse(A .=. B)).
fd_tr((A .>. B), parse(A .>. B)).
fd_tr((A .<. B), parse(A .<. B)).
fd_tr((A .=<. B), parse(A .=<. B)).
fd_tr((A .>=. B), parse(A .>=. B)).
fd_tr((A .<>. B), parse(A .<>. B)).
