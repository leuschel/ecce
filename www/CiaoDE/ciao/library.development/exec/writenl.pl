
 %% :- module(writenl, [writenl/1]).
 %% :- use_module(write).
 %% :- use_module(nl).

m_writenl(X):- m_write(X), m_nl.
