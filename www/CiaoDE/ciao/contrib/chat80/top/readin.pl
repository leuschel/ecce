
:- module(readin,[ read_in/1, sentences/3 ],[ dcg ]).
/* Read a sentence */

/*
 :- mode initread(-).
 :- mode readrest(+,-).
 :- mode word(-,?,?).
 :- mode words(-,?,?).
 :- mode alphanum(+,-).
 :- mode alphanums(-,?,?).
 :- mode digits(-,?,?).
 :- mode digit(+).
 :- mode lc(+,-).
*/

/* Read sentence */
read_in(P):-initread(L),words(P,L,[]),!,to_nl.

initread([K1,K2|U]):-get(K1),get0(K2),readrest(K2,U).

% Changed to standard notation M.H.
readrest(K,[]):- terminator(K),!.
readrest(K,[K1|U]):-K=<32,!,get(K1),readrest(K1,U).
readrest(_K1,[K2|U]):-get0(K2),readrest(K2,U).

% Added sentences M.H.
sentences([]) --> [].
sentences([V|U]) --> words(V),!,blanks,sentences(U),!.

% Added blanks before first word M.H.
words([V]) --> term_char(V1), !, {name(V,[V1])}, blanks.
words([V|U]) --> blanks,word(V),!,blanks,words(U).
words([]) --> [].

word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

alphanum(95,95) :- !.
alphanum(K,K1):-lc(K,K1).
alphanum(K,K):-digit(K).

digits([K|U]) --> [K],{digit(K)},!,digits(U).
digits([]) --> [].

blanks--> [K],{K=<32},!,blanks.
%% Added carriage return and newline
blanks--> [K],{K=0'
},!,blanks.
blanks--> [K],{K=0'},!,blanks.
blanks --> [].

digit(K):-K>47,K<58.

lc(K,K1):-K>64,K<91,!,K1 is K\/8'40.
lc(K,K):-K>96,K<123.

% M.H.
term_char(K) --> [K],{terminator(K)}.
terminator(0'.).
terminator(0'?).
terminator(0'!).

to_nl :-
   repeat,
   get0(10), !.

get(C):- get1_code(C).

get0(C):- get_code(C).
