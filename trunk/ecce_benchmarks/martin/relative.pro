
/* LAM TEST PROGRAM   */
/* RELATIVE PROGRAM   */
 
relative:relative(X,Y) :- relative:ancestor(Z,X), relative:ancestor(Z,Y).
 
relative:ancestor(X,Y) :- relative:parent(X,Y).
relative:ancestor(X,Y) :- relative:parent(X,Z),relative:ancestor(Z,Y).
 
relative:parent(X,Y) :- relative:father(X,Y).
relative:parent(X,Y) :- relative:mother(X,Y).
 
relative:father(jap,carol).
relative:father(jap,jonas).
relative:father(jonas,maria).
relative:mother(carol,paulina).
relative:mother(carol,albertina).
relative:mother(albertina,peter).
relative:mother(maria,mary).
relative:mother(maria,jose).
relative:mother(mary,anna).
relative:mother(mary,john).
 
