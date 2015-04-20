:- module(example7,_,[fuzzy]).

tall(john,0.8):~ .
fast(john,0.7):~ .

good_player(X,_):~ prod
	tall(X,_),
        fast(X,_).
