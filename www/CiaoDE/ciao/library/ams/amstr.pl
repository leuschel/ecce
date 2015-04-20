:- module(amstr, [amstr/2], []).

:- data seen/2.

amstr((:- active_module(Mod,Host,Mode)),[ (:- use_module(Mod,[shut_down/0])),
	                                  '$ams$actmod'(Mod,Mode,Host)
					]).
amstr((:- ams_application(App)), '$ams$app'(App)).
amstr((main(X):- B), ('$ams$main'(X):- B)):-
	asserta_fact(seen(main,1)).
amstr((main:- B), ('$ams$main@':- B)):-
	asserta_fact(seen(main,0)).
amstr((main(X)), ('$ams$main'(X))):-
	asserta_fact(seen(main,1)).
amstr((main), ('$ams$main@')):-
	asserta_fact(seen(main,0)).
amstr(end_of_file,[ (:- multifile '$ams$actmod'/3,'$ams$app'/1),
	            ('$ams$shut_down':- '$ams$actmod'(Mod,_,_),
		                        Mod:shut_down, fail),
	            '$ams$shut_down' | More ]):-
        ( seen(main,1)
	-> More = [ (main(X):- '$ams$main'(X), '$ams$shut_down'),
	            (main(_):- '$ams$shut_down', fail) | More1 ]
	 ; More = More1
	),
        ( seen(main,0)
	-> More1= [ (main:- '$ams$main@', '$ams$shut_down'),
	            (main:- '$ams$shut_down', fail) | More2 ]
	 ; More2= More1
	),
	More2 = end_of_file.
