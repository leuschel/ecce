:- module(db_client_example, [main/0], [actmods]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Needed modules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_active_module(db_server_example, [main0/0, main1/0, main2/0]).
:- use_module(library('actmods/filebased_locate')).
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Defined predicates  %%%%%%%%%%%%%%%%%%%%%%%%

main:- 
	main2.
