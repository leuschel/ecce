%:- module(name_server, [dyn_mod_addr/2, add_address/2]).
:- dynamic dyn_mod_addr/2.

add_address(Module, Address) :-
        retractall(dyn_mod_addr(Module,_)),
        assert(dyn_mod_addr(Module, Address)).

:- save_active_module(act, Address, (write(Address),nl,flush_output)).
