% Provides minimal tcltk functionality

create_tcltk_server(PortId,Geometry) :-
	append("/usr/local/bin/wish -name CIAO_CANVAS -sync -geometry ", 
               Geometry, CommandS),
	atom_codes(Command,CommandS),
	unix(popen(Command,write,PortId)).

close_tcltk_server(PortId) :-
	port_send(PortId,"exit").
%	close(PortId).

port_send(PortId,X) :-
        atom_codes(A,X),
        display(PortId,A),
        nl(PortId),
	flush_output(PortId).
