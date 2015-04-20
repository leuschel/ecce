:- module(sockets,
        [connect_to_socket/3,
         socket_recv/2,
         socket_type/1,
         shutdown_type/1], 
         [assertions,isomodes,regtypes]).

:- reexport(library('sockets/sockets_c')).

:- comment(title, "The socket interface").

:- comment(author, "Manuel Carro").
:- comment(author, "Daniel Cabeza").

:- comment(module, "This module defines primitives to open sockets,
        send, and receive data from them.  This allows communicating
        with other processes, on the same machine or across the
        Internet. The reader should also consult standard bibliography
        on the topic for a proper use of these primitives.").

%% Socket types

:- comment(doinclude,connect_to_socket_type/4).
:- comment(doinclude,connect_to_socket/3).
:- comment(doinclude,bind_socket/3).
:- comment(doinclude,socket_accept/2).
:- comment(doinclude,select_socket/5).
:- comment(doinclude,socket_send/2).
:- comment(doinclude,socket_recv_code/3).
:- comment(doinclude,socket_recv/2).
:- comment(doinclude,socket_type/1).

%% :- type socket_type/1.

:- comment(socket_type/1,"Defines the atoms which can be used to
   specify the socket type recognized by
   @pred{connect_to_socket_type/4}. Defined as follows:
   @includedef{socket_type/1}").

:- regtype socket_type(T) # "@var{T} is a valid socket type.".

socket_type(stream).
socket_type(dgram).
socket_type(raw).
socket_type(seqpacket).
socket_type(rdm).


%% Automatically reexported

:- pred connect_to_socket_type(+Host, +Port, +Type, -Stream) ::
        atom * int * socket_type * stream
 # "Returns a @var{Stream} which connects to @var{Host}.  The @var{Type} of 
    connection can be defined.  A @var{Stream} is returned, which can be 
    used to @pred{write/2} to, to @pred{read/2}, to @pred{socket_send/2} to, 
    or to @pred{socket_recv/2} from the socket.".


:- pred connect_to_socket(+Host, +Port, -Stream) ::
        atm * int * stream
 # "Calls @pred{connect_to_socket_type/4} with SOCK_STREAM connection
    type.  This is the connection type you want in order to use the 
    @pred{write/2} and @pred{read/2} predicates (and other stream IO 
    related predicates).".

connect_to_socket(Host, Port, Stream):-
        connect_to_socket_type(Host, Port, stream, Stream).


%% Automatically reexported

:- pred bind_socket(?Port, +Length, -Socket) ::
        int * int * int
 # "Returs an AF_INET @var{Socket} bound to @var{Port} (which may be 
    assigned by the OS or defined by the caller), and listens to it 
    (hence no listen call in this set of primitives).
    @var{Length} specifies the maximum number of pending connections.".


%% Automatically reexported

:- pred socket_accept(+Sock, -Stream) ::
        int * stream
 # "Creates a new @var{Stream} connected to @var{Sock}.".


%% Automatically reexported

:- pred select_socket(+Socket, -NewStream, +TO_ms, +Streams, -ReadStreams) 
        :: int * stream * int * list(stream) * list(stream) #

"Wait for data available in a list of @var{Streams} and in a
@var{Socket}. @var{Streams} is a list of Prolog streams which will be
tested for reading.  @var{Socket} is a socket (i.e., an integer
denoting the O.S. port number) or a @concept{free variable}.
@var{TO_ms} is a number denoting a timeout.  Within this timeout the
@var{Streams} and the @var{Socket} are checked for the availability of
data to be read.  @var{ReadStreams} is the list of streams belonging
to @var{Streams} which have data pending to be read.  If @var{Socket}
was a free variable, it is ignored, and @var{NewStream} is not checked.
If @var{Socket} was instantiated to a port number and there are
connections pending, a connection is accepted and connected with the
Prolog stream in @var{NewStream}.".


%% Automatically reexported

:- pred socket_send(+Stream, +String) ::
        stream * string 
 # "Sends @var{String} to the socket associated to @var{Stream}. The socket 
    has to be in connected state. @var{String} is not supposed to be 
    NULL terminated, since it is a Prolog string.  If a NULL terminated 
    string is needed at the other side, it has to be explicitly created in 
    Prolog.".

%% Automatically reexported

:- pred socket_recv_code(+Stream, ?String, ?Length) ::
        stream * string * int 
 # "Receives a @var{String} from the socket associated to @var{Stream},
    and returns its @var{Length}.  If @var{Length} is -1, no more data is 
    available.".


:- pred socket_recv(+Stream, ?String) ::
        stream * string
 # "As @pred{socket_recv_code/3}, but the return code is ignored.".

socket_recv(Stream, String):- socket_recv_code(Stream, String, _).


:- pred socket_shutdown(+Stream, +How) :: stream * shutdown_type #
"Shut down a duplex communication socket with which @var{Stream} is
associated.  All or part of the communication can be shutdown,
depending on the value of @var{How}. The atoms @tt{read}, @tt{write},
or @var{read_write} should be used to denote the type of closing
required.".


:- regtype shutdown_type(T) # "@var{T} is a valid shutdown type.".

shutdown_type(read).
shutdown_type(write).
shutdown_type(read_write).

:- pred hostname_address(+Hostname, ?Address) :: atm * atm #
"@var{Address} is unified with the atom representing the address (in
AF_INET format) corresponding to @var{Hostname}.".

 %% :- pred socket_buffering(+Stream, +Direction, -OldBuf, +NewBuffer) :: 
 %%         stream * atm * atm * atm 
 %%  #"The buffering in @var{Direction} (@tt{read} or @{write}) is changed to be @var{NewBuffer} (@tt{unbuf} or @tt{fulbuf}).  For TCP/IP savvys, this just disables the Nagle algorithm in the TCP layer.".
