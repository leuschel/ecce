:- module(sockets_c,
        [connect_to_socket_type/4,
         bind_socket/3,
         socket_accept/2,
         select_socket/5,
         socket_send/2,
         socket_recv_code/3,
         socket_shutdown/2,
%         socket_buffering/4,
         hostname_address/2],
         [assertions]).

:- impl_defined(
        [connect_to_socket_type/4,
         bind_socket/3,
         socket_accept/2,
         select_socket/5,
         socket_send/2,
         socket_recv_code/3,
         socket_shutdown/2,
%         socket_buffering/4,
         hostname_address/2]).


:- comment(module, "Everything is documented in the module sockets.").
