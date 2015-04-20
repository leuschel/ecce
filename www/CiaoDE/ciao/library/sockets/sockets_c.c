/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "common_headers.h"
#if defined(DARWIN) || defined(LINUX)
#include <string.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>


#include <stdio.h>
void perror(const char *s);

#include <errno.h>
/*int errno;*/


/* Local atoms */

TAGGED atom_stream;
TAGGED atom_dgram;
TAGGED atom_raw;
TAGGED atom_seqpacket;
TAGGED atom_rdm;

TAGGED atom_read;
TAGGED atom_write;
TAGGED atom_read_write;


/*
TAGGED atom_buff;
TAGGED atom_unbuff;
*/

struct stream_node *new_socket_stream(streamname, socket)
     TAGGED streamname;
     int socket;
{
  REGISTER struct stream_node *s;

  s = (struct stream_node *)checkalloc(sizeof(struct stream_node));
  s->streamname = streamname;
  s->streammode = 's';
  s->label = MakeSmall(socket);
  s->streamfile = NULL;
  s->isatty = FALSE;
  s->socket_eof = FALSE;
  /*  s->socket_is_unbuffered = 0; */
  s->last_nl_pos = 0;
  s->nl_count = 0;
  s->char_count = 0;
  s->pending_char = -100;

  return insert_new_stream(s);

  /*
  s->forward = root_stream_ptr,
  s->backward = root_stream_ptr->backward,
  root_stream_ptr->backward->forward = s,
  root_stream_ptr->backward = s;

  return s;
  */
}

/* connect_to_socket(+Host, +Port, +Type, -Stream) */

#define MAX_SOCK_NUMBER 65535

BOOL prolog_connect_to_socket(Arg)
     Argdecl;
{

  int sock;
  int port_number;
  TAGGED host_deref;
  struct hostent *host;
  struct sockaddr_in server_inet_addr;
  int    socket_type;
  TAGGED socket_atm;
  char   socket_name[512];

  DEREF(host_deref, X(0));
  if (!TagIsATM(host_deref))
    USAGE_FAULT("connect_to_socket_type/[3,4]: 1st argument must be an atom");

  DEREF(X(1), X(1));
  if (!TagIsSmall(X(1)))
    USAGE_FAULT("connect_to_socket_type/[3,4]: 2nd argument must be a port number");

  if ((port_number = GetInteger(X(1))) > MAX_SOCK_NUMBER)
    USAGE_FAULT("connect_to_socket/[3,4]: port number greater than 65535");
  
  DEREF(socket_atm, X(2));
  if (!TagIsATM(socket_atm))
    USAGE_FAULT("connect_to_socket_type/[3,4]: 3rd argument must be an atom");

  if (socket_atm == atom_stream)
    socket_type = SOCK_STREAM;
  else if (socket_atm == atom_dgram)
    socket_type = SOCK_DGRAM;
  else if (socket_atm == atom_raw)
    socket_type = SOCK_RAW;
  else if (socket_atm == atom_seqpacket)
    socket_type = SOCK_SEQPACKET;
  else if (socket_atm == atom_rdm)
    socket_type = SOCK_RDM;
  else USAGE_FAULT("connect_to_socket_type/[3,4]: unrecognized connection type");


  if ((host = gethostbyname(GetString(host_deref))) == NULL)
    MAJOR_FAULT("connect_to_socket/[3,4]: gethostbyname() failed");

  if ((sock = socket(AF_INET, socket_type, 0)) < 0)
    MAJOR_FAULT("connect_to_socket/[3,4]: socket creation failed");

 /* Specify that we may want to reuse the address  */

  /*
  if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, NULL, 0))
    MAJOR_FAULT("connect_to_socket/[3,4]: error setting option");
  */

  server_inet_addr.sin_family = AF_INET;
  memcpy((char *)&(server_inet_addr.sin_addr), 
         (char *)host->h_addr, 
         host->h_length);

  server_inet_addr.sin_port = htons(port_number);
    
  if (connect(sock, 
              (struct sockaddr *)&server_inet_addr, 
              sizeof(struct sockaddr)) < 0){
    perror("connect() in prolog_connect_to_socket");
    MAJOR_FAULT("connect_to_socket_type/[3,4]: cannot connect()");
  }
  
  sprintf(socket_name, "<%s:%d>", GetString(host_deref), port_number);
  
  return
    cunify(Arg, 
           ptr_to_stream(Arg,new_socket_stream(MakeString(socket_name),sock)),
           X(3)
           );
}


/* bind_socket(?Port, +Lenght, -Sock) */

BOOL prolog_bind_socket(Arg)
     Argdecl;
{
  int sock, port;
  struct sockaddr_in sa;
  int reuse_address = 1;

  DEREF(X(2), X(2));
  if (!IsVar(X(2)))
    USAGE_FAULT("bind_socket: 3rd argument must be a variable");

  DEREF(X(1), X(1));
  if (!TagIsSmall(X(1)))
    USAGE_FAULT("bind_socket: 2nd argument must be a (small) number");

  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    MAJOR_FAULT("bind_socket/3: socket creation failed");

  DEREF(X(0), X(0));
  if (IsVar(X(0)))
    sa.sin_port = 0;                      /* Undocumented in the manuals! */
  else {
    if (!TagIsSmall(X(0))) return FALSE;
    sa.sin_port = htons(GetSmall(X(0)));
  }
  sa.sin_family = AF_INET;
  sa.sin_addr.s_addr = INADDR_ANY;

 /* Specify that we may want to reuse the address  */

  if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, 
                 (void *)&reuse_address, sizeof(int)))
    MAJOR_FAULT("connect_to_socket/[3,4]: error setting option");

  if (bind(sock, (struct sockaddr *)&sa, sizeof(sa)) < 0)
    MAJOR_FAULT("bind_socket: cannot bind");

  if (IsVar(X(0))){
    unsigned int length = sizeof(sa);

    if (getsockname(sock, (struct sockaddr *)&sa, &length) < 0)
      MAJOR_FAULT("bind_socket: cannot get socket name");
    port = ntohs(sa.sin_port);
    if (!cunify(Arg, MakeSmall(port), X(0))) return FALSE;
  }

  listen(sock, GetSmall(X(1)));

  return cunify(Arg,MakeSmall(sock), X(2));
}


/* socket_accept(+Sock, -Stream) */

BOOL prolog_socket_accept(Arg)
     Argdecl;
{
  int socket, new_s;
  struct sockaddr isa;
  unsigned int isalen;
  char new_s_name[16];
  
  DEREF(X(0), X(0));
  if (!TagIsSmall(X(0)))
    USAGE_FAULT("socket_accept: 1st argument must be an integer");
  socket = GetSmall(X(0));

  isalen = sizeof(struct sockaddr);
  new_s = accept(socket, &isa, &isalen);

  sprintf(new_s_name, "<socket %d>", new_s);

  return
    cunify(Arg, 
           ptr_to_stream(Arg, new_socket_stream(MakeString(new_s_name),new_s)),
           X(1));
}


/* Aux function */

static TAGGED stream_list(Arg, max_fd, ready_set)
     Argdecl;
     int max_fd;
     fd_set *ready_set;
{
  REGISTER int this_fd;
  REGISTER TAGGED list = atom_nil;
  REGISTER struct stream_node *temp_stream;
  
  for (this_fd = max_fd; this_fd >= 0; this_fd--)	
    if (FD_ISSET(this_fd, ready_set)){  /* stream for every fd in the set */
      temp_stream = root_stream_ptr->forward;
      while (
             (GetSmall(temp_stream->label) != this_fd) && 
             (temp_stream != root_stream_ptr)
             )
        temp_stream = temp_stream->forward;
      if (temp_stream != root_stream_ptr)
        MakeLST(list, ptr_to_stream(Arg,temp_stream), list);
    }
  return list;
}


/*
 * select_socket(+Socket, -NewStream, +TO_ms, +Streams, -ReadStreams)
 */

BOOL prolog_select_socket(Arg)
     Argdecl;
{
  int 
    listen_sock = 0,  /* Avoid compiler complaints */
    max_fd = 0;
  struct timeval timeout, *timeoutptr;
  fd_set ready;
  BOOL unify_result = TRUE, watch_connections;
  TAGGED car, cdr;
  struct stream_node *stream, *socket_stream;
  char new_s_name[16];
  int newsock, fd_to_include;
  
  DEREF(X(0), X(0));           /* Do we have to wait for new connections? */
  watch_connections = IsInteger(X(0));
  
/* Construct the set of descriptors to watch for. Include socket, if
   specified.  */

  FD_ZERO(&ready);
  if (watch_connections) {
    max_fd = listen_sock = GetInteger(X(0));
    FD_SET(max_fd, &ready);
  }

  DEREF(X(2), X(2));                                   /* Get the timeout */
  if (X(2) == atom_off)
    timeoutptr = NULL;
  else if (IsInteger(X(2))){
    ENG_INT miliseconds_i = GetInteger(X(2));
    timeout.tv_sec = miliseconds_i / 1000;
    timeout.tv_usec = (miliseconds_i - timeout.tv_sec * 1000) * 1000;
    timeoutptr = &timeout;
  } else if (IsFloat(X(2))){
    ENG_FLT miliseconds_f = GetFloat(X(2));
    timeout.tv_sec = (int)(miliseconds_f / 1000);
    timeout.tv_usec = (int)(miliseconds_f - timeout.tv_sec * 1000) * 1000;
    timeoutptr = &timeout;
  } else
    USAGE_FAULT("select_socket/5: 3rd argument must be either \"off\" or a number");
  
  if ((timeoutptr != NULL) && ((timeout.tv_sec < 0) || (timeout.tv_usec < 0)))
    USAGE_FAULT("select_socket/5: timeout must be non-negative");

  DEREF(X(3), X(3));                      /* Get list of streams to watch */
  if (!TagIsLST(X(3)) && !(atom_nil == X(3)))
    USAGE_FAULT("select_socket/5: 4th argument not a list");
  
  DEREF(cdr,X(3));
  while (cdr!=atom_nil) {
    DerefCar(car,cdr);
    DerefCdr(cdr,cdr);
	
    if (!(stream = stream_to_ptr(car, 'r')))          /* Check any stream */
      USAGE_FAULT("select_socket/5: illegal stream (or stream mode) in list");

    fd_to_include = GetSmall(stream->label);
    if (fd_to_include < 0 || fd_to_include > FD_SETSIZE)
      USAGE_FAULT("select_socket/5: illegal stream in list or wrong file descriptor in stream");
	
    FD_SET(fd_to_include, &ready);
    if (fd_to_include > max_fd)  max_fd = fd_to_include;
  }
  
  if (select(max_fd+1, &ready, (fd_set *)NULL, (fd_set *)NULL, timeoutptr) < 0)
    MAJOR_FAULT("select_socket/5: select() call failed");

  if (watch_connections && FD_ISSET(listen_sock, &ready)) {

    if ((newsock = accept(listen_sock, NULL, 0)) < 0)
      MAJOR_FAULT("select_socket/5: accept() call failed");
    
    sprintf(new_s_name, "<socket %d>", newsock);
    socket_stream = new_socket_stream(MakeString(new_s_name), newsock);
    unify_result = cunify(Arg, ptr_to_stream(Arg, socket_stream), X(1));
    FD_CLR(listen_sock,&ready);
  }
  
  return unify_result && cunify(Arg,X(4), stream_list(Arg, max_fd, &ready));
}


/* socket_send(+Stream, +String). Needs socket in connected state. String is
   not supposed to be NULL terminated, since it is a Prolog string.  If a
   NULL terminated string is needed at the other side, it has to be
   explicitly created in Prolog. */

BOOL prolog_socket_send(Arg)
     Argdecl;
{
  struct stream_node *s;
  unsigned char *buffpt;
  int error_code, msglen;
  TAGGED cdr, car;

  s = stream_to_ptr_check(X(0), 'w', &error_code);
  if (!s)
    BUILTIN_ERROR(error_code, X(0), 1);
  if (s->streammode != 's')
    USAGE_FAULT("socket_send/2: first argument must be a socket stream");

  DEREF(X(1), X(1));
  cdr = X(1);
  buffpt = (unsigned char *)Atom_Buffer;

  for (msglen=0; cdr!=atom_nil; msglen++) {
    if (IsVar(cdr))
      BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,2)
    else if (!TagIsLST(cdr))
      BUILTIN_ERROR(TYPE_ERROR(CHARACTER_CODE_LIST),X(1),2)
    else if (msglen == Atom_Buffer_Length) {                   /* realloc */
      Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer,
                                         msglen, Atom_Buffer_Length<<=1);
      buffpt = (unsigned char *)Atom_Buffer+msglen;
    }
    DerefCar(car,cdr);
    if (IsVar(car))
      BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,2)

    if (!TagIsSmall(car) || (car<TaggedZero) || (car>=MakeSmall(256)))
      BUILTIN_ERROR(TYPE_ERROR(CHARACTER_CODE_LIST),X(1),2)
    
    *buffpt++ = GetSmall(car);
    DerefCdr(cdr,cdr);
  }

  /* Do not realloc here -- no need to add a NULL char at the end */
  /*
  if (i == Atom_Buffer_Length){
    Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer,
                                       i, Atom_Buffer_Length<<=1);
    buffpt = (unsigned char *)Atom_Buffer+i;
  }
  */

  if (send(GetSmall(s->label), Atom_Buffer, msglen, 0) < 0)
    MAJOR_FAULT("socket_send/2: send() call failed")  /* Give error, fail */

  return TRUE;
}



/* socket_recv(+Stream, ?String, ?Length).  Needs socket in connected state. */

#define BUFFSIZE 2049

BOOL prolog_socket_receive(Arg)
     Argdecl;
{
  struct stream_node *s;
  /* We could use Atom_Buffer, but its size changes during execution; we'd
     better keep everything under control. */
  unsigned char *buffpt, buffer[BUFFSIZE];
  int error_code, bytes_read, total_bytes;
  TAGGED cdr;

  s = stream_to_ptr_check(X(0), 'r', &error_code);
  if (!s) BUILTIN_ERROR(error_code, X(0), 1);

  if (s->streammode != 's')
    USAGE_FAULT("socket_recv/2: first argument must be a socket stream");
  
  /* recv is about to disappear (c.f. Linux' manpage), so we'll use
     recvfrom instead */

  /* bytes_read = recv(GetSmall(s->label), buffer, BUFFSIZE-1, 0); */

  bytes_read = recvfrom(GetSmall(s->label), buffer, BUFFSIZE-1, 0, NULL, NULL);
  total_bytes = bytes_read;

  if (bytes_read < 0)
    MAJOR_FAULT("socket_recv/2: recv() call failed")

  /*
    else if (bytes_read > BUFFSIZE-1)        
    MAJOR_FAULT("socket_recv/2: internal buffer overrun")
  */

  if (HeapDifference(w->global_top, Heap_End) < CONTPAD+(bytes_read<<1))
      explicit_heap_overflow(Arg, CONTPAD+(bytes_read<<1),2);
    
  buffpt = &buffer[bytes_read-1];
  cdr = atom_nil;
  while (bytes_read>0)	{
    bytes_read--;
    MakeLST(cdr,MakeSmall(*(buffpt--)), cdr);/* No need to cast *(buffpt--) */
  }

  return cunify(Arg,cdr,X(1)) && cunify(Arg, MakeSmall(total_bytes), X(2));
}


/* socket_shutdown(+Stream, +Type) */

/* Patch constants apparently not defined in all Linux implementations (at
   least at the moment) */

#if !defined(SHUT_RD)
#define SHUT_RD    0
#define SHUT_WR    1
#define SHUT_RDWR  2
#endif

BOOL prolog_socket_shutdown(Arg)
     Argdecl;
{
    struct stream_node *s;
    int access_required;
    TAGGED shutdown_type;
    int error_code;
    int how;
    
    DEREF(X(0), X(0));
    DEREF(shutdown_type, X(1));

    if (shutdown_type == atom_read) {
      access_required = 'r';
      how = SHUT_RD;
    } else if (shutdown_type == atom_write){
      access_required = 'w';
      how = SHUT_WR;
    } else if (shutdown_type == atom_read_write) {
      access_required = 'w';
      how = SHUT_RDWR;
    }      
    else USAGE_FAULT("socket_shutdown/2: error in second argument");

    s = stream_to_ptr_check(X(0), access_required, &error_code);
    if (!s) BUILTIN_ERROR(error_code, X(0), 1);

    if (s->streammode != 's')
    USAGE_FAULT("socket_shutdown/2: first argument must be a socket stream");
    
    if ((error_code = shutdown(GetSmall(s->label), how)))
      MAJOR_FAULT("socket_shutdown/2: error in call to shutdown()");

    return TRUE;
}


/* socket_buffering(+Stream, +Direction, -OldBuf, +NewBuffer) */
/*
BOOL prolog_socket_buffering(Arg)
     Argdecl;
{
    struct stream_node *s;
    int access_required;
    int error_code;
    TAGGED direction;
    TAGGED oldbuf;
    TAGGED newbuf;
    int flag;
    
    DEREF(direction, X(1));

    if (direction == atom_read) {
      access_required = 'r';
    } else if (direction == atom_write){
      access_required = 'w';
    } else USAGE_FAULT("socket_buffering/4: error in second argument");

    DEREF(X(0), X(0));
    s = stream_to_ptr_check(X(0), access_required, &error_code);
    if (!s) BUILTIN_ERROR(error_code, X(0), 1);

    if (s->streammode != 's')
    USAGE_FAULT("socket_buffering/4: first argument must be a socket stream");

    oldbuf = (s->socket_is_unbuffered ? atom_unbuff : atom_buff);

    DEREF(newbuf, X(3));
    if (newbuf == atom_unbuff)
      s->socket_is_unbuffered = 1;
    else if (newbuf == atom_buff)
      s->socket_is_unbuffered = 0;
    else USAGE_FAULT("socket_buffering/4: error in fourth argument");
    
    flag = s->socket_is_unbuffered;

    if (setsockopt(GetSmall(s->label),
                   IPPROTO_TCP,
                   TCP_NODELAY, 
                   (void *)&flag,
                   sizeof(int)))
      MAJOR_FAULT("socket_buffering/4: error setting option");

    return cunify(Arg, X(2), oldbuf);
}
*/

/* hostname_address(+Hostname, ?Address) */

#define MAX_BYTES_IN_HOST_ADDRESS 8             /* It is 4 at the present */

BOOL prolog_hostname_address(Arg)
     Argdecl;
{
  TAGGED hostname;
  /* 3 chars per byte plus dots plus trailing zero */
  char address[4*MAX_BYTES_IN_HOST_ADDRESS];  
  int address_index = 0;
  int bytes_index = 0;
  struct hostent *host;

  DEREF(hostname, X(0));
  if (!TagIsATM(hostname))
    USAGE_FAULT("hostname_address/2: 1st argument must be an atom");

  if ((host = gethostbyname(GetString(hostname))) == NULL)
    MAJOR_FAULT("hostname_address/2: gethostbyname() failed");

  while(bytes_index < host->h_length) {
    sprintf(&address[address_index], 
           "%u.", 
           (unsigned char)(host->h_addr_list[0][bytes_index]));
    while(address[address_index])
      address_index++;
    bytes_index++;
  }
  address[--address_index] = 0;

  return cunify(Arg, X(1), MakeString(address));
}



void sockets_c_init(module)
     char *module;
{
  define_c_mod_predicate(module, "connect_to_socket_type", 4, prolog_connect_to_socket);
  define_c_mod_predicate(module, "bind_socket", 3, prolog_bind_socket);
  define_c_mod_predicate(module, "socket_accept", 2, prolog_socket_accept);
  define_c_mod_predicate(module, "select_socket", 5, prolog_select_socket);
  define_c_mod_predicate(module, "socket_send", 2, prolog_socket_send);
  define_c_mod_predicate(module, "socket_recv_code", 3, prolog_socket_receive);
  define_c_mod_predicate(module, "socket_shutdown", 2, prolog_socket_shutdown);
  /*
  define_c_mod_predicate(module, 
                         "socket_buffering",
                         prolog_socket_buffering, 
                         4);
  */
  define_c_mod_predicate(module ,"hostname_address",2,prolog_hostname_address);

  atom_stream = init_atom_check("stream");
  atom_dgram = init_atom_check("dgram");
  atom_raw = init_atom_check("raw");
  atom_seqpacket = init_atom_check("seqpacket");
  atom_rdm = init_atom_check("rdm");

  atom_read = init_atom_check("read");
  atom_write = init_atom_check("write");
  atom_read_write = init_atom_check("read_write");
  /*
  atom_buff = init_atom_check("fullbuf");
  atom_unbuff = init_atom_check("unbuf");
  */
}


void sockets_c_end(module)
     char *module;
{
  undefine_c_mod_predicate(module, "connect_to_socket_type", 4);
  undefine_c_mod_predicate(module, "bind_socket", 3);
  undefine_c_mod_predicate(module, "socket_accept", 2);
  undefine_c_mod_predicate(module, "select_socket", 5);
  undefine_c_mod_predicate(module, "socket_send", 2);
  undefine_c_mod_predicate(module, "socket_recv_code", 3);
  undefine_c_mod_predicate(module, "socket_shutdown", 2);
  /*
  undefine_c_mod_predicate(module, "socket_buffering", 4);
  */
}
