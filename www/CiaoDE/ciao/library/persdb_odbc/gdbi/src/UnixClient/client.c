#include <stdio.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
 
/* ----------------------------------------------------------------------------
*/
 
#define PORT            2020      /* server default port */
#define SERVERHOST      "silver"  /* default server name */

char* file_name = "config.txt";   /* name of configuration file for server name and port */
char host_name[200];              /* name of server host */
int port;                         /* port in server to connect to */
 
int sock = -1;                    /* socket used in connection to server */
 
/* ----------------------------------------------------------------------------
*/
 
static char* get_host_name();
static int get_port();
static int init_sockaddr (struct sockaddr_in *name, const char *hostname,
                          unsigned short int port);
int write_to_server (char* msg);
int init_connection();
int terminate_client();
int get_server_answer();
int handle_request(char* str);
 
/* ----------------------------------------------------------------------------
*/
 
char* get_host_name()
{
  FILE* fp = fopen(file_name, "r");
  if (fp == NULL) {
    strcpy(host_name, SERVERHOST);
  } else {
    fscanf(fp,"%s %d", host_name, &port);
  }
  fclose(fp);
  return host_name;
}
 
int get_port()
{
  static char tmp_host_name[200];
  FILE* fp = fopen(file_name, "r");
  if (fp == NULL) {
    port = PORT;
  } else {
    fscanf(fp,"%s %d", tmp_host_name, &port);
  }
  fclose(fp);
  return port;
}
 
/* ----------------------------------------------------------------------------
*/
/* filling in a sockaddr_in structure, given a host name string
   and a port number. */
 
int init_sockaddr (struct sockaddr_in *name,
                   const char *hostname, unsigned short int port)
{
  struct hostent *hostinfo;
 
  name->sin_family = AF_INET;
  name->sin_port = htons (port);
  hostinfo = gethostbyname (hostname);
  if (hostinfo == NULL) {
    fprintf (stderr, "Unknown host %s.\n", hostname);
    return -1;
  }
  name->sin_addr = *(struct in_addr *) hostinfo->h_addr;
  return 0;
}
 
/* ----------------------------------------------------------------------------
*/
 
int write_to_server (char* msg)
{
  int nbytes;
 
  if (sock < 0)  return -1;
 
  nbytes = write (sock, msg, strlen (msg) + 1);
  if (nbytes < 0) {
    perror ("write");
    return -1;
  }
  return 0;
}
 
/* ----------------------------------------------------------------------------
*/
 
int init_connection()
{
  struct sockaddr_in servername;
  /* Create the socket. */
  sock = socket (PF_INET, SOCK_STREAM, 0);
 
  if (sock < 0) {
    perror ("socket (client)");
    return -1;
  }
  /* Connect to the server. */
  if (init_sockaddr (&servername, get_host_name(), get_port()) < 0) {
    return -1;
  }
  if (0 > connect (sock,
                   (struct sockaddr *) &servername,
                   sizeof (servername))) {
    perror ("connect (client)");
    return -1;
  }
  return sock;
}
 
/* ----------------------------------------------------------------------------
*/
 
int terminate_client()
{
  if (sock >= 0) {
    close (sock);
    sock = -1;
  }
  return 0;
}
 
/* ----------------------------------------------------------------------------
*/
  
int get_server_answer()
{
  int nbytes;
  char AnswerBuf[2];

  /* Wait for an answer from the server */
  while (1) {
    recv (sock ,AnswerBuf ,1 ,0);
    if (AnswerBuf[0] == 0 ) {
      break;
    } else {
      putchar(AnswerBuf[0]);
    }
  }
  return 0;
}
 
/* ----------------------------------------------------------------------------
*/
 
int handle_request(char* str)
{
  if (sock < 0) return -1;
  if (write_to_server(str) < 0)
    return -1;
  return get_server_answer();
}
 
/* ----------------------------------------------------------------------------
*/
 
int main(int argc, char**argv)
{
  char buffer[1024];

  if (init_connection() == 1) {
    fprintf(stderr, "Failed to initialize client\n");
    return -1;
  }
  while (1) {
    printf("Enter you message : ");
    gets(buffer);
    if (strcmp(buffer,".") == 0) {
      break;
    }
    handle_request(buffer);
		printf("\n");
  }
  terminate_client();
  return 0;
}
