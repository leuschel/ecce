/* locks.pl -- A Prolog interface to block files using C	*/
/* AFSID           : $__Header$					*/
/* Author          : Jose Manuel Gomez Perez			*/
/* Created On      : Wed Feb 11 11:11:03 1998			*/

#include "common_headers.h"
#include <sys/file.h>
#include <sys/fcntl.h>

#define   LOCK_SH   1    /* shared lock */
#define   LOCK_EX   2    /* exclusive lock */
#define   LOCK_NB   4    /* don't block when locking */
#define   LOCK_UN   8    /* unlock */


BOOL prolog_lock_file(Arg)
     Argdecl;
{
  int fd, res;

  DEREF(X(0), X(0));
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0),1,STRICT_ATOM)

  DEREF(X(1), X(1));
  if (!IsVar(X(1)))
    BUILTIN_ERROR(TYPE_ERROR(VARIABLE), X(1),2)

  fd=open(GetString(X(0)), O_RDWR);
  if (fd != -1)
    res=flock(fd, LOCK_EX);
  else
    res=-1;
  
  cunify(Arg, MakeSmall(fd), X(1));

  return(cunify(Arg, (res == 0 ? atom_true : atom_fail), X(2)));
}

BOOL prolog_unlock_file(Arg)
     Argdecl;
{
  int fd, res;

  DEREF(X(0), X(0));
  if (!TagIsSmall(X(0)))
    ERROR_IN_ARG(X(0),1,INTEGER)

  res=flock(fd, LOCK_UN);
  if (close(fd) == -1) res=-1;

  return(cunify(Arg, (res = 0 ? atom_true : atom_fail), X(1)));
}

void file_locks_init(module)
     char *module;
{
  define_c_mod_predicate(module, "lock_file", prolog_lock_file, 3);
  define_c_mod_predicate(module, "unlock_file", prolog_unlock_file, 2);
}


void file_locks_end(module)
     char *module;
{
  undefine_c_mod_predicate(module, "lock_file", 3);
  undefine_c_mod_predicate(module, "unlock_file", 2);
}
