#include <sys/param.h>

#include "datadefs.h"


#if defined(DEBUG)
int debug_c;
#endif

#if !defined(MAXPATHLEN)
# define MAXPATHLEN 1024
#endif

char *pathname=NULL;
/*char *emulator_path; -- Unused now. DCG */ /* absolute path name -- Shared */
char source_path[MAXPATHLEN] = "";                             /* Shared */
int prolog_force_interactive = 0;      /* Shared --- not really relevant? */

char *library_directory = NULL;

#if defined(__svr4__) || defined(DARWIN)             /* Solaris or DARWIN */
#include <unistd.h>                                            /* sbrk () */
#include <stdlib.h>                                           /* malloc() */
#else                                                            /* SunOS */
#include <sys/types.h>
#include <malloc.h>
#endif

#if defined(MALLOC_DEBUG)
#include "dmalloc.h"
#endif

#if !defined(X_OK)
# define X_OK 1
#endif
