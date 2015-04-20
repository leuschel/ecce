/* 
 *dynlink.c -- Load dynamic object code onto a running process
 *AFSID           : $__Header$
 *Author          : Manuel Carro & the CLIP group (C) CLIP 1996,1997,1998-1997
 *Created On      : Thu Aug 28 19:04:42 1997
 *Last Modified By: 
 *Last Modified On: 
 *Update Count    : 0
 *Status          : Unknown, Use with caution!
 */


#include "datadefs.h"
#include "support.h"

#if defined(FOREIGN_FILES)

#include <dlfcn.h>
#include <string.h>

#if defined(SunOS4)
#include <sys/param.h>
#endif

/* declarations for global functions accessed here */

#include "dynlink_defs.h"
#include "alloc_defs.h"
#include "own_malloc_defs.h"


/* local declarations */

static void unload_if_present PROTO((char *module_name));

static void add_to_loaded_objects PROTO((char *module_name,
					 void *handle,
					 void (*end_func)(char *)));


/* Length of filenames */

#if !defined(MAX_FILENAME)
# if defined(FILENAME_MAX)
# define MAX_FILENAME FILENAME_MAX
# elif defined(MAXPATHLEN)
# define MAX_FILENAME MAXPATHLEN
# endif
#endif

/* Dynamic linking options.  This is really OS dependant. */
/* And any other object can reference objects in the one we are loading */

#if defined(Solaris) || defined(IRIX)
# if defined(RTLD_PARENT)
#  define LIB_LOADING_TYPE RTLD_LAZY | RTLD_GLOBAL | RTLD_PARENT
# else
#  define LIB_LOADING_TYPE RTLD_LAZY | RTLD_GLOBAL
# endif
#elif defined(LINUX)
# define LIB_LOADING_TYPE RTLD_LAZY              /* No RTLD_GLOBAL in Linux */
#elif defined(SunOS4)
# define LIB_LOADING_TYPE 1                          /* SunOS man pages... */
#endif


/* We maintain a list of objects loaded, in order to free up space if they
  are reloaded. The object is identified by its module_name and accesses to
  by a handle */

typedef struct lobj {
  struct lobj *next;
  void *handle;
  char *module_name;
  void (*end_func)(char *);                                       /* JFMC */
} *loaded_object;

loaded_object all_loaded_objects = NULL;                        /* Shared */



/* dynlink(+File, +Module): dynamically load the File .so library, and set
  the Prolog-accesible predicates to be loaded inside Module. We delete it
  from the list of loaded objects, if needed, and then we add it. */

BOOL prolog_dynlink(Arg)
     Argdecl;
{
  char *lib_name;
  char *module_name;
  int strindx;
  char libfunction[STATICMAXATOM];
  void *lib_handle;
  void (*init_func)(char *);
  void (*end_func)(char *);
  char errmsg[1024];

  /* Extract the module name */

  DEREF(X(1), X(1));

  if (!TagIsATM(X(1)))
    USAGE_FAULT("dynlink/2: second argument must be an atom");
  module_name = (char *)GetString(X(1));

   /* Extract the string and *check* it is really a string */

  DEREF(X(0), X(0));
  if (!TagIsATM(X(0)))
    USAGE_FAULT("dynlink/2: first argument must be an atom");
  lib_name = (char *)GetString(X(0));

#if defined(DEBUG)
  if (debug_c) fprintf(stderr, "Linking %s\n", lib_name);
#endif


#if defined(USE_ATOM_LEN)
  if ((strindx = GetAtomLen(X(0))) > MAX_FILENAME)
    USAGE_FAULT("dynlink/2: full filename too long");
#else
  if ((strindx = strlen(lib_name)) > MAX_FILENAME)
    USAGE_FAULT("dynlink/2: full filename too long");
#endif

 /* If already loaded, unload it before. */

  unload_if_present(module_name);

  lib_handle = dlopen(lib_name, LIB_LOADING_TYPE); /* Open the .so library */
  if (lib_handle == NULL){
    sprintf(errmsg, "dynlink/2: could not load library\n %s)",
            dlerror());
    USAGE_FAULT(errmsg);
  }

  strcpy(libfunction, module_name);                 /* Construct the name */
  strcat(libfunction, "_init");               /* of the init function */

  /* Get the address of init_func */ 

  init_func = (void (*)(char *))dlsym(lib_handle, libfunction);
  if (init_func == NULL){
    dlclose(lib_handle);
    USAGE_FAULT("dynlink/2: could not find initialization function");
  }
  strcpy(libfunction, module_name);             /* Construct the name */
  strcat(libfunction, "_end");                 /* of the end function */

  /* JFMC: Get the address of the end_func */

  end_func = (void (*)(char *))dlsym(lib_handle, libfunction);     
  if (end_func == NULL) {
    dlclose(lib_handle);
    USAGE_FAULT("dynlink/2: could not find end function");
  }

  /* It is already in our address space. Add it to our list. */

  add_to_loaded_objects(module_name, lib_handle, end_func);

  /* Call the init function with the name of the module */

  (*init_func)(module_name);

  return TRUE;
}


/* dynunlink(+File): dynamically unload the File .so library */ /* JFMC */

BOOL prolog_dynunlink(Arg)
     Argdecl;
{
  char *module_name;

   /* Extract the string and *check* it is really a string */

  DEREF(X(0), X(0));
  if (!TagIsATM(X(0)))
    USAGE_FAULT("dynunlink/1: first argument must be an atom");
  module_name = (char *)GetString(X(0));

#if defined(DEBUG)
  if (debug_c) fprintf(stderr, "Unlinking %s\n", module_name);
#endif

 /* If loaded, unload it. */

  unload_if_present(module_name);

  return TRUE;
}

/* Find and remove from the process space the object associated to
   module_name */

void unload_if_present(module_name)
     char *module_name;
{
  loaded_object previous_object;
  loaded_object to_remove = NULL;
  BOOL found = FALSE;

#if defined(DEBUG)
  if (debug_c) fprintf(stderr, "Unloading %s\n",module_name);
#endif

  if (all_loaded_objects){                              
    if (!strcmp(all_loaded_objects->module_name,module_name)){/* First in list? */
      to_remove = all_loaded_objects;
      all_loaded_objects = all_loaded_objects->next;
    } else {                            /* Second or third, or fourth...  */
      to_remove = all_loaded_objects->next;
      previous_object = all_loaded_objects;
      while (!found && to_remove)
        if (!strcmp(to_remove->module_name, module_name)) {
          previous_object->next = to_remove->next;
          found = TRUE;
        } else {
          previous_object = to_remove;
          to_remove = to_remove->next;
        }
    }

    if (to_remove) {
#if defined(DEBUG)
      if (debug_c) fprintf(stderr, "Closing handle for %s\n",module_name);
#endif
      (*to_remove->end_func)(to_remove->module_name);     /* JFMC / MCL */
      dlclose(to_remove->handle);
      checkdealloc((TAGGED *)to_remove->module_name,
                   strlen(to_remove->module_name)+1);
      checkdealloc((TAGGED *)to_remove, sizeof(loaded_object));
    }
  }
#if defined(DEBUG)
  if (debug_c) 
    fprintf(stderr, "all_loaded_objects = %lx\n",
            (long unsigned int)all_loaded_objects);
#endif
}


/* Associate an object with its module_name */

void add_to_loaded_objects(module_name, handle, end_func)
     char *module_name;
     void *handle;
     void (*end_func)(char *);
{
  loaded_object new_object = (loaded_object)checkalloc(sizeof(struct lobj));

#if defined(DEBUG)
  if (debug_c) fprintf(stderr, "Adding %s\n",module_name);
#endif
  new_object->module_name = (char *)checkalloc(strlen(module_name)+1);
  strcpy(new_object->module_name, module_name);
  new_object->handle = handle;
  new_object->end_func = end_func; /* JFMC: will be called just before
				      dlclose */
  new_object->next = (struct lobj *)all_loaded_objects;
  all_loaded_objects = new_object;
#if defined(DEBUG)
  if (debug_c)
    fprintf(stderr, 
            "all_loaded_objects = %lx\n",
            (long unsigned int)all_loaded_objects);
#endif
}
#else 
BOOL prolog_dynlink(Arg)
     Argdecl;
{
    ENG_TTYPRINTF0(
    "{ERROR: dynlink: emulator not created with foreign files interface}\n");
   ENG_TTYPRINTF0("{Please recompile with foreing files option turned on}\n");
  return FALSE;
}

BOOL prolog_dynunlink(Arg)                                        /* JFMC */
     Argdecl;
{
  ENG_TTYPRINTF0(
  "{ERROR: dynunlink: emulator not created with foreign files interface}\n");
  ENG_TTYPRINTF0("{Please recompile with foreing files option turned on}\n");
  return FALSE;
}
#endif
