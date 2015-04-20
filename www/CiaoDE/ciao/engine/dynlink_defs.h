/*
  static void unload_if_present PROTO((char *libname));
  static void add_to_loaded_objects PROTO((char *libname, void *handle));
 */


BOOL prolog_dynlink(Argdecl);
BOOL prolog_dynunlink(Argdecl);  /* JFMC */
