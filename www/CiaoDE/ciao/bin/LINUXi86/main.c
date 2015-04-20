/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* This is the main file for an executable.  It just calls the entry point,
   which can as well be called by any other executable to load and start a
   prolog engine. */

int start(int argc, char **argv);

int main(int argc, char *argv[])
{ return start(argc, argv); }
 
