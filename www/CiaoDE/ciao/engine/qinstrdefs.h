/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

                                /* Control codes to be detected */ 

#define ISCOMPRESSED 12 /* Ctrl-L */
#define ISSCRIPT 35 /* # */

				/* Quick load instructions: 'A'..'O' */

#define ENSURE_SPACE 64
#define LOAD_ATOM 65
#define LOAD_FUNCTOR 66
#define LOAD_NUMBER_S 67
#define LOAD_NUMBER_L 68
#define LOAD_NUMBER_F 69

#define LOAD_VARIABLE 70
#define LOAD_NIL 71
#define LOAD_LIST 72
#define LOAD_TUPLE 73
#define LOAD_ARGUMENT 74
#define LOAD_DBNODE 75

#define RETURN 76

#define RELOC_POINTER 77
#define RELOC_EMUL_ENTRY 78
#define RELOC_COUNTER 79


