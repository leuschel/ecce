/* Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002 CLIP */

int buffered_input(FILE *stream);
BOOL pop_qlinfo(Argdecl);
BOOL prolog_qread(Argdecl);
BOOL push_qlinfo(Argdecl);
BOOL qread1(Argdecl, FILE *qfile, TAGGED *rungoal);
int is_a_script(FILE *file);
void expand_qload(void);
void reloc_counter(ENG_INT Label);
void reloc_emul_entry(int Li, ENG_INT Label);
void reloc_pointer(int Li, ENG_INT Label);
void skip_to_ctrl_l(FILE *file);

/* We want to use buffered reading of .po files --- it is much faster! */

#define BUFFERED_PO

/* We want to have support for loading compressed bytecode */

#define ALLOW_COMPRESSED_CODE


#if defined(ALLOW_COMPRESSED_CODE)
# define GETC(f) readLZ(f)
int readLZ(FILE *stream);
void is_compressed(FILE *file);
#endif

#if defined(BUFFERED_PO)
# if defined(ALLOW_COMPRESSED_CODE)
#  define GETC_LZ(f) buffered_input(f)
# else
#  define GETC(f) buffered_input(f)
# endif   
# if defined(DEBUG)
#  define UNGETC(chr, f) \
   if (!qlbuffidx)  fprintf(stderr, "Error UNGETting: buffer underfull!\n"); \
   else qlbuffidx--
# else
#  define UNGETC(chr, f) qlbuffidx-- 
# endif
#else
# if defined(ALLOW_COMPRESSED_CODE)
#  define GETC_LZ(f) getc(f)
# else
#  define GETC(f) getc(f)
# endif
# define UNGETC(chr, f) ungetc(chr, f)
#endif

