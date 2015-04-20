/* 
 *ciao.h -- Definitions for foreign interface.
 *AFSID           : $__Header$
 *Author          : Manuel Carro
 *Created On      : Tue May 14 21:21:03 1996
 */

#if defined(__STDC__)
#  define PROTO(argl) argl
#  define VA_PROTO1(Arg) (Arg, ...)
#else
#  define PROTO(ignore) ()
#  define VA_PROTO1(Arg) ()
#endif

extern int (*ENG_read_hook) PROTO((int fd));/* Called when reading from TTY */


/* Copied from S3. Not yet working. */

typedef long ENG_qid;		/* Choice offset */
typedef char *ENG_pred_ref;	/* Not true but anyway..;-) */
typedef int ENG_term_ref;

typedef unsigned long int ENG_term;

extern ENG_pred_ref ENG_predicate PROTO((char *, long, char *));
extern int ENG_put_string PROTO((ENG_term_ref, char *));
extern int ENG_query_cut_fail VA_PROTO1(ENG_pred_ref predicate);
