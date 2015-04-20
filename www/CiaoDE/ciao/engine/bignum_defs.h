/*

 */
 
#include "threads.h"
#include "datadefs.h"
#include "support.h"

BOOL bn_positive(register Bignum *x);
int bn_add(register Bignum *x, register Bignum *y, register Bignum *z, Bignum *zmax);
int bn_incr(register Bignum *x, Bignum *ignore, register Bignum *z, Bignum *zmax);
int bn_plus(register Bignum *x, Bignum *ignore, register Bignum *z, Bignum *zmax);
int bn_subtract(register Bignum *x, register Bignum *y, register Bignum *z, Bignum *zmax);
int bn_decr(register Bignum *x, Bignum *ignore, register Bignum *z, Bignum *zmax);
int bn_minus(register Bignum *x, Bignum *ignore, register Bignum *z, Bignum *zmax);
int bn_and(register Bignum *x, register Bignum *y, register Bignum *z, Bignum *zmax);
int bn_or(register Bignum *x, register Bignum *y, register Bignum *z, Bignum *zmax);
int bn_xor(register Bignum *x, register Bignum *y, register Bignum *z, Bignum *zmax);
int bn_not(register Bignum *x, register Bignum *y, register Bignum *z, Bignum *zmax);
int bn_lshift(register Bignum *x, Bignum *dist, register Bignum *z, Bignum *zmax);
int bn_rshift(register Bignum *x, Bignum *dist, register Bignum *z, Bignum *zmax);
int bn_compare(register Bignum *x, register Bignum *y);
int bn_multiply(Bignum *x, Bignum *y, Bignum *z, Bignum *zmax);
int bn_quotient_remainder_quot_wanted(Bignum *x, Bignum *y, Bignum *z, Bignum *zmax);
int bn_quotient_remainder_quot_not_wanted(Bignum *x, Bignum *y, Bignum *z, Bignum *zmax);
int bn_from_float(register Bignum *x, Bignum *ignore, register Bignum *z, Bignum *zmax);
int bn_from_string(register char *x, register Bignum *z, Bignum *zmax, int base);
void bn_to_string(Argdecl, register Bignum *x, int base);


/* To be used by the test routines in ciao_prolog.c */

ENG_INT bn_length(REGISTER Bignum *x);
