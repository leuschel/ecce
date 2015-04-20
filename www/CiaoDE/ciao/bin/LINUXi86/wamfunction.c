/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "datadefs.h"
#include "support.h"
#include "wamfunction.h"

/* declarations for global functions accessed here */

#include "support_defs.h"
#include "bignum_defs.h"
#include "wamfunction_defs.h"

/* local declarations */

static TAGGED lsh_internal(Argdecl, TAGGED t, int dist, INSN *p);
static TAGGED rsh_internal(Argdecl, TAGGED t, int dist, INSN *p);
/*extern TAGGED bn_call
  PROTO((Argdecl, int (*f)(), TAGGED x, TAGGED y, INSN *op));*/
static TAGGED rsh_internal(Argdecl, TAGGED t, int dist, INSN *op);
static TAGGED lsh_internal(Argdecl, TAGGED t, int dist, INSN *op);



/*
extern TAGGED bn_call
              PROTO((Argdecl, int (*f)(), TAGGED x, TAGGED y, INSN *op));

static TAGGED rsh_internal PROTO((Argdecl, TAGGED t, int dist, INSN *op));
static TAGGED lsh_internal PROTO((Argdecl, TAGGED t, int dist, INSN *op));
*/

static char *illexp = "illegal arithmetic expression";         /* Shared  */


TAGGED fu1_minus(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  /*extern bn_minus();*/
  
  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t))
    {
      if (t==TaggedLow)
	return make_integer_check(Arg, GetSmall(TaggedHigh),p);
      else
	return TaggedZero-(t-TaggedZero);
    }
  else if (IsFloat(t))
    return make_float_check(Arg, -GetFloat(t),p);
  else
    return bn_call(Arg,bn_minus,t,0,p);
}

TAGGED fu1_plus(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  /*extern bn_plus();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t) || p==NULL)
    return t;
  else if (IsFloat(t))
    return make_float_check(Arg, GetFloat(t),p);
  else
    return bn_call(Arg,bn_plus,t,0,p);
}

TAGGED fu1_integer(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  /*extern bn_from_float(), bn_plus();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t) || (p==NULL && !IsFloat(t)))
    return t;
  else if (IsFloat(t))
    return bn_call(Arg,bn_from_float,t,0,p);
  else
    return bn_call(Arg,bn_plus,t,0,p);
}

TAGGED fu1_float(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (p==NULL && IsFloat(t))
    return t;
  else
    return make_float_check(Arg, GetFloat(t),p);
}

TAGGED fu1_add1(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  /*extern bn_incr();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t))
    {
      if (t==TaggedHigh-(1<<SmallShift))
	return make_integer_check(Arg, GetSmall(TaggedHigh),p);
      else
	return t+(1<<SmallShift);
    }
  else if (IsFloat(t))
    return make_float_check(Arg, GetFloat(t) + 1.0,p);
  else
    return bn_call(Arg,bn_incr,t,0,p);
}

TAGGED fu1_sub1(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  /*extern bn_decr();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t))
    {
      if (t==TaggedLow)
	return make_integer_check(Arg, GetSmall(TaggedLow)-1,p);
      else
	return t-(1<<SmallShift);
    }
  else if (IsFloat(t))
    return make_float_check(Arg, GetFloat(t) - 1.0,p);
  else
    return bn_call(Arg,bn_decr,t,0,p);
}


				/* binary functions */

TAGGED fu2_plus(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  /*extern bn_add();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u))
    {
      if (TagIsSmall(t1 = t+(u-TaggedZero)))
	return t1;
      else
	return make_integer_check(Arg, GetSmall(t1),p);
    }
  else if (IsFloat(t) || IsFloat(u))
    return make_float_check(Arg, GetFloat(t) + GetFloat(u),p);
  else
    return bn_call(Arg,bn_add,t,u,p);
}

TAGGED fu2_minus(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  /*extern bn_subtract();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u))
    {
      if (TagIsSmall(t1 = t-(u-TaggedZero)))
	return t1;
      else
	return make_integer_check(Arg, GetSmall(t1),p);
    }
  else if (IsFloat(t) || IsFloat(u))
    return make_float_check(Arg, GetFloat(t) - GetFloat(u),p);
  else
    return bn_call(Arg,bn_subtract,t,u,p);
}

TAGGED fu2_times(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  /*extern bn_multiply();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u))
    {
      int st = GetSmall(t);
      int su = (int)(u-TaggedZero);
      int stu = st*su;
      TAGGED tu = ((TAGGED)stu)+TaggedZero;
      
      if (su==0 || (stu/su==st && TagIsSmall(tu)))
	return tu;
    }
  if (IsFloat(t) || IsFloat(u))
    return make_float_check(Arg, GetFloat(t) * GetFloat(u),p);
  else
    return bn_call(Arg,bn_multiply,t,u,p);
}

TAGGED fu2_fdivide(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  return make_float_check(Arg, GetFloat(t)/GetFloat(u),p);
}

TAGGED fu2_idivide(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  /*extern bn_quotient_remainder_quot_wanted();*/
  /*extern BOOL bn_quotient_wanted;*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u))
    return make_integer_check(Arg, (int)(t-TaggedZero)/(int)(u-TaggedZero),p);

  /*bn_quotient_wanted = TRUE;*/
  return bn_call(Arg,bn_quotient_remainder_quot_wanted,t,u,p);
}

TAGGED fu2_rem(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  /*extern bn_quotient_remainder_quot_not_wanted();*/
  /*extern BOOL bn_quotient_wanted;*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u))
    return (int)(t-TaggedZero)%(int)(u-TaggedZero)+TaggedZero;

  /*bn_quotient_wanted = FALSE;*/
  return bn_call(Arg,bn_quotient_remainder_quot_not_wanted,t,u,p);
}

TAGGED fu2_mod(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  int rem, denom;
  TAGGED T_rem;
  /*extern bn_quotient_remainder_quot_not_wanted();*/
  /*extern bn_add();*/
  /*extern BOOL bn_quotient_wanted;*/
  /*extern BOOL bn_positive PROTO((Bignum *x));*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u)) {
    denom = (int)(u-TaggedZero);
    rem = (int)(t-TaggedZero)%denom;
    return ( (denom > 0 && rem < 0) || (denom < 0 && rem > 0) ?
              rem+denom : rem ) + TaggedZero;
  } else {
    /*bn_quotient_wanted = FALSE;*/
    T_rem = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,t,u,p);
    return ( T_rem != TaggedZero &&
             fu1_sign(Arg,u,p) != fu1_sign(Arg,T_rem,p)
             ? bn_call(Arg,bn_add,T_rem,u,p) : T_rem );
  }
}

TAGGED fu1_abs(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  ENG_FLT f;
  /*extern bn_minus();*/
  /*extern BOOL bn_positive PROTO((Bignum *x));*/
  
  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t)) {
      if (t==TaggedLow)
	return make_integer_check(Arg, GetSmall(TaggedHigh),p);
      else if (t < TaggedZero)
	return TaggedZero-(t-TaggedZero);
      else
        return t;
  } else if (IsFloat(t))
    return (((f = GetFloat(t)) < 0.0) ? make_float_check(Arg, -f, p) : t);
  else 
    return ((!bn_positive(TagToSTR(t))) ? bn_call(Arg,bn_minus,t,0,p) : t);
}

TAGGED fu1_sign(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  ENG_FLT f;
  /*extern BOOL bn_positive PROTO((Bignum *x));*/
  
  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t))
    return ((t==TaggedZero) ? TaggedZero :
            (t < TaggedZero) ? TaggedZero-(1<<SmallShift) :
            TaggedZero+(1<<SmallShift));
  else if (IsFloat(t)) {
    f = GetFloat(t);
    return ((f == 0.0) ? t :
            (f < 0.0) ? make_float_check(Arg, -1.0, p) :
            make_float_check(Arg, 1.0, p));
  } else 
    return ((!bn_positive(TagToSTR(t))) ? TaggedZero-(1<<SmallShift) :
            TaggedZero+(1<<SmallShift));
}

TAGGED fu1_not(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  /*extern bn_not();*/
  
  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t))
    return t^(QMask-(1<<SmallShift));
  else
    return bn_call(Arg,bn_not,t,0,p);
}

TAGGED fu2_xor(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  /*extern bn_xor();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u))
    return t^u^TaggedZero;
  else
    return bn_call(Arg,bn_xor,t,u,p);
}

TAGGED fu2_and(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  /*extern bn_and();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u))
    return ((t^ZMask)&(u^ZMask))^ZMask;
  else
    return bn_call(Arg,bn_and,t,u,p);
}

TAGGED fu2_or(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  /*extern bn_or();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(t) && TagIsSmall(u))
    return ((t^ZMask)|(u^ZMask))^ZMask;
  else
    return bn_call(Arg,bn_or,t,u,p);
}

static TAGGED lsh_internal(Arg,t,dist,p)
     Argdecl;
     TAGGED t;
     int dist;
     INSN *p;
{
  TAGGED u;
  /*extern bn_lshift();*/

  
  if (TagIsSmall(t))
    {
      switch (dist)
	{
	case 0:
	  return t;
	case 1:
	  u = (t<<1) + 0x78000000;
	  break;
	case 2:
	  u = (t<<2) + 0x68000000;
	  break;
	case 3:
	  u = (t<<3) + 0x48000000;
	  break;
	case 4:
	  u = (t<<4) + 0x08000000;
	  break;
	default:
	  u = 0;
	}
      if (TagIsSmall(u))
	return u;
      /*
      int value = GetSmall(t);

      if (dist<32 &&
          value>=0 && value < (unsigned long)(1<<31)>>dist ||
	  value<0 && value >= (long)(-1<<31)>>dist)
	return make_integer_check(value<<dist,p);
	*/
    }



  return bn_call(Arg, bn_lshift, t, MakeInteger(Arg,dist), p);
}


static TAGGED rsh_internal(Arg,t,dist,p)
     Argdecl;
     TAGGED t;
     int dist;
     INSN *p;
{
  /*extern bn_rshift();*/

  
  if (TagIsSmall(t))
    {
      if (dist>=WORDSIZE)
	return MakeSmall((t>=TaggedZero)-1);
      else
	return ((int)((t>>dist)-(TaggedZero>>dist)) & -4) + TaggedZero;
    }



  return bn_call(Arg, bn_rshift, t, MakeInteger(Arg,dist), p);
}

TAGGED fu2_lsh(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  int dist;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  dist = GetInteger(u);

  return (dist<0 ? rsh_internal(Arg,t,-dist,p) : lsh_internal(Arg,t,dist,p));
}

TAGGED fu2_rsh(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;
  int dist;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  dist = GetInteger(u);

  return (dist<0 ? lsh_internal(Arg,t,-dist,p) : rsh_internal(Arg,t,dist,p));
}


/*  GCD for rat arithm., ch feb 92

	This works through the following mechanism:
		
		The arithm. functions (via is/2,</2,...) apply
		NDEREF to their args which calls evaluate  [wamsupport.c]
		which, when it sees terms, calls the corresponding
		function ( max arity = 2) 
		Eval calls this functions with p = NULL which means that 
		numstack_end must not be NULL (make_float_check, make_integer_check)  
*/


TAGGED fu2_gcd(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,u,v;
  REGISTER int type = 3;			/* big x big */ 
  /*extern bn_quotient_remainder_quot_not_wanted(), bn_minus(); */
  /*extern BOOL bn_quotient_wanted;*/
  
  /*extern BOOL bn_positive PROTO((Bignum *x));*/
  
  u=X0; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  if (TagIsSmall(u)) {
    type -= 2;
    if (u<=TaggedZero)
      u = (u==TaggedLow ? make_integer_check(Arg, GetSmall(TaggedHigh),p)
	                : TaggedZero-(u-TaggedZero));
  } else if (IsFloat(u)) return ERRORTAG;
  else if (!bn_positive(TagToSTR(u)))
    u = bn_call(Arg,bn_minus,u,0,p);

  v=X1; 
  NDEREF(Arg, v,t1,return ERRORTAG;);
  if (TagIsSmall(v)) {
    type -= 1;
    if (v<=TaggedZero)
      v = (v==TaggedLow ? make_integer_check(Arg, GetSmall(TaggedHigh),p)
	                : TaggedZero-(v-TaggedZero));
  } else if (IsFloat(v)) return ERRORTAG;
  else if (!bn_positive(TagToSTR(v)))
    v = bn_call(Arg,bn_minus,v,0,p);
                                
  if ( u==TaggedZero ) return v;
  if ( v==TaggedZero ) return u;
  /*bn_quotient_wanted = FALSE;*/

  for (;;) {
    switch (type) {  			/*     u x v     */

      case 0:				/* small x small */
  small_x_small:
      	{ REGISTER unsigned long int x = GetSmall(u), y = GetSmall(v);
      	  for (;;) {
	    x = x % y; if ( x==0 ) return MakeSmall(y);
	    y = y % x; if ( y==0 ) return MakeSmall(x);
      	  }
	}

      case 1:				/* small x big   */
	v = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,v,u,p);
	if ( v==TaggedZero ) return u;
	goto small_x_small;

      case 2:                           /*   big x small */
 	u = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,u,v,p);
	if ( u==TaggedZero ) return v;
	goto small_x_small;

      case 3:				/*   big x big   */
	u = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,u,v,p); 
	if ( u==TaggedZero ) return v;
	if ( TagIsSmall(u) ) type -= 2;
	v = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,v,u,p); 
	if ( v==TaggedZero ) return u;
	if ( TagIsSmall(v) ) type -= 1;
    }
  }
} 




#include <math.h>

#if defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(Win32) || defined(IRIX)
# define aint(f) (f>=0.0 ? floor(f) : ceil(f))
#endif

TAGGED fu1_intpart(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  ENG_FLT f;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);

  f = GetFloat(t);
  return make_float_check(Arg, aint(f),p);
}

TAGGED fu1_fractpart(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  ENG_FLT f;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);

  f = GetFloat(t);
  return make_float_check(Arg, f-aint(f),p);
}

TAGGED fu1_floor(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  TAGGED f;
  /*extern bn_from_float(), bn_plus();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t) || (p==NULL && !IsFloat(t)))
    return t;
  else if (IsFloat(t)) {
    f = make_float_check(Arg, floor(GetFloat(t)),p);
    return bn_call(Arg,bn_from_float,f,0,p);
  } else
    return bn_call(Arg,bn_plus,t,0,p);
}

TAGGED fu1_round(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  TAGGED f;
  /*extern bn_from_float(), bn_plus();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t) || (p==NULL && !IsFloat(t)))
    return t;
  else if (IsFloat(t)) {
    f = make_float_check(Arg, floor(GetFloat(t)+0.5),p);
    return bn_call(Arg,bn_from_float,f,0,p);
  } else
    return bn_call(Arg,bn_plus,t,0,p);
}

TAGGED fu1_ceil(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;
  TAGGED f;
  /*extern bn_from_float(), bn_plus();*/

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  if (TagIsSmall(t) || (p==NULL && !IsFloat(t)))
    return t;
  else if (IsFloat(t)) {
    f = make_float_check(Arg, ceil(GetFloat(t)),p);
    return bn_call(Arg,bn_from_float,f,0,p);
  } else
    return bn_call(Arg,bn_plus,t,0,p);
}

TAGGED fu2_pow(Arg,X0,X1,p)
     Argdecl;
     TAGGED X0,X1;
     INSN *p;
{
  REGISTER TAGGED t1,t,u;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);
  u=X1; 
  NDEREF(Arg, u,t1,return ERRORTAG;);
  return make_float_check(Arg, pow(GetFloat(t),GetFloat(u)),p);
}

TAGGED fu1_exp(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);

  return make_float_check(Arg, exp(GetFloat(t)),p);
}

TAGGED fu1_log(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);

  return make_float_check(Arg, log(GetFloat(t)),p);
}

TAGGED fu1_sqrt(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);

  return make_float_check(Arg, sqrt(GetFloat(t)),p);
}

TAGGED fu1_sin(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);

  return make_float_check(Arg, sin(GetFloat(t)),p);
}

TAGGED fu1_cos(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);

  return make_float_check(Arg, cos(GetFloat(t)),p);
}

TAGGED fu1_atan(Arg,X0,p)
     Argdecl;
     TAGGED X0;
     INSN *p;
{
  REGISTER TAGGED t,t1;

  t=X0; 
  NDEREF(Arg, t,t1,return ERRORTAG;);

  return make_float_check(Arg, atan(GetFloat(t)),p);
}


