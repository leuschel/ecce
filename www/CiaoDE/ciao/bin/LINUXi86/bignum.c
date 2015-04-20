/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP */

/* Bignum arithmetics by Torbjorn Granlund, Johan Andersson, and Mats Carlsson
   Ref: Knuth vol. 2 sec. 4.3.1
*/  

#include "threads.h"
#include "datadefs.h"
#include "support.h"

/* declarations for global functions accessed here */

#include "bignum_defs.h"

/* local declarations */

static void bn_negate(register Bignum *x);
static void bn_canonize(register Bignum *x);
static void bn_mult_knuth(register Bignum *x, int xlen, register Bignum *y, int ylen, register Bignum *z);
static int bn_div_mod_quot_wanted(register Bignum *x, register Bignum *y, register Bignum *z, Bignum *zmax);
static int bn_div_mod_quot_not_wanted(register Bignum *x, register Bignum *y, register Bignum *z, Bignum *zmax);

#define HalfUnit (WORDSIZE>>1)
#define HalfMask (BNMAX>>HalfUnit) /* 0x0000ffff */

#define BNMIN ((Bignum)0)          /* 0x0        */
#define BNMAX (~BNMIN)             /* 0xffffffff */

#define ZeroLength (Tag(ATM,4)+QMask)

#define BignumRawLength(b) (b)[0]

#define BignumLength(b) ((BignumRawLength(b)-ZeroLength)>>2)

#define MakeLength(l) ((Bignum)((l)<<2)+ZeroLength)

#define SetBignumLength(b,l) (BignumRawLength(b)=MakeLength(l))

#define BignumPos(b1) WordPos((b1)[BignumLength(b1)])

#define WordPos(w) ((long)(w)>=0)

#define BignumCheck(p,l) \
  if ((p)+(l)+2 > zmax) return (l)+2; \
  SetBignumLength(p,l);

#if BIGENDIAN
# define Getshort(p,i) (((unsigned short *)(p))[((i)+1)^1])
#else
# define Getshort(p,i) (((unsigned short *)(p))[(i)+1])
#endif

/* Only intended to be used outside this file -- e.g., by routines located
   at ciao_prolog.c */

ENG_INT bn_length(REGISTER Bignum *x)
{
  return BignumLength(x);
}


BOOL bn_positive(x)
     REGISTER Bignum *x;
{
  return BignumPos(x);
}


/* PRECONDITION: The result must fit in x.
   +x might occupy one more word than -x */
static void bn_negate(x)
     REGISTER Bignum *x;
{
  REGISTER int i, k;
  int xlen = BignumLength(x);

  for (k=1,i=1; i <= xlen; i++)  {
    x[i] = ~x[i]+k;
    if (k && x[i] != BNMIN) k=0;
  }
}

static void bn_canonize(x)
     REGISTER Bignum *x;
{
  int xlen= BignumLength(x);
  BOOL xs= BignumPos(x);
  REGISTER int i;

  if (xs)
    for (i=xlen; i > 1 && x[i]==BNMIN && WordPos(x[i-1]); i--);
  else
    for (i=xlen; i > 1 && x[i]==BNMAX && !WordPos(x[i-1]); i--);

  SetBignumLength(x,i);
}

int bn_add(x,y,z,zmax)
     REGISTER Bignum *x, *y, *z;
     Bignum *zmax;
{
  REGISTER int i;
  REGISTER Bignum *w, xi, wi;
  unsigned long sign_extension; 
  int min, max;
  BOOL xs= BignumPos(x), ys= BignumPos(y);
  
  if (BignumRawLength(x) > BignumRawLength(y))
    {
      min= BignumLength(y);
      max= BignumLength(x);
      sign_extension= ys-1;
      w= x;
    }
  else
    {
      min= BignumLength(x);
      max= BignumLength(y);
      sign_extension= xs-1;
      w= y;
    }
  
  BignumCheck(z,max+1);

  i=0;
 add_no_carry:
  for (i++; i<=min; i++)
    {
      xi = x[i];
      z[i]= xi+y[i];
      if (xi > z[i])
	goto add_with_carry;
    }
  
  i--;
 se_no_carry:
  for (i++; i<=max; i++)
    {
      wi = w[i];
      z[i]= wi+sign_extension;
      if (z[i] < wi)
	goto se_with_carry;
    }
  
  goto check_sign_overflow;

 add_with_carry:
  for (i++; i<=min; i++)
    {
      xi = x[i];
      z[i]= xi+y[i]+1;
      if (z[i] > xi)
	goto add_no_carry;
    }

  i--;
 se_with_carry:
  for (i++; i<=max; i++)
    {
      wi = w[i];
      z[i]= wi+sign_extension+1;
      if (z[i] > wi)
	goto se_no_carry;
    }
  
 check_sign_overflow:
  if (xs == ys)
    z[max+1] = xs-1; 
  else
    z[max+1] = WordPos(z[max])-1;
  
  bn_canonize(z);  
  return 0;
}

int bn_incr(x,ignore,z,zmax)
     REGISTER Bignum *x, *z;
     Bignum *ignore, *zmax;
{
  REGISTER int i, k;
  int xlen = BignumLength(x);
  int max = xlen+BignumPos(x);
  
  BignumCheck(z,max);
  
  for (i=1, k=1; i<=xlen; i++)
    if ((z[i]=x[i]+k)) k=0;
  
  if (BignumPos(x))
    z[max] = BNMIN+k;
  
  bn_canonize(z);
  return 0;
}


int bn_plus(x,ignore,z,zmax)
     REGISTER Bignum *x, *z;
     Bignum *ignore, *zmax;
{
  int xlen = BignumLength(x);
  REGISTER int i;
  
  BignumCheck(z,xlen);
  for (i=1; i<=xlen; i++)
    z[i] = x[i];

  return 0;
}

int bn_subtract(x,y,z,zmax)
     REGISTER Bignum *x, *y, *z;
     Bignum *zmax;
{
  REGISTER int i;
  REGISTER Bignum *w, xi;
  unsigned long sign_extension;
  int min, max;
  BOOL xs= BignumPos(x), ys= BignumPos(y);
  
  if (BignumRawLength(x) > BignumRawLength(y))
    {
      min= BignumLength(y);
      max= BignumLength(x);
      sign_extension= ys-1;
      w= x;
    }
  else
    {
      min= BignumLength(x);
      max= BignumLength(y);
      sign_extension= xs-1;
      w= y;
    }

  BignumCheck(z,max+1);

  i=0;
 subtract_no_carry:
  for (i++; i<=min; i++)
    {
      xi = x[i];
      z[i]= xi-y[i];
      if (z[i] > xi)
	goto subtract_with_carry;
    }

  i--;
 se_no_carry:
  for (i++; i<=max; i++) 
    if (x==w)
      {
	xi = x[i];
	z[i]= xi-sign_extension;
	if (z[i] > xi)
	  goto se_with_carry;
      }
    else
      {
	z[i]= sign_extension-y[i];
	if (z[i] > sign_extension)
	  goto se_with_carry;
      }
  
  goto check_sign_overflow;
  
 subtract_with_carry:
  for (i++; i<=min; i++)
    {
      xi = x[i];
      z[i]= xi-y[i]-1;
      if (xi > z[i])
	goto subtract_no_carry;
    }

  i--;
 se_with_carry:
  for (i++; i<=max; i++)
    if (x==w)
      {
	xi = x[i];
	z[i]= xi-sign_extension-1;
	if (xi > z[i])
	  goto se_no_carry;
      }
    else
      {
	z[i]= sign_extension-y[i]-1;
	if (sign_extension > z[i])
	  goto se_no_carry;
      }
  
 check_sign_overflow:
  if (xs != ys)
    z[max+1]= xs-1;
  else
    z[max+1]= WordPos(z[max])-1;
  
  bn_canonize(z);
  return 0;
}

     

int bn_decr(x,ignore,z,zmax)
     REGISTER Bignum *x, *z;
     Bignum *ignore, *zmax;
{
  REGISTER int i, k;
  int xlen = BignumLength(x);
  int max = xlen+(!BignumPos(x));
  
  BignumCheck(z,max);
  
  for (i=1, k=1; i<=xlen; i++)
    if (~(z[i]=x[i]-k)) k=0;
  
  if (!BignumPos(x))
    z[max] = BNMAX;
  
  bn_canonize(z);
  return 0;
}


int bn_minus(x,ignore,z,zmax)
     REGISTER Bignum *x, *z;
     Bignum *ignore, *zmax;
{
  int xlen = BignumLength(x);
  REGISTER int i;
  
  if (BignumPos(x))
    {
      BignumCheck(z,xlen);
      for (i=1; i<=xlen; i++)
	z[i] = x[i];
    }
  else
    {
      BignumCheck(z,xlen+1);
      for (i=1; i<=xlen; i++)
	z[i] = x[i];
      z[xlen+1] = BNMAX;
    }
  bn_negate(z);
  bn_canonize(z);
  return 0;
}


int bn_and(x,y,z,zmax)
     REGISTER Bignum *x, *y, *z;
     Bignum *zmax;
{
  REGISTER int i;
  int min, max;
  unsigned long mask;
  
  if (BignumRawLength(x) > BignumRawLength(y))
    {
      min= BignumLength(y);
      max= BignumLength(x);
      mask= BignumPos(y)-1;
    }
  else
    {
      Bignum *temp;
      
      min= BignumLength(x);
      max= BignumLength(y);
      mask= BignumPos(x)-1;
      temp = x;
      x = y;
      y = temp;
    }

  BignumCheck(z,max);
  
  for (i= 1; i <= min; i++)
    z[i]= x[i]&y[i];
  
  for (; i <= max; i++)
    z[i]= x[i]&mask;
  
  bn_canonize(z);
  return 0;
}

int bn_or(x,y,z,zmax)
     REGISTER Bignum *x, *y, *z;
     Bignum *zmax;
{
  REGISTER int i;
  int min, max;
  unsigned long mask;
  
  if (BignumRawLength(x) > BignumRawLength(y))
    {
      min= BignumLength(y);
      max= BignumLength(x);
      mask= BignumPos(y)-1;
    }
  else
    {
      Bignum *temp;
      
      min= BignumLength(x);
      max= BignumLength(y);
      mask= BignumPos(x)-1;
      temp = x;
      x = y;
      y = temp;
    }

  BignumCheck(z,max);
  
  for (i= 1; i <= min; i++)
    z[i]= x[i]|y[i];
  
  for (; i <= max; i++)
    z[i]= x[i]|mask;
  
  bn_canonize(z);
  return 0;
}

int bn_xor(x,y,z,zmax)
     REGISTER Bignum *x, *y, *z;
     Bignum *zmax;
{
  REGISTER int i;
  int min, max;
  unsigned long mask;
  
  if (BignumRawLength(x) > BignumRawLength(y))
    {
      min= BignumLength(y);
      max= BignumLength(x);
      mask= BignumPos(y)-1;
    }
  else
    {
      Bignum *temp;
      
      min= BignumLength(x);
      max= BignumLength(y);
      mask= BignumPos(x)-1;
      temp = x;
      x = y;
      y = temp;
    }

  BignumCheck(z,max);
  
  for (i= 1; i <= min; i++)
    z[i]= x[i]^y[i];
  
  for (; i <= max; i++)
    z[i]= x[i]^mask;
  
  bn_canonize(z);
  return 0;
}

int bn_not(x,y,z,zmax)
     REGISTER Bignum *x, *y, *z;
     Bignum *zmax;
{
  REGISTER int i;
  int xlen = BignumLength(x);

  BignumCheck(z,xlen);
  
  for (i= 1; i <= xlen; i++)
    z[i]= ~x[i];
  
  /* z must be canonical now */
  return 0;
}


/* This was used to communicate a value from lsh_internal and rsh_internal to
   bn_lshift and bn_rshift, but cannot be passed through bn_call */

/*unsigned long bn_shift_dist; */




int bn_lshift(x,dist,z,zmax)
     REGISTER Bignum *x, *z;
     Bignum *dist;
     Bignum *zmax;
{
  REGISTER Bignum xi;
  BOOL xs= BignumPos(x);
  int xlen = BignumLength(x);
  unsigned short shift = Getshort(dist,1);/* Guessing --- seems to work. MCL */
  int div, rem;
  REGISTER int i;
  
  div = shift >> 5;
  rem = shift &  0x1f;

  BignumCheck(z,xlen+div+(rem>0));
  
  /* first perform the `long' shift, if any */
  for (i=1; i<=div; i++)
    z[i] = BNMIN;
  
  /* Then perform the short shift */
  if (rem == 0) /* copy */
    for (i=1; i <= xlen; i++)
      z[div+i]= x[i];
  else
    {
      int mer=WORDSIZE-rem;
      unsigned long carry=0;
      
      for (i=1; i<=xlen; i++)
	xi = x[i],
	z[div+i] = xi<<rem | carry,
	carry = xi>>mer;
      z[div+i] = (xs-1)<<rem | carry;
    }
  
  bn_canonize(z);
  return 0;
}

int bn_rshift(x,dist,z,zmax)
     REGISTER Bignum *x, *z;
     Bignum *dist;
     Bignum *zmax;
{
  REGISTER Bignum xi;
  BOOL xs= BignumPos(x);
  int xlen = BignumLength(x);
  unsigned short shift = Getshort(dist,1);/* Guessing --- seems to work. MCL */
  int div, rem;
  REGISTER int i;

  div = shift >> 5;
  rem = shift &  0x1f;

  if (xlen-div<1) {
    BignumCheck(z,1);
    z[1] = xs-1;
    return 0;
  }

  BignumCheck(z,xlen-div);
  
  if (rem==0)
    for (i=xlen-div; i>=1; i--)
      z[i] = x[div+i];
  else
    {
      int mer=WORDSIZE-rem;
      unsigned long carry=(xs-1)<<mer;
      
    for (i=xlen-div; i>=1; i--)
      xi = x[div+i],
      z[i] = xi>>rem | carry,
      carry = xi<<mer;
    }
  
  bn_canonize(z);
  return 0;
}



int bn_compare(x,y)
     REGISTER Bignum *x, *y;
{
  int xlen= BignumLength(x), ylen= BignumLength(y);
  BOOL xs=BignumPos(x), ys=BignumPos(y);

  if (xs != ys)
    return (xs ? 1 : -1);
  else if (xlen != ylen)
    return (xs^(xlen>ylen) ? -1 : 1);
  else
    {
      REGISTER int i=xlen+1;
      
      while (--i)
	if (x[i]!=y[i])
	  return (x[i]<y[i] ? -1 : 1);
      return 0;
    } 
}



/* y is shorter than x */
static void bn_mult_knuth(x,xlen,y,ylen,z)
     REGISTER Bignum *x, *y, *z;
     int xlen, ylen;
{
  REGISTER int i, j;
  REGISTER unsigned short yj;

  for (i=1; i<=xlen+ylen; i++) z[i] = 0;

  xlen <<= 1;
  ylen <<= 1;
  for (j=1; j<=ylen; j++)
    if ((yj=Getshort(y,j)))
      {
	REGISTER unsigned long t=0;
	
	for (i=1; i<=xlen; i++)
	  t = Getshort(z,i+j-1) + Getshort(x,i)*yj + (t>>HalfUnit),
	  Getshort(z,i+j-1) = t&HalfMask;
	Getshort(z,xlen+j) = (t>>HalfUnit);
      }
}


int bn_multiply(x,y,z,zmax)
     Bignum *x, *y, *z, *zmax;
{
  BOOL sx = BignumPos(x);
  BOOL sy = BignumPos(y);
  int xlen = BignumLength(x);
  int ylen = BignumLength(y);

  BignumCheck(z,xlen+ylen);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (xlen>ylen)
    bn_mult_knuth(x,xlen,y,ylen,z);
  else
    bn_mult_knuth(y,ylen,x,xlen,z);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (sx^sy) bn_negate(z);
  bn_canonize(z); 
  return 0;
}


BOOL bn_quotient_wanted;

static int bn_div_mod_quot_wanted(x,y,z,zmax)
     REGISTER Bignum *x, *y, *z;
     Bignum *zmax;
{
  int d;
  int xlen = BignumLength(x);
  int ylen = BignumLength(y);
  int ulen=xlen<<1, vlen=ylen<<1; /*, zlen; */
  REGISTER int i, j, k;
  unsigned long carry, v1qhat, v2qhat;
  REGISTER unsigned short
		 *u,		/* dividend, size=ulen+1 */
                 *v,		/* divisor, size=vlen */
                 *q;		/* quotient, size=ulen-vlen+1 */
  REGISTER unsigned short v1, v2, qj;
  
  while (!Getshort(x,ulen) && ulen>1) ulen--;
  while (!Getshort(y,vlen) && vlen>1) vlen--;
  if (vlen>ulen) {
    BignumCheck(z,1);
    z[1] = 0;
    return 0;
  }
  else if (vlen==1) {                      /* special case: a simple loop */
    v1 = Getshort(y,1);
    for (carry=0, i=ulen; i>0; i--)
      carry += Getshort(x,i),
      Getshort(z,i) = carry/v1,
      carry = (carry%v1)<<HalfUnit;
    BignumCheck(z,(ulen+2)>>1);
    Getshort(z,ulen+1) = 0;
    Getshort(z,ulen+2) = 0;
    return 0;
  }
  BignumCheck(z,(3*ulen+4)>>1);
  v = (unsigned short *)z+ulen+2;
  u = v+vlen;
  q = u+ulen+1;
				/* Normalize. */
  v1 = Getshort(y,vlen);
  for (d=0; v1 < (unsigned long)1<<(HalfUnit-1); d++)
    v1 <<= 1;
  for (carry=0, i=0; i<ulen; i++)
    carry += Getshort(x,i+1)<<d,
    u[i] = carry & HalfMask,
    carry >>= HalfUnit;
  u[ulen] = carry;
  for (carry=0, i=0; i<vlen; i++)
    carry += Getshort(y,i+1)<<d,
    v[i] = carry & HalfMask,
    carry >>= HalfUnit;
  
  v1 = v[vlen-1], v2 = v[vlen-2];
  for (j=ulen; j>=vlen; j--)
    {
				/* Calculate q[j]. */
      carry = (u[j]<<HalfUnit) + u[j-1];
      if (u[j]==v1) qj = HalfMask;
      else qj = carry/v1;
      
      v1qhat = v1*qj;
      v2qhat = v2*qj;
      while (carry-v1qhat < (1<<HalfUnit) &&
	     v2qhat > ((carry-v1qhat)<<HalfUnit)+u[j-2])
	qj--, v1qhat-=v1, v2qhat-=v2;
				/* Multiply and subtract. */
      if ((q[j]=qj))
	{
	  for (carry=0, i=0, k=j-vlen; i<vlen-2; i++, k++)
	    carry = u[k] - v[i]*qj - carry,
	    u[k] = carry & HalfMask,
	    carry = (u[k]-carry)>>HalfUnit;
	  carry = u[k] - v2qhat - carry,
	  u[k] = carry & HalfMask;
	  carry = (u[k]-carry)>>HalfUnit;
	  k++;
	  carry = u[k] - v1qhat - carry;
	  u[k] = carry & HalfMask;
	  carry = (u[k]-carry)>>HalfUnit;
	  carry = u[j] - carry;
	  u[j] = carry & HalfMask;
	  carry = (u[j]-carry)>>HalfUnit;
	  if (carry)
	    {
	      q[j]--;
	      for (carry=0, i=0, k=j-vlen; i<vlen; i++, k++)
		carry += u[k] + v[i],
		u[k] = carry & HalfMask,
		carry = carry>>HalfUnit;
	      u[j] += carry;
	    }
	}
    }
  /* q[vlen .. ulen] is the desired quotient. */
  SetBignumLength(z,(ulen-vlen+3)>>1);
  for (i=1, k=vlen; k<=ulen; i++, k++)
    Getshort(z,i) = q[k];
  Getshort(z,i++) = 0;
  Getshort(z,i++) = 0;
  return 0;
}


static int bn_div_mod_quot_not_wanted(x,y,z,zmax)
     REGISTER Bignum *x, *y, *z;
     Bignum *zmax;
{
  int d;
  int xlen = BignumLength(x);
  int ylen = BignumLength(y);
  int ulen=xlen<<1, vlen=ylen<<1; /*, zlen; */
  REGISTER int i, j, k;
  unsigned long carry, v1qhat, v2qhat;
  REGISTER unsigned short
		 *u,		/* dividend, size=ulen+1 */
                 *v,		/* divisor, size=vlen */
                 *q;		/* quotient, size=ulen-vlen+1 */
  REGISTER unsigned short v1, v2, qj;
  
  while (!Getshort(x,ulen) && ulen>1) ulen--;
  while (!Getshort(y,vlen) && vlen>1) vlen--;
  if (vlen>ulen) {
    BignumCheck(z,xlen);
    for (i=1; i<=xlen; i++)
      z[i] = x[i];
    return 0;
  }
  else if (vlen==1) {                      /* special case: a simple loop */
    v1 = Getshort(y,1);
    for (carry=0, i=ulen; i>0; i--)
      carry += Getshort(x,i),
      Getshort(z,i) = carry/v1,
      carry = (carry%v1)<<HalfUnit;
    BignumCheck(z,1);
    z[1] = carry>>HalfUnit;
    return 0;
  }
  BignumCheck(z,(3*ulen+4)>>1);
  v = (unsigned short *)z+ulen+2;
  u = v+vlen;
  q = u+ulen+1;
				/* Normalize. */
  v1 = Getshort(y,vlen);
  for (d=0; v1 < (unsigned long)1<<(HalfUnit-1); d++)
    v1 <<= 1;
  for (carry=0, i=0; i<ulen; i++)
    carry += Getshort(x,i+1)<<d,
    u[i] = carry & HalfMask,
    carry >>= HalfUnit;
  u[ulen] = carry;
  for (carry=0, i=0; i<vlen; i++)
    carry += Getshort(y,i+1)<<d,
    v[i] = carry & HalfMask,
    carry >>= HalfUnit;
  
  v1 = v[vlen-1], v2 = v[vlen-2];
  for (j=ulen; j>=vlen; j--)
    {
				/* Calculate q[j]. */
      carry = (u[j]<<HalfUnit) + u[j-1];
      if (u[j]==v1) qj = HalfMask;
      else qj = carry/v1;
      
      v1qhat = v1*qj;
      v2qhat = v2*qj;
      while (carry-v1qhat < (1<<HalfUnit) &&
	     v2qhat > ((carry-v1qhat)<<HalfUnit)+u[j-2])
	qj--, v1qhat-=v1, v2qhat-=v2;
				/* Multiply and subtract. */
      if ((q[j]=qj))
	{
	  for (carry=0, i=0, k=j-vlen; i<vlen-2; i++, k++)
	    carry = u[k] - v[i]*qj - carry,
	    u[k] = carry & HalfMask,
	    carry = (u[k]-carry)>>HalfUnit;
	  carry = u[k] - v2qhat - carry,
	  u[k] = carry & HalfMask;
	  carry = (u[k]-carry)>>HalfUnit;
	  k++;
	  carry = u[k] - v1qhat - carry;
	  u[k] = carry & HalfMask;
	  carry = (u[k]-carry)>>HalfUnit;
	  carry = u[j] - carry;
	  u[j] = carry & HalfMask;
	  carry = (u[j]-carry)>>HalfUnit;
	  if (carry)
	    {
	      q[j]--;
	      for (carry=0, i=0, k=j-vlen; i<vlen; i++, k++)
		carry += u[k] + v[i],
		u[k] = carry & HalfMask,
		carry = carry>>HalfUnit;
	      u[j] += carry;
	    }
	}
    }
  /* u[0 .. vlen-1]>>d is the desired remainder. */
  SetBignumLength(z,(vlen+2)>>1);
  v1qhat = (1<<d)-1;
  for (carry=0, i=vlen; i>0; i--)
    carry += u[i-1],
      Getshort(z,i) = carry>>d,
      carry = (carry&v1qhat)<<HalfUnit;
  Getshort(z,vlen+1) = 0;
  Getshort(z,vlen+2) = 0;
  return 0;
}



int bn_quotient_remainder_quot_wanted(x,y,z,zmax)
     Bignum *x, *y, *z, *zmax;
{
  BOOL sx = BignumPos(x);
  BOOL sy = BignumPos(y);
  int value;
  
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  value = bn_div_mod_quot_wanted(x,y,z,zmax);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (!value) {
    if (sx^sy)
      bn_negate(z);
    bn_canonize(z);
  }
  return value;
}

int bn_quotient_remainder_quot_not_wanted(x,y,z,zmax)
     Bignum *x, *y, *z, *zmax;
{
  BOOL sx = BignumPos(x);
  BOOL sy = BignumPos(y);
  int value;
  
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  value = bn_div_mod_quot_not_wanted(x,y,z,zmax);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (!value) {
    if (!sx)
      bn_negate(z);
    bn_canonize(z);
  }
  return value;
}



int bn_from_float(x,ignore,z,zmax)
     REGISTER Bignum *x, *z;
     Bignum *ignore, *zmax;
{
  ENG_FLT f;
  ENG_FLT norm = 4294967296.0;	/* 2**32 */
  ENG_FLT norm2 = norm*norm;
  unsigned long *fp = (unsigned long *)(&f);
  int i, exp, div, rem, zlen;
  BOOL sx = TRUE;
  Bignum uhigh, ulow;
  
  fp[0] = x[1];			/* GetFloat(x) */
  fp[1] = x[2];
#if defined(sun)
  if (fp[1-BIGENDIAN] & 0x80000000)
#else
  if (f < 0.0 ||
      (f == 0.0 && (fp[0]|fp[1]) != 0) ||
      (fp[0]&fp[1]) == 0xffffffff)
#endif
    sx = FALSE,
    f = -f;
  
    if (f < 1.0)
      {
	BignumCheck(z,1);
	z[1] = 0;
	goto ret;
      }
    else if (f != f || f == f/2.0) /* catch NaN, Infinity */
      {
	return QMask>>2;
      }
  
  /* normalize */
  exp = 64;
  while (f >= norm2)
    exp+=64, f /= norm2;
  norm2 /= 2.0;
  while (f < norm2)
    --exp, f *= 2.0;

  zlen = ((exp-1)>>5)+2;	/* ensure there is room for sign bit */
  div = (exp-64)>>5;
  rem = exp & 0x1f;
  BignumCheck(z,zlen);
  for (i=1; i<=zlen; i++) z[i] = 0;
				/* turn off high bit of uhigh since it
				   causes trouble on certain machines */
  f -= norm2;
  uhigh = f/norm;
  ulow = f-norm*uhigh;
  uhigh += 0x80000000;
  if (rem==0)
    {
      if (div>=0) z[div+1] = ulow;
      if (div>=-1) z[div+2] = uhigh;
    }
  else
    {
      if (div>=0) z[div+1] = ulow<<rem;
      if (div>=-1) z[div+2] = (ulow>>(WORDSIZE-rem))+(uhigh<<rem);
      if (div>=-2) z[div+3] = uhigh>>(WORDSIZE-rem);
    }
 ret:
  if (!sx) bn_negate(z);
  bn_canonize(z);
  return 0;
}


/* Precond: x is a syntactically correct string denoting an integer */
int bn_from_string(x,z,zmax,base)
     REGISTER char *x;
     REGISTER Bignum *z;
     Bignum *zmax;
     int base;
{
  BOOL sx;
  REGISTER int j;
  int zlen;
  char cur;
  unsigned long t, digit;
/*   unsigned int radix = GetSmall(current_radix); */

  if (*x=='+')
    x++, sx=TRUE;
  else if (*x=='-')
    x++, sx=FALSE;
  else
    sx=TRUE;

  zlen = 2;
  BignumCheck(z,1);
  z[1] = 0;			/* always keep a zero pad word */
  for (cur = *x++; cur; cur = *x++)
    {
      digit = (cur>='a' ? cur-'a'+10 : cur>='A' ? cur-'A'+10 : cur-'0');
      for (j=1; j<zlen; j++)
	t = base*Getshort(z,j) + digit,
	Getshort(z,j) = t&HalfMask,
	digit = t>>HalfUnit;
      if (digit)
	{
	  zlen += 2;
	  BignumCheck(z,zlen>>1);
	  Getshort(z,j) = digit;
	  Getshort(z,j+1) = 0;
	  Getshort(z,j+2) = 0;
	  Getshort(z,j+3) = 0;
	}
    }
  if (!sx) bn_negate(z);
  bn_canonize(z);
  return 0;
}



void bn_to_string(Arg,x,base)
     Argdecl;
     REGISTER Bignum *x;
     int base;
{
  REGISTER int j, k;
  int xlen, slen, alen, dlen;
  unsigned long r, digit, divisor;
  BOOL sx = BignumPos(x);
  char hibase = 'a'-10;
  unsigned short *work;
  REGISTER char *c0, *c, d;

  if (base<0)
    hibase = 'A'-10,
    base = -base;
  xlen = BignumLength(x)<<1;

				/* compute divisor = base**N
				   such that divisor <= 1<<16. */
  r = (1<<HalfUnit)/base;
  for (dlen=1, divisor=base; divisor<=r; dlen++)
    divisor *= base;

				/* string length <=
				   (words+1)*ceiling(digits/word) */
  slen = (((xlen+1)*(dlen+1)) & -4)+4 + (xlen<<1);
  for (alen=Atom_Buffer_Length; slen>alen;)
    alen <<= 1;
  /*
  if (alen>Atom_Buffer_Length)
    Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer,
				       Atom_Buffer_Length, alen),
    Atom_Buffer_Length = alen;
  */
  if (alen > Atom_Buffer_Length)
      EXPAND_ATOM_BUFFER(alen);
  c = Atom_Buffer;
  work = (unsigned short *)(c+slen-(xlen<<1));
  if (!sx)
    {
      *c++ = '-';
      for (k=1, j=0; j<xlen; j++)
	  work[j] = ~Getshort(x,j+1)+k,
	  k &= !work[j];
    }
  else
    for (j=0; j<xlen; j++)
      work[j] = Getshort(x,j+1);
  while (xlen>0 && !work[xlen-1])
    xlen--;

  while (xlen>0)
    {
      for (j=xlen-1, r=0; j >= 0; j--)
	{
	  digit = (r<<HalfUnit) + work[j];
	  work[j] = digit/divisor;
	  r = digit%divisor;
	}
      for (j=dlen; j>0; j--)
	{
	  digit = r%base;
	  *c++ = (digit<10 ? '0'+digit : hibase+digit);
	  r /= base;
	} 
      while (xlen>0 && !work[xlen-1])
	xlen--;
    }

  do
    c--;
  while (c[0]=='0');
  c[1] = 0;
  for (c0=Atom_Buffer+1-sx; c0<c; c0++, c--)
    d = *c0, *c0 = *c, *c = d;
}
