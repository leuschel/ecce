/* Copyright (C) 1998,1999,2000,2001,2002 UPM-CLIP */

#include "common_headers.h"
#include <math.h>

BOOL prolog_pow(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  if (!IsNumber(X(0)))
    ERROR_IN_ARG(X(0),1,NUMBER)

  DEREF(X(1), X(1));
  if (!IsNumber(X(1)))
    ERROR_IN_ARG(X(1),2,NUMBER)

  return
    cunify(Arg,
           make_float(Arg, pow(GetFloat(X(0)), GetFloat(X(1)))),
           X(2));
}

#if defined(Solaris) || defined(LINUX)
double log2(n)
     double n;
{
  return log(n)/log(2.0);
}
#endif

BOOL prolog_log2(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  if (!IsNumber(X(0)))
    ERROR_IN_ARG(X(0),1,NUMBER)

  return
    cunify(Arg,
           make_float(Arg, log2(GetFloat(X(0)))),
           X(1));
}

void math_c_init(module)
     char *module;
{
  define_c_mod_predicate(module, "pow", prolog_pow, 3);
  define_c_mod_predicate(module, "log2", prolog_log2, 2);
}
