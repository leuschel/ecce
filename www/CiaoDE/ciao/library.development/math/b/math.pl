:- module(math, [pow/3], [assertions]).

:- pred pow(+num, +num, out(num)) + foreign(pow).

:- reqlibs(['-lm']). % General case
:- reqlibs('SUNSOL',['-lsockets', '-lm']).  % SUNSOL specific case


:- impl_defined([pow/3]).

Tipos permitidos:

pl       Test            E Ctype   Pl2C                C2Pl
                                                    
int      IsInteger       7 long    GetInteger          MakeInteger
num      IsNumber        9 double  GetFloat            MakeFloat
atm      IsAtom          0 char *  GetString           MakeString
string   in List2String 32 char *  TODO (List2String)  (see prolog_getenvstr())
address  IsInteger       7 void *  (void *)GetInteger  MakeInteger((long) X)
                          



prolog_pow(Arg)           
     Argdecl;
{
  ...
  /* outM is an output argument */
  Ctype outM;
  /* result is the out() argument */
  Ctype result;
  ...

  /* N is an input argument */
  DEREF(X(N), X(N));
  if (!Test(X(N)))
    ERROR_IN_ARG(X(N),N+1,E)

  result = pow(..., Pl2C(X(N)), ..., &outM, ...);

  ...

  DEREF(X(M), X(M));
  
  if (!cunify(Arg, X(M), C2Pl(outM)))
    return FALSE;

  ...

  return cunify(Arg, X(R), C2Pl(result));

}

void math_init(module)
     char *module;
{
  define_c_mod_predicate(module, "pow", prolog_pow, 3);
  ...
}
