
ENG_FLT getdouble(FILE *f);
ENG_INT getlong(FILE *f);
TAGGED getlarge(Argdecl, FILE *f);
char *getstring(Argdecl, FILE *f);
int getshort(FILE *f);
void getbytecode(Argdecl, FILE *f, INSN *insn_p, int length);
