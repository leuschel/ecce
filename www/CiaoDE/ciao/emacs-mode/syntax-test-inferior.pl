%%% For inferior mode (paste into a ciao top-level buffer):

% Splash:

Ciao-Prolog 1.9 #44: Mon Dec 23 18:59:46 CET 2002
?- true.

yes
?- X=2, Y=f('kh
	lkjh').
   
X = 2,
Y = f('kh
	lkjh') ? ;

no
ciaopp ?- 

2 ?- 

?- 

   

Ciao Preprocessor V0.8#26 (ciao-1.8)
(C) UPM-CLIP -- Thu Jan  9 20:23:28 CET 2003
ciaopp ?- precompile('/tmp/foowrite.pl',[]).
Select Annotator:            [none,mel,cdg,udg,urlp,crlp] (none) ? 
Select Type Analysis:        [none,regtypes,types,ppregtypes,pptypes,typesfd,pptypesfd] (none) ? 
Select Mode Analysis:        [none,gr,share,shfr,shfrnv,son,shareson,shfrson,path,depth,terms,eterms,ptypes,sha,def,fr,fd,aeq,local] (none) ? 

% Messages:

Ciao-Prolog 1.9 #44: Mon Dec 23 18:59:46 CET 2002
?- hello darling.
{SYNTAX ERROR: (lns 2-2) operator expected after expression
hello 
** here **
darling . 
}

no
?- use_module(foo).
{ERROR: absolute_file_name/7, arg 1 - file foo not found}

no
?- 
