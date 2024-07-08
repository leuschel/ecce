power(_B,0,Res) :- Res=1.
power(Base,Exp,Res) :- Exp>0, E1 is Exp-1,
   power(Base,E1,R1),
   Res is Base*R1.

square(B,Res) :- power(B,2,Res).
cube(B,Res) :- power(B,3,Res).
pow5(B,Res) :- power(B,5,Res).
