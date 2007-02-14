/* Esparza-Finkel-Mayr Protocol (EFM) 
Example of Fig. 2 in 
    J. Esparza, A. Finkel, and R. Mayr. 
    On the verification of broadcast protocols. 
    In Proceedings of LICS '99, pages 352-359. IEEE Computer Society, 1999. 
  
Vars 
6 
Rules 
X1>=1,X4>=1 -> 
X1=X1-1 
X4=X4-1 
X2=X2+1 
X5=X5+1 ; 
X2>=1,X6>=1 -> 
X2=X2-1 
X3=X3+1 ; 
X4>=1,X3>=1 -> 
X3=X3-1 
X2=X2+1 ; 
X3>=1 -> 
X3=X3-1 
X1=X1+1 
X6=X6+X5 
X5=0 ; 
X2>=1 -> 
X2=X2-1 
X1=X1+1 
X4=X4+X6 
X6=0 ; 
Initial state 
  X1>=1,X4=1,.X2=0,X3=0,X5=0,X6=0 
Target state 
  X3>=1,X2>=1 
/*

