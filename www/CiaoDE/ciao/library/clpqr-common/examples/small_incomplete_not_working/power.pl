%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Taken from Eric Vetillard (PrologIA)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            Original Version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% % power.p3
%% % Program by Eric Vetillard, PrologIA
%% % (C) PrologIA 1991,1992
%% 
%% /* Data */
%% 
%% rp -> 
%% reinsert("power/power.p3");
%% 
%% PowerStationsSmall( <
%% < 1 , 12 , 12 ,  850 , 2000 , 10000 , 20 , 20000 > ,
%% < 2 ,  3 ,  9 , 1250 , 1750 , 26000 , 13 , 10000 > ,
%% < 3 ,  0 ,  2 , 1500 , 4000 , 30000 , 30 ,  5000 > > ) -> ;
%% 
%% PowerStations( <
%% < 1 ,  0 , 12 ,  850 , 2000 , 10000 , 20 , 20000 > ,
%% < 2 ,  0 , 10 , 1250 , 1750 , 26000 , 13 , 10000 > ,
%% < 3 ,  0 ,  5 , 1500 , 4000 , 30000 , 30 ,  5000 > > ) -> ;
%% 
%% Periods( <
%% < 1 , 6 , 15000 > ,
%% < 2 , 3 , 30000 > ,
%% < 3 , 6 , 25000 > ,
%% < 4 , 3 , 40000 > ,
%% < 5 , 6 , 27000 > > ) -> ;
%% 
%% /* Data organization and Simple Constraints */
%% 
%% make_period(d,<>,p,<d,p>) -> ;
%% make_period(d,<s>.t,p,r) ->
%% make_period(d,t,< <i,n,s',p'> >.p,r) ,
%% { s = <i,m,M,p1,p2,c1,c2,c3> ,
%%   n>=m , n<=M ,
%%   p'>=n*p1 , p'<=n*p2 } ;
%% 
%% make_day(<>,s,d,d) -> ;
%% make_day(<p>.t,s,d,r) ->
%% make_period(p,s,<>,r')
%% make_day(t,s,<r'>.d,r) ;
%% 
%% make_data(d) ->
%% Periods(p)
%% PowerStations(s)
%% make_day(p,s,<>,d) ;
%% 
%% /* Production constraints and Cost Computation */
%% 
%% calc_station(s,h,c,p',m') ->
%% PowerStations(t)
%% member(<i,m,M,p1,p2,c1,c2,c3>,t) ,
%% { s= < i,n,s',p> ,
%%   m'=n*p2 , p'=p ,
%%   c=h*(n*c1+(p-n*p1)*c2)+s'*c3 } ;
%% 
%% calc_period(<>,h,c,c,p,p,m,m) -> ;
%% calc_period(<s>.t,h,c,c''',p,p''',m,m''') ->
%% calc_station(s,h,c',p',m')
%% calc_period(t,h,c'',c''',p'',p''',m'',m''') ,
%% { c''=c+c' , m''=m+m' , p''=p+p' } ;
%% 
%% calc_prod_cost(<>,c,c) -> ;
%% calc_prod_cost(< <d,p> >.t,c,c''') ->
%% calc_period(p,h,0,c',0,p',0,p'')
%% calc_prod_cost(t,c'',c''') ,
%% { c'' = c+c' ,
%%   d = <i,h,u> ,
%%   p'>=u , p''>=115/100*u } ;
%% 
%% start_period(<>,<>) -> ;
%% start_period(<s1>.t1,<s2>.t2) ->
%% start_period(t1,t2) ,
%% { s1 = <i1,n1,u1,p1> , s2= <i2,n2,u2,p2> ,
%%   u1>=0 , u1>=n1-n2 , u1<=n1 } ;
%% 
%% start_day(< <d1,p1> >,f) ->
%% start_period(p1,f) ;
%% start_day(< <d1,p1>,<d2,p2> >.t,f) ->
%% start_period(p1,p2)
%% start_day(< <d2,p2> >.t,f) ;
%% 
%% calc_start(<>) -> ;
%% calc_start(< <d,p> >.t) ->
%% start_day(< <d,p> >.t,p) ;
%% 
%% /* Utilities */
%% 
%% member(x,<x>.t) -> ;
%% member(x,<y>.t) ->
%% member(x,t) ;
%% 
%% /* Enumeration */
%% 
%% anum(0) -> ;
%% anum(1) -> ;
%% anum(2) -> ;
%% anum(3) -> ;
%% anum(4) -> ;
%% anum(5) -> ;
%% anum(6) -> ;
%% anum(7) -> ;
%% anum(8) -> ;
%% anum(9) -> ;
%% anum(10) -> ;
%% anum(11) -> ;
%% anum(12) -> ;
%% 
%% enum_periodn(<>,c) ->
%% val(min_price,x) ,
%% { c<=x-1 } ;
%% enum_periodn(< <i,n,s,p> >.r,c) ->
%% anum(n)
%% val(min_price,x) 
%% enum_periodn(r,c) ,
%% { c<=x-1 } ;
%% 
%% enum_alln(<>,c) -> ;
%% enum_alln(< <d,p> >.t,c) ->
%% enum_periodn(p,c)
%% val(min_price,x) 
%% enum_alln(t,c) ,
%% { c<=x-1 } ;
%% 
%% enum_all(t,c) ->
%% enum_alln(t,c) ;
%% 
%% /* Main Predicates */
%% 
%% setmin(c,d) ->
%% val(min_price,x) / ,
%% { c>=x } ;
%% setmin(c,d) ->
%% assign(min_price,c)
%% cpu_time(t)
%% outm("New best value : ")
%% outl(c)
%% outm("CPU time       : ")
%% outl(t) ;
%% 
%% pow(c) ->
%% assign(min_price,100000000)
%% outml("Preparing data ...")
%% make_data(d)
%% outml("Setting constraints ...")
%% calc_prod_cost(d,0,c)
%% calc_start(d)
%%         min_value(c,c') outl(c')
%% outml("Making choices ...")
%% reset_cpu_time
%% enum_all(d,c)
%% min_value(c,c)
%% setmin(c,d)
%% fail ,
%% { c>=0  } ;
%% pow(c) ->
%% val(min_price,c)
%% cpu_time(t)
%% outm("Final CPU time : ")
%% outl(t) ;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            Translated Version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% :- qmode(pow(C),[mode(C,f)]).


powerStationsSmall([
        [1,12,12,850,2000,10000,20,20000],
        [2,3,9,1250,1750,26000,13,10000],
        [3,0,2,1500,4000,30000,30,5000]]).

powerStations([
        [1,0,12,850,2000,10000,20,20000],
        [2,0,10,1250,1750,26000,13,10000],
        [3,0,5,1500,4000,30000,30,5000]]).

periods([
        [1,6,15000],
        [2,3,30000],
        [3,6,25000],
        [4,3,40000],
        [5,6,27000]]).

make_period(D, [], P, [D,P]).
make_period(D, [S|T], P, R) :-
        S = [I,M,M1,P1,P2,C1,C2,C3],
        N>=M,
        N=<M1,
        P_>=N*P1,
        P_=<N*P2,
        make_period(D, T, [[I,N,S_,P_]|P], R).

make_day(G4, S, D, D1) :-
        piii(G4,[[]]),
	D = D1.
make_day(G4, S, D, R) :-
        piii(G4, [[P],T]),
        piii(H4,[[]]),
        make_period(P, S, H4, R1),
        piii(I4, [[R1],D]),
        make_day(T, S, I4, R).

make_data(D) :-
        periods(P),
        powerStations(S),
        piii(G4,[[]]),
        make_day(P, S, G4, D).

calc_station(S, H, C, P1, M1) :-
        piii(S, [[I,N,S1,P]]),
        M1-N*P2=0,
        P1=P,
%%      c=h*(n*c1+(p-n*p1)*c2)+s'*c3 } ;
	H4 - P +N*P1=0,
	I4 - N*C1 - H4*C2 =0,
        C-H*I4-S1*C3=0,
        powerStations(T),
        piii(G4, [[I,M,M2,P1,P2,C1,C2,C3]]),
        member(G4, T).

calc_period(G4, H, C, C1, P, P1, M, M1) :-
        piii(G4,[[]]),
	C = C1,
	P = P1,
	M = M1.
calc_period(G4, H, C, C111, P, P111, M, M111) :-
        piii(G4, [[S],T]),
        C11-C-C1=0,
        M11-M-M1=0,
        P11-P-P1=0,
        calc_station(S, H, C1, P1, M1),
        calc_period(T, H, C11, C111, P11, P111, M11, M111).

calc_prod_cost(G4, C, C1) :-
        piii(G4,[[]]),
	C = C1.
calc_prod_cost(G4, C, C111) :-
        piii(G4, [[[D,P]],T]),
        C11-C-C1=0,
        piii(D, [[I,H,U]]),
        P1-U>=0,
        P11-115/100*U>=0,
        H4=0,
        I4=0,
        J4=0,
        calc_period(P, H, H4, C1, I4, P1, J4, P11),
        calc_prod_cost(T, C11, C111).

start_period(G4, H4) :-
        piii(G4,[[]]),
        piii(H4,[[]]).
start_period(G4, H4) :-
        piii(G4, [[S1],T1]),
        piii(H4, [[S2],T2]),
        piii(S1, [[I1,N1,U1,P1]]),
        piii(S2, [[I2,N2,U2,P2]]),
        U1>=0,
        U1-N1+N2>=0,
        U1-N1=<0,
        start_period(T1, T2).

start_day(G4, F) :-
        piii(G4, [[[D1,P1]]]),
        start_period(P1, F).
start_day(G4, F) :-
        piii(G4, [[[D1,P1],[D2,P2]],T]),
        start_period(P1, P2),
        piii(H4, [[[D2,P2]],T]),
        start_day(H4, F).

calc_start(G4) :-
        piii(G4,[[]]).
calc_start(G4) :-
        piii(G4, [[[D,P]],T]),
        start_day(G4, P).

member(X, G4) :-
        piii(G4, [[X],T]).
member(X, G4) :-
        piii(G4, [[Y],T]),
        member(X, T).

anum(G4) :-
        G4=0.
anum(G4) :-
        G4=1.
anum(G4) :-
        G4=2.
anum(G4) :-
        G4=3.
anum(G4) :-
        G4=4.
anum(G4) :-
        G4=5.
anum(G4) :-
        G4=6.
anum(G4) :-
        G4=7.
anum(G4) :-
        G4=8.
anum(G4) :-
        G4=9.
anum(G4) :-
        G4=10.
anum(G4) :-
        G4=11.
anum(G4) :-
        G4=12.

enum_periodn(G4, C) :-
        piii(G4,[[]]),
        C-X+1=<0,
        H4=min_price,
        val(H4, X).
enum_periodn(G4, C) :-
        piii(G4, [[[I,N,S,P]],R]),
        C-X+1=<0,
        anum(N),
        H4=min_price,
        val(H4, X),
        enum_periodn(R, C).

enum_alln(G4, C) :-
        piii(G4,[[]]).
enum_alln(G4, C) :-
        piii(G4, [[[D,P]],T]),
        C-X+1=<0,
        enum_periodn(P, C),
        H4=min_price,
        val(H4, X),
        enum_alln(T, C).

enum_all(T, C) :-
        enum_alln(T, C).

setmin(C, D) :-
        C-X>=0,
        G4=min_price,
        val(G4, X), !.
setmin(C, D) :-
        G4=min_price,
        assign(G4, C),
        cpu_time(T),
        H4='New best value : ',
        outm(H4),
        outl(C),
        I4='CPU time       : ',
        outm(I4),
        outl(T).

pow(C) :-
        C>=0,
        G4=min_price,
        H4=100000000,
        assign(G4, H4),
        I4='Preparing data ...',
        outml(I4),
        make_data(D),
        J4='Setting constraints ...',
        outml(J4),
        K4=0,
        calc_prod_cost(D, K4, C),
        calc_start(D),
        min_value(C, C1),
        outl(C1),
        L4='Making choices ...',
        outml(L4),
        reset_cpu_time,
        enum_all(D, C),
	C = C2,
        min_value(C, C2),
        setmin(C, D),
        fail.
pow(C) :-
        G4=min_price,
        val(G4, C),
        cpu_time(T),
        H4='Final CPU time : ',
        outm(H4),
        outl(T).
