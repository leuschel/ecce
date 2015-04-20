%% queries de ejemplo. Ignacio

Ejemplo 2D
pl2sql(capacity(No,Dep,Dest,Type,Seats),
	  (flight(No,Dep,Dest,Type),
	   plane(Type,Seats),
	   Type='b-737'),
	  SQLQueryTerm).

SELECT rel1.FLIGHT_NO , rel1.DEPARTURE , rel1.DESTINATION , rel1.PLANE_TYPE , rel2.SEATS
FROM FLIGHT rel1 , PLANE rel2
WHERE rel2.TYPE = rel1.PLANE_TYPE AND rel1.PLANE_TYPE = "b-737"
;

Modificacion (prueba)

T='b-737',
pl2sql(capacity(No,Dep,Dest,Type,Seats),
	  (flight(No,Dep,Dest,Type),
	   plane(Type,Seats),
	   Type=T),
	  SQLQueryTerm).

SELECT rel1.FLIGHT_NO , rel1.DEPARTURE , rel1.DESTINATION , rel1.PLANE_TYPE , rel2.SEATS
FROM FLIGHT rel1 , PLANE rel2
WHERE rel2.TYPE = rel1.PLANE_TYPE AND rel1.PLANE_TYPE = "b-737"
;
               

Ejemplo 3D (NOT)
pl2sql(no_planes(No,Dep,Dest,Type),
	  (flight(No,Dep,Dest,Type), \+ plane(Type,Seats)),
	  SQLQueryTerm).

SELECT rel1.FLIGHT_NO , rel1.DEPARTURE , rel1.DESTINATION , rel1.PLANE_TYPE
FROM FLIGHT rel1
WHERE NOT EXISTS
(SELECT *
FROM PLANE rel2
WHERE rel2.TYPE = rel1.PLANE_TYPE
)
;


Ejemplo 4D
pl2sql(X,X is count(S,plane(P,S)),SQLQueryTerm).

SELECT COUNT(rel1.SEATS)
FROM PLANE rel1

GROUP BY rel1.TYPE;

Ejemplo 5D
pl2sql(
	  big_planes(munich,Dest,Type,Seats),
	  FNo^(
	       flight(FNo,munich,Dest,Type),
	       plane(Type,Seats),
	       Seats > avg(S,T^plane(T,S))),
	  SQLQueryTerm).

SELECT "munich" , rel1.DESTINATION , rel1.PLANE_TYPE , rel2.SEATS
FROM FLIGHT rel1 , PLANE rel2
WHERE rel1.DEPARTURE = "munich" AND rel2.TYPE = rel1.PLANE_TYPE AND rel2.SEATS > 
(SELECT AVG(rel3.SEATS)
FROM PLANE rel3

)
;


pl2sql(
	  big_planes(munich,Dest,Type,Seats),
	  (mayor60(Seats)),
	  SQLQueryTerm).

LOS PREDICADOS DE ESTE TIPO FALLAN: LA RAZON ES QUE LAS VARIABLES DEL ProjectionTerm DEBEN ESTAR ASOCIADOS A UNA TABLA POR MEDIO DEL DatabaseGoal

?- pl2sql(pred(X,Y,Z,V),(ext1(X,Y,Z),ext2(V),ext3(X,V)),S).

no

?- sc:pl2sql(pred(X,Y,Z,V),(X>Y,Z<V),T).

no

pl2sql(
	  big_planes(munich,Dest,Type,Seats),
	  (flight(FNo,munich,Dest,Type),plane(Type,Seats),Seats=60),
	  SQLQueryTerm).
SELECT "munich" , rel1.DESTINATION , rel1.PLANE_TYPE , rel2.SEATS
FROM FLIGHT rel1 , PLANE rel2
WHERE rel1.DEPARTURE = "munich" AND rel2.TYPE = rel1.PLANE_TYPE AND rel2.SEATS = 60
;


         


pl2sql(
	  flight(No,Dep,Dest,Type),
	  flight(No,Dep,Dest,Type),
	  SQLQueryTerm).
%% ver resultado1


pl2sql(
	  big_planes(munich,Dest,Type,Seats),
	  (flight(No,munich,Dest,Type),plane(Type,Seats),Seats>avg(S,plane(T,S)))
	  ,SQLQueryTerm).
%% ver resultado3

pl2sql(
	  big_planes(munich,Dest,Type,Seats),
	  (flight(No,munich,Dest,Type),plane(Type,Seats),Seats>60)
	  ,SQLQueryTerm).
%% ver resultado2

Resultado 2
===========
?- pl2sql(
	  big_planes(munich,Dest,Type,Seats),
	  (flight(No,munich,Dest,Type),plane(Type,Seats),Seats>60)
	  ,SQLQueryTerm).
         
SELECT "munich" , rel1.DESTINATION , rel1.PLANE_TYPE , rel2.SEATS
FROM FLIGHT rel1 , PLANE rel2
WHERE rel1.DEPARTURE = "munich" AND rel2.TYPE = rel1.PLANE_TYPE AND rel2.SEATS > 60
;


Resultado 1
===========
?- pl2sql(
	  flight(No,Dep,Dest,Type),
	  flight(No,Dep,Dest,Type),
	  SQLQueryTerm).
         
Dep = '$var$'(var2),
Dest = '$var$'(var3),
No = '$var$'(var1),
SQLQueryTerm = [query([att(rel1,'FLIGHT_NO'),att(rel1,'DEPARTURE'),att(rel1,'DESTINATION'),att(rel1,'PLANE_TYPE')],[rel('FLIGHT',rel1)],[])],
Type = '$var$'(var4) ? ;

no

?- printqueries([query([att(rel1,'FLIGHT_NO'),att(rel1,'DEPARTURE'),att(rel1,'DESTINATION'),att(rel1,'PLANE_TYPE')],[rel('FLIGHT',rel1)],[])]).

SELECT rel1.FLIGHT_NO , rel1.DEPARTURE , rel1.DESTINATION , rel1.PLANE_TYPE
FROM FLIGHT rel1

;

yes
 
?- 

Resultado3
==========
?- pl2sql(
	  big_planes(munich,Dest,Type,Seats),
	  (flight(No,munich,Dest,Type),plane(Type,Seats),Seats>avg(S,plane(T,S)))
	  ,SQLQueryTerm).

SELECT "munich" , rel1.DESTINATION , rel1.PLANE_TYPE , rel2.SEATS
FROM FLIGHT rel1 , PLANE rel2
WHERE rel1.DEPARTURE = "munich" AND rel2.TYPE = rel1.PLANE_TYPE AND rel2.SEATS > 
(SELECT AVG(rel3.SEATS)
FROM PLANE rel3

GROUP BY rel3.TYPE)
;


?- 
Revisando tokenize_term
=======================


?- tokenize_term(big_planes(munich,Dest,Type,Seats),T).
?- tokenize_term((flight(No,munich,Dest,Type),plane(Type,Seats),Seats>60),T).

?- tokenize_term((plane(Type,Seats),Seats>60),T).

Seats = '$var$'(var10),
T = (plane('$var$'(var9),'$var$'(var10)),'$var$'(var10)>'$const$'(60)),
Type = '$var$'(var9) ? ;

no
?- tokenize_term((flight(No,munich,Dest,Type),plane(Type,Seats),Seats>60),T).

Dest = '$var$'(var6),
No = '$var$'(var5),
Seats = '$var$'(var8),
T = (flight('$var$'(var5),'$const$'(munich),'$var$'(var6),'$var$'(var7)),plane('$var$'(var7),'$var$'(var8)),'$var$'(var8)>'$const$'(60)),
Type = '$var$'(var7) ? ;

no
