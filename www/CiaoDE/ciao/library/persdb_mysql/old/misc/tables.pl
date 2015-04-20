:-use_module(library('persdb_sql/misc/pl2sql_orig')).
%% :-use_module(library('persdb_sql/pl2sql')).

:- multifile([relation/3,attribute/4]).
:- data([relation/3,attribute/4]).

% --- Meta Database for schema definition of SQL DB in Prolog ---------------
%
% maps Prolog predicates to SQL table names, Prolog predicate argument 
% positions to SQL attributes, and Prolog operators to SQL operators. 
% ATTENTION! It is assumed that the arithmetic operators in Prolog and 
% SQL are the same, i.e. + is addition in Prolog and in SQL, etc. If this 
% is not the case, then a mapping function for arithmetic operators is 
% necessary too.
% --------------------------------------------------------------------------

% --- relation(PrologFunctor,Arity,SQLTableName) ----------------
% --- attribute(PrologArgumentPosition,SQLTableName,SQLAttributeName) ---

%%---------------------------------------------------------------------%%
%%------------------  EMPLOYEES  --------------------------------------%%
%%---------------------------------------------------------------------%%
relation(product,2,'PRODUCT').
attribute(1,'PRODUCT','ID',integer).
attribute(2,'PRODUCT','QUANTITY',integer).

relation(department,2,'DEPARTMENT').
attribute(1,'DEPARTMENT','DEPT_ID',integer).
attribute(2,'DEPARTMENT','DEPT_NAME',string).

relation(employee,5,'EMPLOYEE').
attribute(1,'EMPLOYEE','EMP_ID',integer).
attribute(2,'EMPLOYEE','EMP_FNAME',string).
attribute(3,'EMPLOYEE','EMP_LNAME',string).
attribute(4,'EMPLOYEE','CITY',string).
attribute(5,'EMPLOYEE','DEPT_ID',integer).

%%---------------------------------------------------------------------%%
%%------------------  FLIGHTS  ----------------------------------------%%
%%---------------------------------------------------------------------%%

relation(flight,4,'FLIGHT').
relation(plane,2,'PLANE').
attribute(1,'FLIGHT','FLIGHT_NO',string).
attribute(2,'FLIGHT','DEPARTURE',string).
attribute(3,'FLIGHT','DESTINATION',string).
attribute(4,'FLIGHT','PLANE_TYPE',string).
attribute(1,'PLANE','TYPE',string).
attribute(2,'PLANE','SEATS',integer).

%%---------------------------------------------------------------------%%
%%------------------  BULLS -------------------------------------------%%
%%---------------------------------------------------------------------%%

relation(plazas,3,'PLAZAS').
relation(ferias,3,'FERIAS').
relation(lidias_de_toros,6,'LIDIAS_TOROS').
relation(toreros,3,'MATADORES').
relation(ganaderos,2,'GANADEROS').

attribute(1,'PLAZAS','PLAZA',string).
attribute(2,'PLAZAS','LOCALIDAD',string).
attribute(3,'PLAZAS','CAPACIDAD',integer).

attribute(1,'FERIAS','FERIA',string).
attribute(2,'FERIAS','PLAZA',string).
attribute(3,'FERIAS','MES',string).

attribute(1,'LIDIAS_TOROS','TORO',string).
attribute(2,'LIDIAS_TOROS','PESO',integer).
attribute(3,'LIDIAS_TOROS','GANADERO',string).
attribute(4,'LIDIAS_TOROS','FERIA',string).
attribute(5,'LIDIAS_TOROS','FECHA_TOREO',string).
attribute(6,'LIDIAS_TOROS','TORERO',string).

attribute(1,'MATADORES','TORERO',string).
attribute(2,'MATADORES','APODERADO',string).
attribute(3,'MATADORES','PROCEDENCIA',string).

attribute(1,'GANADEROS','GANADERO',string).
attribute(2,'GANADEROS','LOCALIDAD',string).
	
%% EJEMPLO: Capacidad y nombre de las plazas en las que se han toreado toros de la ganaderi'a de Atanasio Ferna'ndez

%% :-definicion_ext(plazas_Atanasio(Nom,Cap),
%%		 (plazas(Nombre_Plaza,Loc,Cap),
%%		  lidias_de_toros(Toro,Peso,'Atanasio Fernandez',Feria,Fecha,Torero),
%%		  ferias(Feria,Nombre_Plaza,Mes)).

%% :- definicion_ext(pred(X,Y,Z,V),(ext1(X,Y,Z),ext2(V),ext3(X,V))).
%% :- definicion_ext(pred(X,Y,Z,V),(ext1(X,Y,Z),ext2(Y,munich,3),ext3(V))).
    %% Redefinir dos veces es como un or. Incorporar tambien el ; para las or..
