%% :- module(predicates,[relation/3,attribute/4]).
:- multifile relation/3.
:- multifile attribute/4.
relation(product,5,'PRODUCT').
attribute(1,'PRODUCT','ID',integer).
attribute(2,'PRODUCT','NAME',string).
attribute(3,'PRODUCT','QUANTITY',integer).
attribute(4,'PRODUCT','COLOR',string).
attribute(5,'PRODUCT','UNIT_PRICE',float).
%% predicate example: product(300,'Tee Shirt',28,'White',9.00).

relation(employee,3,'EMPLOYEE').
attribute(1,'EMPLOYEE','EMP_LNAME',string).
attribute(2,'EMPLOYEE','START_DATE',date).
%% date is a type to be defined and checked, at the moment it works without type checking.
attribute(3,'EMPLOYEE','SALARY',float).
%% predicate example: employee('Barletta','1998-7-18',45450), o lo que es igual 
%%          employee('Barletta','1998-07-18',45450.0000)  
