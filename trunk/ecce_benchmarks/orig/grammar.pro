/* grammar.pro */


expression( Term, Qualifiers ) -->
    value( Term ), qualification( Qualifiers ).
expression( Term, [] ) -->
    value( Term ).
/* Only the last rule is applicable to the query given. */

value( Term ) --> identifier( Term ).
value( Term ) --> numeric( Term ).
value( Term ) --> built_in( Term ).
value( Term ) --> bracketted( Term ).
value( subscripted( Term, Subscript ) ) -->
    identifier( Term ),
    start_subscript, value( Subscript ),
    end_subscript.
value( Term ) --> leftparen, 
	value( Term ), rightparen.
/* Only the first and last rules are applicable for the query given. */
/* Rules for 'numeric', 'built_in' and 'bracketted' are not used for 
   the given query. */

qualification( merge_qualifiers( Qualifier, Qualifiers ) ) -->
    qualifier( Qualifier ), 
    qualification( Qualifiers ).
qualification( Qualifier ) -->
    qualifier( Qualifier ).

qualifier( such_that( Left, Right ) ) -->
    such_that, value( Left ), equals, value( Right ).
qualifier( for_all( Term ) ) --> for_all, subrange( Term ).
qualifier( when( Term ) )    --> when, value( Term ).
qualifier( where( Term ) )   --> where, is_a( Term ).
/* None of these grammar rules turn out to be applicable for the query used. */

leftparen --> ['('].
rightparen --> [')'].
leftbracket --> ['['].
rightbracket --> [']'].
colon --> [':'].
semicolon --> [';'].
dotdot --> ['..'].
comma --> [','].
equals --> ['='].
where --> [where].
when --> [when].
is_a --> [is].
such_that --> [suchthat].
such_that --> [such], [that].
for_all --> [forall].
for_all --> [for], [all].
start_subscript --> ['{'].
end_subscript --> ['}'].

identifier( Identifier ) --> common_function( Identifier ).
identifier( Identifier ) --> common_variable( Identifier ).
/* None of the rules for 'common_function' turn out to be applicable
   for the query used. */

common_variable( a ) --> [a].
common_variable( n ) --> [n].
common_variable( f ) --> [f].
common_variable( v ) --> [v].
common_variable( x ) --> [x].
common_variable( i ) --> [i].
common_variable( shp ) --> [shp].
common_variable( arg ) --> [arg].
common_variable( res ) --> [res].


/* The following definitions are missed from the program, but they are
   needed to be able to run the program. */
numeric( _, nomatch, _ ).
built_in( _, nomatch, _ ).
bracketted( _, nomatch, _ ).
common_function( _, nomatch, _ ).


