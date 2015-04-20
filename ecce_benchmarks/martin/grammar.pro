/* grammar.pro */


grammar:expression( Term, Qualifiers ) -->
    grammar:value( Term ), qualification( Qualifiers ).
grammar:expression( Term, [] ) -->
    grammar:value( Term ).
/* Only the last rule is applicable to the query given. */

grammar:value( Term ) --> grammar:identifier( Term ).
grammar:value( Term ) --> grammar:numeric( Term ).
grammar:value( Term ) --> grammar:built_in( Term ).
grammar:value( Term ) --> grammar:bracketted( Term ).
grammar:value( subscripted( Term, Subscript ) ) -->
    grammar:identifier( Term ),
    grammar:start_subscript, value( Subscript ),
    grammar:end_subscript.
grammar:value( Term ) --> grammar:leftparen, 
	grammar:value( Term ), grammar:rightparen.
/* Only the first and last rules are applicable for the query given. */
/* Rules for 'numeric', 'built_in' and 'bracketted' are not used for 
   the given query. */

grammar:qualification( merge_qualifiers( Qualifier, Qualifiers ) ) -->
    grammar:qualifier( Qualifier ), 
    grammar:qualification( Qualifiers ).
grammar:qualification( Qualifier ) -->
    grammar:qualifier( Qualifier ).

grammar:qualifier( such_that( Left, Right ) ) -->
    grammar:such_that, value( Left ), grammar:equals, grammar:value( Right ).
grammar:qualifier( for_all( Term ) ) --> grammar:for_all, grammar:subrange( Term ).
grammar:qualifier( when( Term ) )    --> grammar:when, grammar:value( Term ).
grammar:qualifier( where( Term ) )   --> grammar:where, grammar:is_a( Term ).
/* None of these grammar rules turn out to be applicable for the query used. */

grammar:leftparen --> ['('].
grammar:rightparen --> [')'].
grammar:leftbracket --> ['['].
grammar:rightbracket --> [']'].
grammar:colon --> [':'].
grammar:semicolon --> [';'].
grammar:dotdot --> ['..'].
grammar:comma --> [','].
grammar:equals --> ['='].
grammar:where --> [where].
grammar:when --> [when].
grammar:is_a --> [is].
grammar:such_that --> [suchthat].
grammar:such_that --> [such], [that].
grammar:for_all --> [forall].
grammar:for_all --> [for], [all].
grammar:start_subscript --> ['{'].
grammar:end_subscript --> ['}'].

grammar:identifier( Identifier ) --> grammar:common_function( Identifier ).
grammar:identifier( Identifier ) --> grammar:common_variable( Identifier ).
/* None of the rules for 'common_function' turn out to be applicable
   for the query used. */

grammar:common_variable( a ) --> [a].
grammar:common_variable( n ) --> [n].
grammar:common_variable( f ) --> [f].
grammar:common_variable( v ) --> [v].
grammar:common_variable( x ) --> [x].
grammar:common_variable( i ) --> [i].
grammar:common_variable( shp ) --> [shp].
grammar:common_variable( arg ) --> [arg].
grammar:common_variable( res ) --> [res].


/* The following definitions are missed from the program, but they are
   needed to be able to run the program. */
grammar:numeric( _, nomatch, _ ).
grammar:built_in( _, nomatch, _ ).
grammar:bracketted( _, nomatch, _ ).
grammar:common_function( _, nomatch, _ ).

