/* a regular expression parser */

generate(empty,T,T).

test(S) :-  generate(or(char(a),star(char(b))),S,[]).