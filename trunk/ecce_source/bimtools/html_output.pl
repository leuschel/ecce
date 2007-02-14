
/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */


/* file: html_output.pro */


:- dynamic output_html/1.
output_html(no).

/* ---------- */
/* set_html/0 */
/* ---------- */

set_html :-
	print('Output HTML code:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	output_html(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((not(NewValue=yes),not(NewValue=no))
	 -> (print('Illegal value'),nl)
	 ;  (set_output_html(NewValue))
	).

set_output_html(_NewVal) :-
	retract(output_html(_Cur)),
	fail.
set_output_html(NewVal) :-
	asserta(output_html(NewVal)).


print_html(HTMLText) :-
   (output_html(yes) ->
     print(HTMLText) ; true).


print_command(Cmd,Text) :-
  print_html('<'),print_html(Cmd),
  print_html('>'),
  print(Text),
  print_html('</'),print_html(Cmd),
  print_html('>').


print_bold(X) :-
  print_command('B',X).

print_em(X) :-
  print_command('EM',X).

print_yellow(X) :-
	print_html('<font color="yellow">'),
	print(X),
	print_html('</font>').
print_orange(X) :-
	print_html('<font color="orange">'),
	print(X),
	print_html('</font>').
print_green(X) :-
	print_html('<font color="green">'),
	print(X),
	print_html('</font>').
print_red(X) :-
	print_html('<font color="red">'),
	print(X),
	print_html('</font>').

html_begin_yellow :- print_html('<font color="yellow">').
html_begin_orange :- print_html('<font color="orange">').
html_begin_green :- print_html('<font color="green">').

html_end_color :- print_html('</font>').

newparagraph :- print_html('<P>'),nl.
newlinebreak :- print_html('<BR>'),nl.