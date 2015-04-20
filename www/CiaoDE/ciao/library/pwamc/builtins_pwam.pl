/*             Copyright (C)1990-2002 UPM-CLIP				*/

%----------------------------------------------------------------------------
% Next were used in plpwam.pl and pwamql.pl
%----------------------------------------------------------------------------

% The following predicates are compiled using special instructions.

:- mode inline_codable(+).
:- mode name_of_builtin(+,?,?).
:- mode eval_builtin(+).
:- mode name_of_function(+,?,?).

inline_codable(true).
inline_codable(otherwise).
inline_codable(false).
inline_codable(fail).
inline_codable('$choice'(_)).
inline_codable('$cut'(_)).
inline_codable('$dcut'(_)).
inline_codable(_ is _).
inline_codable(_ = _).
inline_codable('C'(_,_,_)).
%% inline_codable(ground(_)).
%% inline_codable(ground(_,_)).
%% inline_codable(ground(_,_,_)).
%% inline_codable(indep(_,_)).
inline_codable(available_siblings).
% ---- Added for tracing... MH
inline_codable('$trace_fork'(_)).
inline_codable('$trace_goal_success'(_)).
inline_codable('$trace_join'(_)).
%% [MCL:BUILTIN_INC]
inline_codable('$inc'(_,_)).
%--------------------------------------------------------------
inline_codable('$label1').
inline_codable('$label1_no_label2').
inline_codable('$label2').
%--------------------------------------------------------------
inline_codable(X) :- 
	functor(X,N,Ar), name_of_builtin(N,Ar,_).

% The following predicates are trusted to preserve all arguments.
name_of_builtin(@>,2,31).
name_of_builtin(@=<,2,32).
name_of_builtin(=:=,2,33).
name_of_builtin(=\=,2,34).
name_of_builtin(<,2,35).
name_of_builtin(>=,2,36).
name_of_builtin(>,2,37).
name_of_builtin(=<,2,38).
name_of_builtin(name,2,39).
name_of_builtin(=..,2,40).
name_of_builtin(arg,3,41).
name_of_builtin(compare,3,42).
name_of_builtin(functor,3,43).
name_of_builtin(type,2,44).
name_of_builtin(get_attribute,2,45).
name_of_builtin(attach_attribute,2,46).
name_of_builtin(detach_attribute,1,47).
name_of_builtin(update_attribute,2,48).
name_of_builtin(atom,1,49).
name_of_builtin(atomic,1,50).
name_of_builtin(float,1,51).
name_of_builtin(integer,1,52).
name_of_builtin(nonvar,1,53).
name_of_builtin(number,1,54).
name_of_builtin(var,1,55).
name_of_builtin(==,2,56).
name_of_builtin(\==,2,57).
name_of_builtin(@<,2,58).
name_of_builtin(@>=,2,59).

/*
MCL 12-91 This is to be uncommented when these C predicate become
builtins. The numbers must agree with the ones in the initial.c table.
Also, check how other builtins are implemented & fill the proper tables.
*/

%% name_of_builtin(nground,1,44).
%% name_of_builtin(nindep,1,45).
%% name_of_builtin(nindep,2,46).


eval_builtin(=:=).
eval_builtin(=\=).
eval_builtin(<).
eval_builtin(>=).
eval_builtin(>).
eval_builtin(=<).

name_of_function(-,1,0).
name_of_function(+,1,1).
name_of_function('SUB1',1,2).
name_of_function('ADD1',1,3).
name_of_function(integer,1,4).
name_of_function(float,1,5).
name_of_function(\,1,6).
name_of_function(abs,1,7).
name_of_function(log2,1,8).
name_of_function(*,2,9).
name_of_function(/,2,10).
name_of_function(//,2,11).
name_of_function(mod,2,12).
name_of_function(^,2,13).
name_of_function(/\,2,14).
name_of_function(\/,2,15).
name_of_function(<<,2,16).
name_of_function(>>,2,17).
name_of_function(gcd,2,18).
name_of_function(-,2,19).
name_of_function(max,2,20).
name_of_function(min,2,21).
name_of_function(pow,2,22).
name_of_function(+,2,23).
