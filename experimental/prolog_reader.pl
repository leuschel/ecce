:- module(prolog_reader,
          [load_file/1, is_user_pred/1, is_built_in/1,
           get_clause/3,get_clause_as_list/2, get_clause_as_list/3,
           convert_to_list/2,
           hnf/2]).


load_file(File) :-
    (File=argv(_) -> (print('% ### Please supply filename as argument'),nl,fail)
                  ;  true),
	prolog_flag(single_var_warnings, Old, off),
	prolog_flag(redefine_warnings,OldR,off),
    load_files(File,[compilation_mode(assert_all)]),
    %print('% loaded file: '),print(File),nl,
	set_prolog_flag(single_var_warnings, Old),
	set_prolog_flag(redefine_warnings, OldR).
	
	
is_user_pred(P) :- current_predicate(_,P), predicate_property(P,dynamic).
is_built_in(A) :- nonvar(A), \+(is_user_pred(A)).

get_clause(Head,Body,Ref) :- is_user_pred(Head),
	clause(Head,Body,Ref).
	
get_clause_as_list(Head,ListBody) :- get_clause_as_list(Head,ListBody,_).
get_clause_as_list(Head,ListBody,Ref) :-
	clause(Head,Body,Ref),
	convert_to_list(Body,ListBody).

convert_to_list((A,B), [A| BRest]) :-
	!,
	convert_to_list(B,BRest).
convert_to_list(true, []).
convert_to_list(A, [A]) :-
	A \= true.


hnf(Call,(Call :- HNF) ) :- head_normal_form(Call,HNF).

head_normal_form(Call,HNF) :-
   findall(clause(Call,Body),clause(Call,Body), HNF_List),
   convert_list_into_disjunction(HNF_List,Call,HNF).

convert_list_into_disjunction([],_Call,false).
convert_list_into_disjunction([H],Call,HD) :- generate_disjunct(Call,H,HD).
convert_list_into_disjunction([H,H2|T],Call,';'(HD,Rest)) :-
   generate_disjunct(Call,H,HD),
   convert_list_into_disjunction([H2|T],Call,Rest).

%:- use_module(library(terms),[term_variables/2]).
generate_disjunct(Call,clause(Copy,Body),Disjunct) :-
                  generate_equality_formula(Call,Copy,EqFormula),
                  clever_and(EqFormula,Body,Res),
                  Res = Disjunct.
             %     term_variables(Copy,CallVars),
             %     get_free_variables(Body,[],CallVars,FreeVars),
             %     generate_exists(FreeVars,Res,Disjunct).

generate_equality_formula(A,B,Formula) :-
  A =.. [Func|AArgs],
  B =.. [Func|BArgs],
  gen_equalities(AArgs,BArgs,Formula).

gen_equalities([],[],true).
gen_equalities([A|TA],[B|TB],Res) :-
   gen_equalities(TA,TB,TT),
   (A==B -> Res=TT
         ;  clever_and('='(A,B),TT,Res)
   ).

clever_and(true,X,X).
clever_and(Y,true,Y) :- Y \= true.
clever_and(X,Y,','(X,Y)) :- X \= true, Y \= true.