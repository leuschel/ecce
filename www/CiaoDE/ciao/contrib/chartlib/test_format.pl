:- module(test_format,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").


:- export(equalnumber/3).
:- export(not_empty/4).
:- export(not_empty/3).
:- export(check_sublist/4).
:- export(valid_format/4).
:- export(vectors_format/4).
:- export(valid_vectors/4).
:- export(length_element/2).
:- export(valid_attributes/2).
:- export(valid_elementlist/2).
:- export(valid_table/2).
:- export(add_doublequotes/2).

:- use_module(library('chartlib/bltclass')).
:- use_module(library(lists)).

:- comment(module,"Most of the predicates exported by this module perform
	some checks to determine whether the arguments attain some
	conditions or not. In the second case an exception will be
	thrown. To catch the exceptions you can use the following
	metapredicates when invoking chartlib exported predicates:

	@begin{itemize}

        @item @pred{chartlib_text_error_protect/1}

        @item @pred{chartlib_text_error_protect/1}
 
        @end{itemize}

        Both metapredicates are defined in the chartlib_errhandle module
        that comes with this library. Some of the predicates have a
        @var{Predicate} argument which will be used in case of error to
        show which chartlib predicate causes the error.

").

:- pred equalnumber(+list,+list,+callable).

:- comment(equalnumber(X,Y,Predicate),"Test whether the list @var{X} and the
	list @var{Y} contain the same number of elements.

").
equalnumber(X,Y,_Predicate):-
	length(X,Number),
	length(Y,Number),
	!.
 	
equalnumber(_,_,Predicate):-
	throw(error(chartlib,error1,Predicate)).
	
:- push_prolog_flag(multi_arity_warnings,off).

:- pred not_empty(+list,+list,+list,+callable).

:- comment(not_empty(X,Y,Z,Predicate),"Tests whether at least one the lists
	@var{X}, @var{Y} or @var{Z} are empty.

").

not_empty(X,Y,Z,Predicate):-
	not_empty(X,Predicate),
	not_empty(Y,Predicate),
	not_empty(Z,Predicate).

:- pred not_empty(+list,+list,+callable).

:- comment(not_empty(X,Y,Predicate),"Tests whether the lists @var{X} or
	@var{Y} are empty.

").

not_empty(X,Y,Predicate):-
	not_empty(X,Predicate),
	not_empty(Y,Predicate).

:- pred not_empty(+list,+callable).

:- comment(not_empty(X,Predicate),"Tests if the list @var{X} is an empty
	list.

").
not_empty([],Predicate):-
	!,
	throw(error(chartlib,error2,Predicate)).
not_empty(_,_Predicate).



:- pred check_sublist(+list,+integer,+integer,+atm).

:- comment(check_sublist(List,Number,Number,Predicate), "Tests if the
	number of elements in each sublist of @var{List} is @var{Number1}
	or @var{Number2}.

"). 

check_sublist([],_N1,_N2,_Predicate).
check_sublist([X|Xs],N1,N2,Predicate):-
	check_element(X,N1,N2),
	!,
	check_sublist(Xs,N1,N2,Predicate).
check_sublist(_,_N1,_N2,Predicate):-
	throw(error(chartlib,error3,Predicate)).
	
check_element(X,N1,_N2):-
	length(X,N1).
check_element(X,_N1,N2):-
	length(X,N2).

check_sublist1([],_N).
check_sublist1([X|Xs],N):-
	length(X,N),
	check_sublist1(Xs,N).

check_sublist1([],_N1,_N2).
check_sublist1([X|Xs],N1,N2):-
	check_element(X,N1,N2),
	check_sublist1(Xs,N1,N2).

check_sublist2([]).
check_sublist2([X|Xs]):-
	length(X,5),
	check_sublist2(Xs).
check_sublist2([X|Xs]):-
	length(X,3),
	check_sublist2(Xs).
check_sublist2([X|Xs]):-
	length(X,1),
	check_sublist2(Xs).


:- pop_prolog_flag(multi_arity_warnings).

:- pred valid_format(+list,+list,+list,+callable).

:- comment(valid_format(XVector,YVector,BarsAttributes,Predicate), "Tests
	the following restrictions:

        @begin{itemize}

        @item The @var{XVector} number of elements is the same as each
        @var{YVector} sublist number of elements.

        @item The @var{YVector} length is equal to @var{BarsAttributes} length.

        @end{itemize}
").

valid_format(XVector,YVector,BarsAttributes,_Predicate):-
	length(XVector,Xlength),
	length(YVector,Ylength),
	length(BarsAttributes,Ylength),
	check_sublist1(YVector,Xlength).
valid_format(_XVector,_YVector,_BarsAttributes,Predicate):-
	throw(error(chartlib,error5,Predicate)).
	


	
:- pred valid_attributes(+list,+callable).

:- comment(valid_attributes(BarsAttibuttes,Predicate),"Check if each
	@var{BarsAttibuttes} element is a list composed of one or four
	elements.

"). 



valid_attributes(BarsAttibuttes,_Predicate):-
	check_sublist1(BarsAttibuttes,1,4).
valid_attributes(_BarsAttibuttes,Predicate):-
	throw(error(chartlib,error6,Predicate)).
	

:- push_prolog_flag(multi_arity_warnings,off).

:- pred vectors_format(+list,+list,+list,+callable).

:- comment(vectors_format(XVector,YVectors,LinesAttributes,
	Predicate),"Tests the following conditions:

        @begin{itemize}

        @item @var{YVectors} list and @var{LinesAttributes} list have the 
        same number of elements.

        @item @var{XVector} list and each @var{YVectors} element have the 
        same number of elements.

        @item Each sublist of @var{LinesAttributes} is composed of
        5, 3 or 1 elements.

        @end{itemize}

").

vectors_format(XVector,YVectors,LinesAttributes,_Predicate):-
	length(XVector,Xlength),
	length(YVectors,Ylength),
	length(LinesAttributes,Ylength),
	check_sublist1(YVectors,Xlength),
	check_sublist2(LinesAttributes).

vectors_format(_XVector,_YVectors,_LinesAttibuttes,Predicate):-
	throw(error(chartlib,error4,Predicate)).
	


:- pred valid_vectors(+list,+list,+list,+callable).

:- comment(valid_vectors(XVector,YVectors,LinesAttributes,
	Predicate),"Tests the following conditions:

        @begin{itemize}

        @item @var{XVector} list, @var{YVectors} list and
        @var{LinesAttributes} list have the same number of elements.

        @item Each sublist of @var{LinesAttributes} is composed of
        5, 3 or 1 element.

        @end{itemize}

").
valid_vectors(XVectors,YVectors,LinesAttributes,
	      _Predicate):-
	length(XVectors,XYlength),
	length(YVectors,XYlength),
	length(LinesAttributes,XYlength),
	check_sublist2(LinesAttributes),
	valid_vectors1(XVectors,YVectors).

	
valid_vectors(_XVectors,_YVectors,_LinesAttributes,Predicate):-
	throw(error(chartlib,error4,Predicate)).


valid_vectors1([],[]).
valid_vectors1([X|Xs],[Y|Ys]):-
	length(X,Xlength),
	length(Y,Xlength),
	valid_vectors1(Xs,Ys).

:- pop_prolog_flag(multi_arity_warnings).

:- comment(hide,valid_elementlist/2).

:- pred valid_elementlist/2.

%%valid_elementlist(ElementList,Predicate).
valid_elementlist(ElementList,_Predicate):-
	check_sublist1(ElementList,2).
valid_elementlist(_ElementList,Predicate):-
	throw(error(chartlib,error7,Predicate)).
	
:- pred valid_table(+list,+callable).

:- comment(valid_table(ElementTable,Predicate),"All of the
	@var{ElementTable} sublists have the same number of elements and are
	not empty.

").

valid_table(ElementTable,_Predicate):-
	length_element(ElementTable,RowsNumber),
	RowsNumber > 0,
	check_sublist1(ElementTable,RowsNumber).

valid_table(_ElementTable,Predicate):-
	throw(error(chartlib,error8,Predicate)).
	
:- comment(hide,length_element/2).

:- pred length_element/2.

length_element([Y|_Ys],Yelementlength):-
	length(Y,Yelementlength).

:- pred add_doublequotes(X,Y)

    #"@var{Y} is the result of adding the doublequotes code character at
     the beginning and at the end of the character codes list @var{X}.".

:- comment(hide,add_doublequotes/2).

add_doublequotes(X,Y):-
	add_doublequotes1([34|X],Y).   %% " = 34
add_doublequotes1([],[34]).
add_doublequotes1([X|Xs],[X|Ys]):-
	add_doublequotes1(Xs,Ys).
