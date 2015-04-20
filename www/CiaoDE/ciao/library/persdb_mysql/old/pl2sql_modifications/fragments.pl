% ----------------------------------------------------------------------------
% Building a string containing the query
% ----------------------------------------------------------------------------

:- pred buildqueries(SQLTermList,SQLString) :: list(sqlterm) * string

   # "Builds a SQL string from the list of SQL term @var{SQLTermList}.".

buildqueries(QueriesList,String):-
   buildqueries2(QueriesList,[],String).

buildqueries2([Query],PreviousString,QueryString):-
   build_query(Query,QueryStr),
   list_concat([PreviousString," ",QueryStr,";"],QueryString).

buildqueries2([Query|Queries],PreviousString,QueryString):-
   \+ (Queries = []),
   build_query(Query,QueryStr1),
   list_concat([PreviousString," ",QueryStr1," UNION "],QueryString1),
   buildqueries2(Queries, QueryString1, QueryString).

% ----------------------------------------------------------------------------

:- pred build_query(QueryCode,SQLString). 

build_query(query([agg_query(Function,Select,From,Where,Group)],_,_),SQLStr):-
   % --- ugly rule here: aggregate function only in SELECT Part of query ----
   !,
   build_query(agg_query(Function,Select,From,Where,Group),SQLStr).

build_query(query(Select,From,Where),SQLStr):-
   build_clause3("SELECT",Select,",",Str1),
   build_clause3("FROM",From,",",Str2),
   build_clause3("WHERE",Where,"AND",Str3),
   list_concat([Str1," ",Str2," ",Str3],SQLStr).


build_query(agg_query(Function,Select,From,Where,Group),SQLStr):-
   build_clause4("SELECT",Function,Select,",",Str1),
   build_clause3("FROM",From,",",Str2),
   build_clause3("WHERE",Where,"AND",Str3),
   build_clause3("GROUP BY",Group,",",Str4),
   list_concat([Str1," ",Str2," ",Str3," ",Str4],SQLStr).

build_query(negated_existential_subquery(Select,From,Where),SQLStr):-
   build_clause3("SELECT",Select,",",Str1),
   build_clause3("FROM",From,",",Str2),
   build_clause3("WHERE",Where,"AND",Str3),
   list_concat(["NOT EXISTS ","(",Str1," ",Str2," ",Str3,")"],SQLStr).

% ----------------------------------------------------------------------------

:- pred build_clause4(Keyword,Function,ClauseCode,Separator,SQLString)

   # "Where 

   @var{Keyword} is one of SELECT, FROM, WHERE, or GROUP BY, 

   @var{Function} is an aggregation function, and

   @var{ClauseCode} is the code corresponding to the appropriate clause
   of an SQL query, and

   @var{Separator} indicates the character(s) which separate the items
   of a clause from each other (, or AND), and

   @var{SQLString} contains the SQL sentence fragment corresponding to the
   clause.".

build_clause4(Keyword,Function,[Column],Separator,SQLString):-
   build_clause2([Column],Separator,Str),
%%   aggregate_functor(Function,FunctionTerm), modified 19-10-98, Ignacio
   atom_codes(Function,FunctionString),
   list_concat([Keyword," ",FunctionString,"(",Str,")"],SQLString).

% ----------------------------------------------------------------------------

:- pred build_clause3(Keyword,ClauseCode,Separator,SQLString).

build_clause3(_Keyword,[],_,"").
build_clause3(Keyword,[Column|RestColumns],Separator,SQLString):-
   build_clause2([Column|RestColumns],Separator,Str),
   list_concat([Keyword," ",Str],SQLString).

% ----------------------------------------------------------------------------

:- pred build_clause2(ClauseCode,Separator,SQLString).

build_clause2([Item],_,SQLString):-
   build_column(Item,SQLString).
build_clause2([Item,NextItem|RestItems],Separator,SQLString):-
   build_column(Item,Str1),
   build_clause2([NextItem|RestItems],Separator,Str2),
   list_concat([Str1," ",Separator," ",Str2],SQLString).

% ----------------------------------------------------------------------------

:- pred build_column(ColumnCode,SQLString).

build_column('*',"*").
build_column(att(RangeVar,Attribute),SQLString):-
   atom_codes(RangeVar,RangeVarStr),
   atom_codes(Attribute,AttributeStr),
   list_concat([RangeVarStr,".",AttributeStr],SQLString).
build_column(rel(Relation,RangeVar),SQLString):-
   atom_codes(Relation,RelationStr),
   atom_codes(RangeVar,RangeVarStr),
   list_concat([RelationStr," ",RangeVarStr],SQLString).

build_column('$const$'(StringAtom),String):-
   get_type('$const$'(StringAtom),string),
   atom_codes(StringAtom,String).
%%%   writeq(String).    %% Using writeq ^^^^^^^^^^  
build_column('$const$'(Number),String):-
   get_type('$const$'(Number),NumType),
   check_type_compatible(NumType,number),
   number_codes(Number,String). 
build_column(comp(LeftArg,Operator,RightArg),String):-
   build_column(LeftArg,Str1),
   atom_codes(Operator,Str2),
   build_column(RightArg,Str3),
   list_concat([Str1," ",Str2," ",Str3],String).
build_column(LeftExpr * RightExpr,String):-
   build_column(LeftExpr,Str1),
   build_column(RightExpr,Str2),
   list_concat([Str1,"*",Str2],String).
build_column(LeftExpr / RightExpr,String):-
   build_column(LeftExpr,Str1),
   build_column(RightExpr,Str2),
   list_concat([Str1,"/",Str2],String).
build_column(LeftExpr + RightExpr,String):-
   build_column(LeftExpr,Str1),
   build_column(RightExpr,Str2),
   list_concat([Str1,"+",Str2],String).
build_column(LeftExpr - RightExpr,String):-
   build_column(LeftExpr,Str1),
   build_column(RightExpr,Str2),
   list_concat([Str1,"-",Str2],String).
build_column(agg_query(Function,Select,From,Where,Group),String):-
%%   nl,
   build_query(agg_query(Function,Select,From,Where,Group),Str),
   list_concat(["(",Str,")"],String).
build_column(negated_existential_subquery(Select,From,Where),String):-
   build_query(negated_existential_subquery(Select,From,Where),String).


%% 2nd FRAGMENT

sqlterm2string(SQLQueryTerm,SQLQueryString) :-
      buildqueries(SQLQueryTerm,SQLQueryString),
      !,
      debug_message(" Translation done ",[]).
sqlterm2string(SQLQueryTerm,_) :-
    error_message("could not convert to string ~w",[SQLQueryTerm]),
    fail.

%% END OF 2nd FRAGMENT

%% %% Original code fails on some SQLQueryTerms for which printqueries/1 succeeds!
%% %% Doing this serious kludge instead for now:
%%             REPAIRED! 
%% sqlterm2string(SQLQueryTerm,SQLQueryString) :-
%%    telling(O),
%%    TMP = '/tmp/sqlqueryfileIgnacio',
%%    tell(TMP),
%%    write('"'),
%%    printqueries(SQLQueryTerm),
%%    write('"'),
%%    write('.'),
%%    nl,
%%    told,
%%    tell(O),
%%    seeing(I),
%%    see(TMP),
%%    do_the_read(SQLQueryTerm,I,SQLQueryString).
%% 
%% do_the_read(_SQLQueryTerm,I,SQLQueryString)  :-
%%    read(SQLQueryString),
%%    seen,
%%    see(I),
%%    !.
%% do_the_read(SQLQueryTerm,I,_SQLQueryString)  :-
%%    error_message("could not convert to string ~w",[SQLQueryTerm]),
%%    seen,
%%    see(I),
%%    fail.

%% sqlterm2string(Queries,QueryString) :-
%%    queries_dstring(Queries,QueryString,[]),
%%    !.
%% sqlterm2string(SQLQueryTerm,_SQLQueryString) :-
%%    error_message("could not convert to string ~w",[SQLQueryTerm]).
