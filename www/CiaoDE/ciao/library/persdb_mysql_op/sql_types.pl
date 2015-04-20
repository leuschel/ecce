:- module(sql_types,
	[
%%%    OLD type handling predicates.
	    type_union/3,
	    type_compatible/2,
	    get_type/2,
	    accepted_type/2,
%%%
	    sql_type/1,
	    sql_compatible/2,
%	    convert_type/2,
	    character/2,
	    character_varying/2,
	    bit/2,
	    bit_varying/2,
	    numeric/3,
	    decimal/3,
	    integer/1,
	    smallint/1,
	    float/2,
	    date/1,
	    time/1,
	    time/2,
	    datetime/1,
	    datetime/2
	],
	[assertions,regtypes,isomodes]).

:- use_module(library(lists),[length/2]).

:- comment(title,"SQL 92 Standard Type System").

:- comment(author,"Manuel Hermenegildo, Manuel Carro, Jesus Correas").

:- comment(module,"This module provides Prolog representation and
   checking capabilities for the SQL 92 basic type system.").

:- comment(bug,"There must be a way to describe the database specific
   (non-standard) types instead of embedding them in this module.
   Every specific database must have an associated prolog source to
   deal with specific types. accepted_type/2 is not very adequate. ").

:- comment(bug,"There is no type compatibility for date/time types
   yet. Conversion handling must be added in order to translate prolog
   dates/times into SQL strings and vice versa (in pl2sql.pl).").

% ----------------------------------------------------------------------------
% SQL92 types.
% ----------------------------------------------------------------------------

:- regtype sql_type(Type) :: term 

# "Type definition for SQL types definitions.  SQL type abbreviations
  nor alternative spellings are not included.".

sql_type(character(N)):-
	int(N),
	N > 0,
	max_characters(Max),
	N =< Max.

sql_type(character_varying(N)):-
	int(N),
	N > 0,
	max_characters(Max),
	N =< Max.
	
sql_type(bit(N)):-
	int(N),
	N > 0,
	max_bits(Max),
	N =< Max.

sql_type(bit_varying(N)):-
	int(N),
	N > 0,
	max_bits(Max),
	N =< Max.

sql_type(numeric(P,Q)):-
	int(P),
	int(Q),
	P > 0,
	Q >= 0,
	P > Q,
	max_numerics(Max),
	P =< Max.

sql_type(decimal(P,Q)):-
	int(P),
	int(Q),
	P > 0,
	Q >= 0,
	P > Q,
	max_numerics(Max),
	P =< Max.
sql_type(integer).
sql_type(smallint).
sql_type(float(P)):-
	int(P),
	P >= 0,
	max_numerics(Max),
	P =< Max.
sql_type(date).
sql_type(time(P)):-
	int(P),
	P >= 0,
	max_date_decimals(Max),
	P =< Max.
sql_type(time).
sql_type(datetime).
sql_type(datetime(P)):-
	int(P),
	P >= 0,
	max_date_decimals(Max),
	P =< Max.

% Types for backwards compatibility:
sql_type(string).
sql_type(int).
sql_type(num).
sql_type(flt).

% NOTE: Following limits are a minimal approach to what SQL 92
%       standard refers to as 'implementation defined' items. They
%       are intended to be valid for any database interface.

max_characters(255).
max_bits(255).
max_numerics(15).
max_decimals(15).
max_date_decimals(10).
max_integer(2147483647).
min_integer(-2147483648).
max_smallint(2147483647).  % Currently smallint is not different to integer.
min_smallint(-2147483648).

% ----------------------------------------------------------------------------
% Prolog types for SQL92 data types.
% ----------------------------------------------------------------------------

:- pred character(Value,Length) :: string * int 

# "Type definition for @tt{CHARACTER(n)} SQL92 data type. @var{Value}
  is a Prolog representation of a fixed-length character string of
  length @var{Length}. @var{Value} is a Prolog atom or string of
  length less or equal to @var{Length}".

character(X,N):-
	string(X),
	int(N),
	length(X,N1),
	N1 =< N.
character(X,N):-
	atm(X),
	int(N),
	atom_codes(X,Xs),
	length(Xs,N1),
	N1 =< N.

:- pred character_varying(Value,Length) :: string * int 

# "Type definition for @tt{CHARACTER VARYING(n)} SQL92 data
  type. @var{Value} is a Prolog representation of a variable-length
  character string of length up to @var{Length}. @var{Value} is a
  Prolog atom or string of length less or equal to @var{Length}".

character_varying(X,N):-
	character(X,N).
	
:- set_prolog_flag(multi_arity_warnings,off).

:- pred bit(Value,Length) :: string * int 

# "Type definition for @tt{BIT(n)} SQL92 data type. @var{Value} is a
  Prolog representation of a fixed-length bit string of length up to
  @var{Length}. @var{Value} is a Prolog atom or string of 0s and 1s of
  length less or equal to @var{Length}".

bit(X,N):-
	list(X,bit),
	int(N),
	length(X,N1),
	N1 =< N.

bit(0).
bit(1).

:- pred bit_varying(Value,Length) :: string * int 

# "Type definition for @tt{BIT VARYING(n)} SQL92 data
  type. @var{Value} is a Prolog representation of a fixed-length bit
  string of length up to @var{Length}. @var{Value} is a Prolog atom or
  string of 0s and 1s of length less or equal to @var{Length}".

bit_varying(X,N):-
	bit(X,N).

:- pred numeric(Value,Length,Dec) :: string * int * int

# "Type definition for @tt{NUMERIC(p,q)} SQL92 data type. @var{Value}
  is a Prolog representation of a numeric value of total length
  @var{Length} and @var{Dec} digits after the decimal
  point. @var{Value} is a Prolog float number that fits in the numeric
  data type.".

numeric(X,P,Q):-
	int(P),
	int(Q),
	flt(X),
	X < 10 ** (P-Q).

:- pred decimal(Value,Length,Dec) :: string * int * int

# "Type definition for @tt{DECIMAL(p,q)} SQL92 data type. @var{Value}
  is a Prolog representation of a numeric value of total length
  @var{Length} and @var{Dec} digits after the decimal
  point. @var{Value} is a Prolog float number that fits in the numeric
  data type (currently, it is exactly numeric(p,q)).".

decimal(X,P,Q):-
	numeric(X,P,Q).

:- pred integer(Value) :: int

# "Type definition for @tt{INTEGER} SQL92 data type. @var{Value} is a
  Prolog representation of an integer value.  @var{Value} is a Prolog
  integer that fits in the SQL92 integer data type.".

integer(X):-
	int(X),
	max_integer(Max),
	min_integer(Min),
	X =< Max,
	X >= Min.

:- pred smallint(Value) :: int

# "Type definition for @tt{SMALLINT} SQL92 data type. @var{Value} is a
  Prolog representation of a small integer value.  @var{Value} is a
  Prolog integer that fits in the SQL92 smallint data type (currently,
  it is an integer).".

smallint(X):-
	int(X).

:- pred float(Value,Precision) :: flt * int

# "Type definition for @tt{FLOAT} SQL92 data type. @var{Value} is a
  Prolog representation of a float value.  @var{Value} is a Prolog
  float that fits in the SQL92 float data type (currently, there is
  no check on precision; extra decimals are truncated. @var{Precision}
  is only used for table creation.)".

float(X,P):-
	int(P),
	float(X).

:- pred date(Value) :: term

# "Type definition for @tt{DATE} SQL92 data type. @var{Value} is a
  Prolog representation of a date value.  @var{Value} is a Prolog
  term @tt{date/3} with the fields of an SQL92 date data type.".

date(date(Year,Month,Day)):-
	year(Year),
	month(Month),
	day(Day).

:- pred time(Value,Precision) :: term * int

# "Type definition for @tt{TIME(p)} SQL92 data type. @var{Value} is a
  Prolog representation of a time value.  @var{Value} is a Prolog term
  @tt{time/3} with the fields of an SQL92 time data type, and
  @var{Precision} is the precision in the seconds field (it is not
  currently checked.)".

time(time(Hour,Minute,Second),_P):-
	hour(Hour),
	minute(Minute),
	second(Second).

:- pred time(Value) :: term

# "Type definition for @tt{TIME} SQL92 data type. @var{Value} is a
  Prolog representation of a time value.  @var{Value} is a Prolog term
  @tt{time/3} with the fields of an SQL92 time data type.".

time(time(Hour,Minute,Second)):-
	hour(Hour),
	minute(Minute),
	second(Second).

:- pred datetime(Value) :: term

# "Type definition for @tt{DATETIME} SQL92 data type. @var{Value} is a
  Prolog representation of a date and time value.  @var{Value} is a
  Prolog term @tt{datetime/6} with the fields of an SQL92 datetime
  data type.".

datetime(datetime(Year,Month,Day,Hour,Minute,Second)):-
	date(date(Year,Month,Day)),
	time(time(Hour,Minute,Second)).

:- pred datetime(Value) :: term

# "Type definition for @tt{DATETIME(p)} SQL92 data type. @var{Value}
  is a Prolog representation of a date and time value.  @var{Value} is
  a Prolog term @tt{datetime/6} with the fields of an SQL92 datetime
  data type, and @var{Precision} is the precision in the seconds field
  (it is not currently checked.)".

datetime(datetime(Year,Month,Day,Hour,Minute,Second),P):-
	date(date(Year,Month,Day)),
	time(time(Hour,Minute,Second),P).

:- set_prolog_flag(multi_arity_warnings,on).

year(Year):-
	int(Year),
	Year >= 0,
	Year =< 9999.

month(Month):-
	int(Month),
	Month > 0,
	Month =< 12.

day(Day):-
	int(Day),
	Day > 0,
	Day =< 31.

hour(Hour):-
	int(Hour),
	Hour >= 0,
	Hour =< 23.

minute(Minute):-
	int(Minute),
	Minute >= 0,
	Minute =< 59.

second(Second):-
	number(Second),
	Second >= 0,
	Second =< 61.999999.

%% ---------------------------------------------------------------------------
%% Prolog to SQL compatibility type checks.
%% ---------------------------------------------------------------------------

:- pred sql_compatible(Type,SqlType) 

# "Given a Prolog type @var{Type}, provides the SQL type @var{SqlType}
  which is compatible with @var{Type} in the sense that @var{SqlType}
  can store the contents of @var{Type}. This is a basic compatibility
  check.".

sql_compatible(int,integer).
sql_compatible(atm,character_varying(Max)):-
	max_characters(Max).
sql_compatible(string,character_varying(Max)):-
	max_characters(Max).
sql_compatible(float,float(Max)):-
	max_numerics(Max).


%% ---------------------------------------------------------------------------
%% -- OLD TYPE HANDLING PREDICATES -------------------------------------------
%% ---------------------------------------------------------------------------

:- pred type_union(TypeA,TypeB,Union) :: sqltype * sqltype * sqltype

# "@var{Union} is the union type of @var{TypeA} and @var{TypeB}.".

type_union(TypeA,TypeA,TypeA).
type_union(TypeA,TypeB,TypeA) :- 
	subtype(TypeB,TypeA).
type_union(TypeA,TypeB,TypeB) :- 
	subtype(TypeA,TypeB).
type_union(TypeA,TypeB,TypeC) :- 
	subtype(TypeA,TypeC),
	subtype(TypeB,TypeC).

:- pred type_compatible(TypeA,TypeB) :: sql_type * sql_type

# "Checks if @var{TypeA} and @var{TypeB} are compatible types, i.e.,
   they are the same or one is a subtype of the other.".

type_compatible(Type,Type):-
   sql_type(Type).
type_compatible(SubType,Type):-
   subtype(SubType,Type).
type_compatible(Type,SubType):-
   subtype(SubType,Type).

:- pred subtype(SubType,SuperType) :: sql_type * sql_type

#  "Simple type hierarchy checking.".

subtype(SubType,SuperType):-
   is_subtype(SubType,SuperType).
subtype(SubType,SuperType):-
   is_subtype(SubType,InterType),
   subtype(InterType,SuperType).

:- pred is_subtype(SubType,SuperType) :: sqltype * sql_type

# "Simple type hierarchy for numeric types.".

is_subtype(int,num).
is_subtype(flt,num).

%jcf-begin
is_subtype(character(_),string).
is_subtype(character_varying(_),string).
is_subtype(numeric(_,_),flt).
is_subtype(decimal(_,_),flt).
is_subtype(float(_),flt).
is_subtype(integer,int).
%jcf-end

:- pred get_type(+Constant,Type) :: term * sql_type

# "Prolog implementation-specific definition of type retrievals. CIAO 
   Prolog version given here (ISO).".

get_type('$const$'(Constant),num):-
   number(Constant).
get_type('$const$'(Constant),flt):-
   float(Constant).
get_type('$const$'(Constant),int):-
   term_typing:integer(Constant).
get_type('$const$'(Constant),string):-
   atom(Constant).
get_type('$const$'(Constant),string):-
   string(Constant).


:- pred accepted_type(SystemType, NativeType) :: sql_type * sql_type

# "For the moment, tests wether the @var{SystemType} received is a
  native database-specific type (in the future other systems should be
  supported) and obtains its equivalent @var{NativeType} sqltype.".

accepted_type(NativeType, NativeType) :-
	sql_type(NativeType), !.
