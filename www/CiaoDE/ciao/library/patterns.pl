:- module(patterns, 
	[match_pattern/2, 
	 match_pattern/3,
	 case_insensitive_match/2,
	 letter_match/2,
	 pattern/1,
	 match_pattern_pred/2], [assertions, dcg, regtypes]).

:- use_module(library(lists)).

:- comment(title,"Pattern (regular expression) matching").

:- comment(author,"The CLIP Group").

:- comment(module,"This library provides facilities for matching
   strings and terms against @index{patterns} (i.e., @index{regular
   expressions}).").

:- comment(pattern/1,"Special characters for @var{Pattern} are:
  @begin{description}
  @item{*} Matches any string, including the null string.
  @item{?} Matches any single character.
  @item{[...]} Matches any one of the enclosed characters.  A pair of
   characters separated by a minus sign denotes a range; any character
   lexically between those two characters, inclusive, is matched.  If the
   first character following the [ is a ^ then any character not enclosed
   is matched.  No other character is special inside this construct.  To
   include a ] in a character set, you must make it the first character.
   To include a `-', you must use it in a context where it cannot possibly
   indicate a range: that is, as the first character, or immediately after
   a range.
  @item{|} Specifies an alternative.  Two patterns A and B with
   | in between form an expression that matches anything that either A or B
   will match.
  @item{@{...@}} Groups alternatives inside larger patterns.
  @item{\\} Quotes a special character (including itself).
 @end{description}").

:- regtype pattern(P) # "@var{P} is a pattern to match against.".

%% Should be defined more precisely.
pattern(_).

:- push_prolog_flag(multi_arity_warnings,off).

:- pred match_pattern(Pattern, String) : pattern * string 

        # "Matches @var{String} against @var{Pattern}. For example, 
           @tt{match_pattern(\"*.pl\",\"foo.pl\")} succeeds.".

match_pattern(Pattern, String) :-
        pattern_term0(Pattern, PaTerm),
        match(PaTerm, String, []), !.

:- pred match_pattern(Pattern, String, Tail) : pattern * string * string

        # "Matches @var{String} against @var{Pattern}. @var{Tail} is
           the remainder of the string after the match. For example,
           @tt{match_pattern(\"??*\",\"foo.pl\",Tail)} succeeds,
           instantiating @tt{Tail} to @tt{\"o.pl\"}.".

match_pattern(Pattern, String, Tail) :-
        pattern_term0(Pattern, PaTerm),
        match(PaTerm, String, Tail).

:- pop_prolog_flag(multi_arity_warnings).

pattern_term0(Chs, PaTerm0) :-
        pattern_term(Chs, PaTerm, Chs_),
        ( Chs_ = [] ->
              PaTerm0 = PaTerm
        ; PaTerm0 = opt([PaTerm|PaTerms]),
          pattern_terms(Chs_, PaTerms, [])
        ).

pattern_terms([], [], []).
pattern_terms(next(Chs), [PaTerm|PaTerms], Chs_rest) :-
        pattern_term(Chs, PaTerm, Chs_),
        pattern_terms(Chs_, PaTerms, Chs_rest).
pattern_terms(rest(Chs), [], Chs).

pattern_term([], [], []).
pattern_term([Ch|Chs], PaTerm, Chs_) :-
        pattern_term_(Ch, Chs, PaTerm, Chs_).

pattern_term_(0'\\, [Ch|Chs],     [Ch|PaTerm],          Chs_) :- !,
                                       pattern_term(Chs, PaTerm, Chs_).
pattern_term_(0'*,  Chs,          [star|PaTerm],        Chs_) :- !,
                                       pattern_term(Chs, PaTerm, Chs_).
pattern_term_(0'?,  Chs,          [any|PaTerm],         Chs_) :- !,
                                       pattern_term(Chs, PaTerm, Chs_).
pattern_term_(0'[,  [0'^,Ch|Chs], [anynot(Set)|PaTerm], Chs_) :- !,
                                       pattern_set(Ch, Chs, Set, PaTerm, Chs_).
pattern_term_(0'[,  [Ch|Chs],     [any(Set)|PaTerm],    Chs_) :- !,
                                       pattern_set(Ch, Chs, Set, PaTerm, Chs_).
pattern_term_(0'{,  Chs,          [opt(Opts)|PaTerm],   Chs_) :- !,
                                       pattern_terms(next(Chs), Opts, Tail),
                                       pattern_term(Tail, PaTerm, Chs_).
pattern_term_(0'|,  Chs,          [],              next(Chs)) :- !.
pattern_term_(0'},  Chs,          [],              rest(Chs)) :- !.
pattern_term_(Ch,   Chs,          [Ch|PaTerm],          Chs_) :-
                                       pattern_term(Chs, PaTerm, Chs_).

/* A ']' must appear the first, a '-', the first, or after a range */
pattern_set(0'], [Ch|Chs], [0']|Set], PaTerm, Chs_) :- !,
        pattern_set(Ch, Chs, Set, PaTerm, Chs_).
pattern_set(Ch0, [Ch|Chs], Set, PaTerm, Chs_) :-
        pattern_set_after(Ch, Ch0, Chs, Set, PaTerm, Chs_).

pattern_set_(0'], Chs, [], PaTerm, Chs_) :- !,
        pattern_term(Chs, PaTerm, Chs_).
pattern_set_(Ch0, [Ch|Chs], Set, PaTerm, Chs_) :-
        pattern_set_after(Ch, Ch0, Chs, Set, PaTerm, Chs_).

pattern_set_after(0'], Ch0, Chs, [Ch0], PaTerm, Chs_) :- !,
        pattern_term(Chs, PaTerm, Chs_).
pattern_set_after(0'-, Ch0, [ChN,Ch|Chs], Set, PaTerm, Chs_) :- !,
        char_range(Ch0, ChN, Set, Set_),
        pattern_set_(Ch, Chs, Set_, PaTerm, Chs_).
pattern_set_after(Ch, Ch0, Chs, [Ch0|Set], PaTerm, Chs_) :-
        pattern_set_(Ch, Chs, Set, PaTerm, Chs_).

char_range(Ch0, ChN, Tail, Tail) :- Ch0 > ChN, !.
char_range(Ch0, ChN, [Ch0|Chs], Tail) :-
        Ch1 is Ch0+1,
        char_range(Ch1, ChN, Chs, Tail).

match([]) --> [].
match([E|Es]) --> match(E), match(Es).
match(star) --> [].
match(star) --> [_], match(star).
match(any) --> [_].
match(any(Chars)) --> [Ch], { contains1(Chars, Ch) }.
match(anynot(Chars)) --> [Ch], { nocontainsx(Chars, Ch) }.
match(opt([Exp|_])) --> match(Exp).
match(opt([_|LExp])) --> match(opt(LExp)).
match(Ch) --> [Ch].



:- pred case_insensitive_match(Pred1, Pred2) # "Tests if two predicates
   @var{Pred1} and @var{Pred2} match in a case-insensitive way.".

case_insensitive_match(Pred1, Pred2):-
	(var(Pred1); var(Pred2)), !.
case_insensitive_match(Pred1, Pred2):-
	functor(Pred1, _F1, 0),
	functor(Pred2, _F2, 0), !,
	case_insensitive_match1([Pred1], [Pred2]).
case_insensitive_match(Pred1, Pred2):-
	Pred1=..[F|Args1],
	Pred2=..[F|Args2],
	case_insensitive_match1(Args1, Args2).

case_insensitive_match1([], []).
case_insensitive_match1([Atom1|A1s], [Atom2|A2s]):-
	(var(Atom1); var(Atom2)), !,
	 case_insensitive_match1(A1s, A2s).
case_insensitive_match1([Atom1|A1s], [Atom2|A2s]):-
	name(Atom1, L1),
	name(Atom2, L2),
	list_match(L1, L2),
	case_insensitive_match1(A1s, A2s).

list_match([], []).
list_match([M1|Ms], [W1|Ws]):-
	letter_match(M1, W1),
	list_match(Ms, Ws).

:- pred letter_match(X,Y) # "True iff @var{X} and @var{Y} represents
   the same letter".

letter_match(X, Y):- X==Y, !.
letter_match(X, Y):- X is Y + (0'a-0'A), !.
letter_match(X, Y):- Y is X + (0'a-0'A), !.

:- pred match_pattern_pred(Pred1, Pred2) # "Tests if two predicates
   @var{Pred1} and @var{Pred2} match using regular expressions.".

match_pattern_pred(Pattern, Pred):-
	(var(Pattern); var(Pred)), !.
match_pattern_pred(Pattern, Pred):-
        functor(Pattern, _F1, 0),
	functor(Pred, _F2, 0), !,
	match_pattern_pred1([Pattern], [Pred]).
match_pattern_pred(Pattern, Pred):-
	Pattern=..[F|Args1],
	Pred=..[F|Args2],
	match_pattern_pred1(Args1, Args2).

match_pattern_pred1([], []).
match_pattern_pred1([A1|A1s], [A2|A2s]):-
	(var(A1); var(A2)), !,
	match_pattern_pred1(A1s, A2s).
match_pattern_pred1([A1|A1s], [A2|A2s]):-
	name(A1, A1l),
	name(A2, A2l),
	match_pattern(A1l, A2l),
	match_pattern_pred1(A1s, A2s).
