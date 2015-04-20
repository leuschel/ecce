:- module(text_procesing, [...]).

/*  A module to manipulate Strings and Texts */

/* copyright (c) Arvind Bansal,  
   Kent State University, 
   Kent, OH, USA */

/* This module may be copied and distributed freely to the users of
SICS community provided any enhancements are communicated to the
author and other SICS users community */


:- use_module(library(lists)).


/* String Operations  */

first_char(String, Char) :-
     String = [Char|_].

rest_string(String,  RestString) :-
     String = [_|RestString].

cons(Char, String, [Char|String]).

char_n(Index, String, Char) :-
   nth(Index, String, Char).

paste(String1, String2,  String) :-
    append(String1, String2, String).


substring(String,  Start, Length, SubString) :-
      substring(String, 1, Start, Length, SubString).
      
substring([], _, _, Length, SubString) :-
      (Length =:= 0 ->
         SubString = []
      ;otherwise ->
         format(user,  "Error in substring specification", []),
         !, fail
      ). 
substring(String, Position, Start, Length, SubString) :-
     (Position < Start ->
       String = [_|Ss],
       NewPosition is Position + 1,
       substring(Ss, NewPosition, Start, Length, SubString)
      ;otherwise ->
          n_characters(String, 1, Length, SubString)
     ).

n_characters([], _, _, _) :- 
        format(user, "String is shorter", []), !, fail.
n_characters(String, Count, Length, SubString) :-
      (Count =< Length  ->
            String = [Char|Ss],
            SubString = [Char|Rs],
            NewCount is Count + 1,
            n_characters(Ss, NewCount, Length, Rs)
      ;otherwise ->  SubString = []
      ).


/* substituting characters in a string */

substitute_char(Char, String,  NewChar, NewString) :-
    substitute(Char, String, NewChar, NewString).

/*  Splitting a string at a specific character  */

split_at_char([FirstChar|Ss], FirstChar, [],  [FirstChar|Ss]).
split_at_char([FirstChar|Ss], Char, [FirstChar|Bs],  After) :-
             split_at_char(Ss, Char, Bs,  After).


/* Splitting a string at a specific substring */


split_at_substring(String, Substring, [], String) :-
        prefix(Substring, String).
split_at_substring([Char|Ss], Substring, [Char|Ls], Right) :-
            split_at_substring(Ss, Substring, Ls, Right).


delete_prefix([], Ys, Ys).
delete_prefix([X|Xs], [X|Ys], Zs) :-   delete_prefix(Xs, Ys, Zs).


/* Checking for String  */

is_string(String) :- is_list(String), are_atomic(String).

are_atomic([]).
are_atomic([Char|Cs]) :-  atomic(Char), are_atomic(Cs).


is_empty_string(String) :-
   String == [].

is_non_empty_string(String) :-
   is_string(String),
   String = [_|_].

/*  Checking for a substring  */

is_substring(SubString, String) :-
        split_at_substring(String, SubString, List1, List2),
        length(List1, N1), length(List2, N2),
        length(String, N),
        N1 + N2 =:= N.

/* Joining substrings to form a string */

glue_string(StringList, String) :-
        append(StringList, String).

append([], []).
append([L|Ls], List) :-
   append(Ls, List1),
   append(L, List1, List).


/*  String to Words Operations */


all_wordstring([], []).
all_wordstring(SentenceString,  [WordString|Ws]) :-
       next_wordstring(SentenceString, WordString, Remaining), 
       all_wordstring(Remaining, Ws).

wordstring_n(Index, SentenceString, WordString) :-      
       (Index < 1 ->
          format(user, "Improper Index ~d", [Index]), !, fail         
       ;Index > 1 ->
           next_wordstring(SentenceString, _, Remaining),
           NewIndex is Index - 1,
           wordstring_n(NewIndex, Remaining, WordString)
        ;otherwise ->
           next_wordstring(SentenceString, WordString, _)
        ).

next_wordstring(String, WordString, Remaining) :-
     trim_leading_white(String, String1),
     split_at_white(String1, WordString, Remaining).


trim_leading_white(String, TrimmedString) :-
   String = [Char|Ss],
   (is_white(Char) -> trim_leading_white(Ss, TrimmedString)
   ;otherwise ->  TrimmedString = String
   ).

split_at_white([], [], []).
split_at_white([C|Cs],  Left, Right) :-
        ( is_white(C) ->  Right = Cs, Left = []
        ;otherwise ->
           Left = [C|Ls], split_at_white(Cs, Ls, Right)
        ).


is_white(Character) :-
    ((is_blank(Character)
      ;is_tab(Character)
      ;is_period(Character)
      ;is_CRLF(Character)) -> true
    ;otherwise -> fail
    ).

is_blank(0' ).
is_tab(9).
is_CRLF(10).
is_period(0'.).

/* string to word conversion  */


/*  Converting List of Strings to List of atoms */

string_to_word_list([], []).
string_to_word_list([String|Ss], [Word|Ws]) :-
       atom_chars(Word, String),
       string_to_word_list(Ss, Ws).

word_to_string_list([], []).
word_to_string_list([Atomic|As], [String|Ss]) :-
       name(Atomic, String),
       word_to_string_list(As, Ss).

/*  Processing a sentence of a language   */


/* Joining words to form a sentence  */

sentence(WordList, Sentence) :-
        (is_nonempty_list(WordList) ->
            glue_words(WordList, Sentence)
        ;otherwise ->  Sentence = ''
        ).


glue_words(WordList, Sentence) :-
     glue_words(WordList, ' ' , Sentence).

glue_words(List, Delimiter, Atom) :-
        List = [L|Ls],
        glue_leading_delimiter(Ls, Delimiter, AtomList),
        glue_atoms([L|AtomList], Atom).

glue_leading_delimiter([], _, []).
glue_leading_delimiter([L|Ls], Delimiter, [Atom|As]) :-
        glue_atoms([Delimiter, L], Atom),
        glue_leading_delimiter(Ls, Delimiter, As).


glue_atoms(WordList, Word) :-
    word_to_string_list(WordList,  WordStringList),
    append(WordStringList,  WordString),
    atom_chars(Word, WordString).

glue_suffix(Suffix, Word, SuffixedWord) :-
     glue_atoms([Word,Suffix], SuffixedWord).


glue_prefix(Prefix, Word, PrefixedWord) :-
     glue_atoms([Prefix, Word], PrefixedWord).


/*  Extracting words from a sentence  */

words(Sentence, WordList) :-
   (is_string(Sentence) ->
     all_words(Sentence, WordList)
   ;atomic(Sentence) ->
     name(Sentence, SentenceString),
     all_words(SentenceString, WordList)
   ).


all_words(SentenceString, WordList) :-
  (is_empty_string(SentenceString) ->
     WordList = []
  ;otherwise ->
     next_word(SentenceString, Word, Remaining),
     WordList = [Word|Ws],
   all_words(Remaining, Ws)
  ). 

next_word(Sentence, Word, Remaining) :-
   (is_string(Sentence) ->
        next_wordstring(Sentence, WordString, Remaining),
        atom_chars(Word, WordString)
   ;atomic(Sentence) ->
        name(Sentence, SentenceString),
        next_wordstring(SentenceString, WordString, Remaining),
        name(Word, WordString)
   ;otherwise  ->
        format("Incorrect sentence format", []), !, fail
   ).
        
/*  Finding out the nth word in a sentence  */

word_n(Index, Sentence, Word) :-
 (is_string(Sentence) ->
       wordstring_n(Index, Sentence, WordString),
       name(Word, WordString)
 ;atomic(Sentence) ->
       name(Sentence, SentenceString),
       wordstring_n(Index, SentenceString, WordString),
       name(Word, WordString)
 ;otherwise ->
       format("Improper format of the sentence", []), !, fail
 ).


first_word(Sentence, Word) :-  word_n(1, Sentence, Word).

    
last_word(Sentence, LastWord) :-
        words(Sentence, WordList), last(WordList, LastWord).

but_last_words(Sentence, ButLastWords) :-
        words(Sentence, AllWords),
        but_last(AllWords, ButLastWords).


but_last(List, ButLast) :-
        (is_non_empty_list(List) ->
            reverse(List, [_|ReverseButLastList]),
            reverse(ReverseButLastList, ButLast)
        ;otherwise -> fail
        ).

is_non_empty_list(List) :-
      ((is_list(List),List = [_|_]) -> true
      ;otherwise -> fail
      ).

/* Handling  a text file  */


skip_until_word(Stream, MarkerWord) :-
       (at_end_of_stream(Stream) -> fail
       ;otherwise ->
           get_word(Stream, Word),
           (Word == MarkerWord ->  true
           ;otherwise -> 
              skip_until_word(Stream, MarkerWord)
           )
        ).

get_word(Stream, Word) :-
   skip_white_spaces(Stream),
   get_wordstring(Stream, WordString),
   name(Word, WordString).

get_word(Stream, Word, LastChar) :-
   skip_white_spaces(Stream),
   get_wordstring(Stream, WordString, LastChar),
   name(Word, WordString).

skip_white_spaces(Stream) :-
    (at_end_of_stream(Stream) -> fail
    ;otherwise ->
       peek_char(Stream, Char),
       (is_white(Char) -> 
           get0(Stream, Char), skip_white_spaces(Stream)
       ;otherwise -> true
       )
    ).

get_wordstring(Stream, WordString) :-
       get0(Stream, Char),
       (is_white(Char) -> WordString = []
       ;otherwise ->  
           WordString = [Char|Ws],
           get_wordstring(Stream, Ws)
       ).
              
get_wordstring(Stream, WordString, LastChar) :-
       get0(Stream, Char),
       (is_white(Char) -> WordString = [], LastChar = Char
       ;otherwise ->
           WordString = [Char|Ws],
           get_wordstring(Stream, Ws, LastChar)
       ).

next_line(Stream, Line) :-
     next_linestring(Stream, LineString),
     name(Line, LineString).

next_linestring(Stream, LineString) :-
        ((at_end_of_stream(Stream); at_end_of_line(Stream)) ->
            get0(Stream, _), LineString = []
        ;otherwise ->
            get0(Stream, Char), 
            LineString = [Char|Ls], next_linestring(Stream, Ls)
        ).


my_skip_line(Stream) :-
        (at_end_of_file(Stream) -> true
        ;at_end_of_line(Stream) -> get(Stream, _)
        ;otherwise -> get(Stream, _), my_skip_line(Stream)
        ).


get_sentence(Stream, Sentence) :-
    get_sentence_string(Stream, SentenceString),
    name(Sentence, SentenceString).

get_sentence_string(Stream, SentenceString) :-
      (at_end_of_stream(Stream) ->  SentenceString = []
      ;otherwise ->
           skip_white_spaces(Stream),
           get_wordstring(Stream, WordString, LastChar),
           (is_period(LastChar)  ->  
                   insert_wordstring(WordString, SentenceString, [0'.]) 
           ;otherwise ->
               insert_wordstring(WordString, SentenceString, Rest),
               insert_blank(Rest, Ss),
               get_sentence_string(Stream, Ss)
            )
       ).

insert_wordstring([], Rest,Rest).
insert_wordstring([W|Ws], [W|Ss], Rest) :-
      insert_wordstring(Ws, Ss, Rest).

insert_blank([0' |Ss], Ss).


lower_case(Element, LowerCaseElement) :-
        (atom(Element) ->
            lower_case_atom(Element, LowerCaseElement)
        ;is_string(Element) ->
            lower_case_string(Element, LowerCaseElement)
        ;otherwise ->
            LowerCaseElement = Element
        ).

lower_case_atom(Atom, LowerAtom) :-
        atom_chars(Atom, String),
        lower_case_string(String, LowerString),
        atom_chars(LowerAtom, LowerString).

lower_case_string([], []).
lower_case_string([H1|T1], [H2|T2]) :-
        to_lower_case(H1, H2),
        lower_case_string(T1, T2).



to_lower_case(Char, LowerChar) :-
        (is_uppercase_alphabet(Char) ->
            LowerChar is Char + 0'a - 0'A
        ;otherwise -> LowerChar is Char
        ).


upper_case(Element, UpperCaseElement) :-
        (atom(Element) ->
            upper_case_atom(Element, UpperCaseElement)
        ;is_string(Element) ->
            upper_case_string(Element, UpperCaseElement)
        ;otherwise ->
            UpperCaseElement = Element
        ).


upper_case_atom(Atom, LowerAtom) :-
        atom_chars(Atom, String),
        upper_case_string(String, LowerString),
        atom_chars(LowerAtom, LowerString).

upper_case_string([], []).
upper_case_string([H1|T1], [H2|T2]) :-
        to_upper_case(H1, H2),
        upper_case_string(T1, T2).

to_upper_case(Char, UpperChar) :-
        (is_lowercase_alphabet(Char) ->
            UpperChar is Char - 0'a + 0'A
        ;otherwise -> UpperChar is Char
        ).

is_uppercase_alphabet(AsciiChar) :-
        AsciiChar >= 0'A, AsciiChar =< 0'Z.

is_lowercase_alphabet(AsciiChar) :-
        AsciiChar >= 0'a, AsciiChar =< 0'z.
