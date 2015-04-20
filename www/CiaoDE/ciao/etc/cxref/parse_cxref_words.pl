:- module(parse_cxref_words, [get_list_of_words/3], [dcg]).

:- use_module(library(strings)).

get_first_word(W) -->
        cxref_skip,
        get_word(W).

get_word([]) --> end_word, !.
get_word([Char|Chars]) --> [Char], !, get_word(Chars).
get_word([]) --> [].

end_word --> " ".

skippable --> " ".
skippable --> "%".     %% Ask the cxref people...
skippable --> "&".     %% Ask the cxref people...
skippable --> "0".
skippable --> "1".
skippable --> "2".
skippable --> "3".
skippable --> "4".
skippable --> "5".
skippable --> "6".
skippable --> "7".
skippable --> "8".
skippable --> "9".


cxref_skip --> skippable, !, cxref_skip.
cxref_skip --> [].


get_list_of_words([]) --> cxref_skip.
get_list_of_words([AtomWord|Words]) --> 
        get_first_word(Word),
        {atom_codes(AtomWord, Word)},
        get_list_of_words(Words).
