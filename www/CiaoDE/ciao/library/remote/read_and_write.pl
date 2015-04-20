:- module(read_and_write, [
        remote_read/2,
        remote_write/2
        ], []).


:- use_module(library(read), [read/2]).
:- use_module(library(attrdump),
        [copy_extract_attr/3, reinstall_attributes/1]).

:- use_module(library(fastrw)).


display_debug(What):-
        (
            debug_flag(on) ->
            display(What),
            nl
        ;
            true
        ).

%debug_flag(on).
debug_flag(off).

%% Juggle with this values in order to select marshalling and/or 
%% attribute encoding. 

%send_type(plain).
send_type(marshalled).

%encode_type(plain).
encode_type(encode_attributes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remote_write(Stream, AttrTerm):-
        display_debug('Writing...'(AttrTerm)), 
        expand_attributes(AttrTerm, ExpandedTerm),
        marshall_term(Stream, ExpandedTerm).

            

remote_read(Stream, AttrTerm):-
        display_debug('Reading...'), 
        unmarshall_term(Stream, ExpandedTerm),
        compact_attributes(AttrTerm, ExpandedTerm),
        display_debug('Read...'(AttrTerm)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attr_encode(Term, '$encoded'(NewTerm, Attributes)):-
        copy_extract_attr(Term, NewTerm, Attributes).

attr_decode('$encoded'(FreeTerm, Attributes), Term):-
        reinstall_attributes(Attributes),
        Term = FreeTerm.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand_attributes(AttrTerm, ExpandedTerm):-
        (
            encode_type(plain) ->
            AttrTerm = ExpandedTerm
        ;
            display_debug('about to encode terms'), 
            attr_encode(AttrTerm, ExpandedTerm),
            display_debug('terms encoded')
        ).

compact_attributes(AttrTerm, ExpandedTerm):-
        (
            encode_type(plain) ->
            ExpandedTerm = AttrTerm
        ;
            display_debug('about to decode terms'), 
            attr_decode(ExpandedTerm, AttrTerm),
            display_debug('terms decoded')
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

marshall_term(Stream, Term):-
        (
            send_type(plain) ->
            displayq(Stream, Term),
            display(Stream, '.'),
            nl(Stream)
        ;
            fast_write(Stream, Term)
        ).


unmarshall_term(Stream, Term):-
        (
            send_type(plain) ->
            read(Stream, Term)
        ;
            fast_read(Stream, Term)
        ).
