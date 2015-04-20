:- module(compressed_bytecode,
        [compressLZ/1,
         copyLZ/1],
         [assertions]).

:- impl_defined(
        [compressLZ/1,
         copyLZ/1]).

:- comment(title, "Predicates for the compression and uncompression of bytecode.").

:- comment(author, "Oscar Portela Arjona").
