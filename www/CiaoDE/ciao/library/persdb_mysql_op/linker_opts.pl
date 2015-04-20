%% This is a template for linker options.  The library path, which
%% *should not be changed*, is replaced at compile time by the directory
%% where the mysql client library lives.  

:- extra_linker_opts('LINUXi86', [ 
        '-L/usr/lib/mysql',
        '-lz'
                                 ]).

:- extra_linker_opts('DARWINppc', [
        '-L/usr/lib/mysql',
        '-lz',
        '-framework System'
                                  ]).
