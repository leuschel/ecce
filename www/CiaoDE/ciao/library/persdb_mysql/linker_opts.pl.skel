%% This is a template for linker options.  The library path, which
%% *should not be changed*, is replaced at compile time by the directory
%% where the mysql client library lives.  

:- extra_linker_opts('LINUXi86', [ 
        '-Lwhere_mysql_client_lives',
        '-lz'
                                 ]).

:- extra_linker_opts('DARWINppc', [
        '-Lwhere_mysql_client_lives',
        '-lz',
        '-framework System'
                                  ]).
