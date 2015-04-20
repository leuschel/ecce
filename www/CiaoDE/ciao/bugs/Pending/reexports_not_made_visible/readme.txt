When a module 'a' reexports some predicates, and the module 'a' is
statically linked in the shell, and the use_module(a) is written in
the shell, none of the reexported predicates are made visible.

Note that this is a problem of 'compiler', not the shell.

--jfran 
