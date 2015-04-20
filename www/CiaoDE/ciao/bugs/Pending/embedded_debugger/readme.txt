The embedded debugger does not behave correctly in this example.
We have two modules:

  kk (main module)
  mm

The first one calls j/1 and k/1, which are defined in module mm.
We set a spy point in j/1 and a spy point in k_2/1, called by b/1.
If we compile kk (ciaoc kk) and execute it with:

$ ./kk 1 2 3

then, the embedded debugger miss the call to j/1 from module kk to
module mm.

Possible reason: the embedded debugger may transform the calls to
  debugged predicates instead of debugged predicates (so that it
  cannot catch calls from modules not marked to be debugged).


-- Jfran (the bug initially reported by Jesus Correas)


