/* so that a system can read the following in: */

true.

fail_iff :- fail .

iff(true).

iff(true,true).
iff(false,false).

iff(true,true,true).
iff(false,true,false).
iff(false,false,true).
iff(false,false,false).

iff(true,true,true,true).
iff(false,true,false,false).
iff(false,false,true,false).
iff(false,false,false,true).
iff(false,true,true,false).
iff(false,true,false,true).
iff(false,false,true,true).
iff(false,false,false,false).

