%% Since Ciao Version 0.8, changes are unified with the GlobalChangeLog 
%% in $(CIAOSRC)/version.

:- use_package(assertions).

:- comment(title,"The Ciao engine"). 

:- comment(module,"

   Currently, this contains the ChangeLog for the Ciao engine. Since
   Ciao Version 0.8, changes to the engine are unified with the
   GlobalChangeLog in @tt{$(CIAOSRC)/version}.  Previous changes do not
   have a version explicitly associated with it.  Eventually, this
   should contain a description of the engine, as a chapter of the
   Reference Manual.

   ").

%% This is so that lpdoc documents this as an application instead 
%% of as a library:
main.
