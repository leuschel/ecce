:- module(_,_,[make,functions]).

:- reexport('LPSETTINGS',[component/1,filepath/1,systempath/1]).
:- reexport(ciaosrc('CIAOSHARED'), [lpmake/1,lpdoc2/1,setlocalciao/1]).

readmetarget := '..'.
