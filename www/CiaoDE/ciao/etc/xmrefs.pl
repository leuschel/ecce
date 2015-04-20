
:- module(xmrefs,
        [ xmrefs/0,
          xmlinks/0,
          quit/0
        ],
        [ assertions, regtypes
        ]).

:- use_module(library(davinci)).
:- use_module(library('xrefs/mrefs'),[ mrefs_lgraph/1, mrefs_ugraph/1 ]).
:- reexport(library('xrefs/mrefs'),[ set_files/1, set_flag/1 ]).

:- comment(title,"Crossed-references between modules").
:- comment(subtitle,"@bf{The CIAO System Documentation Series}").
:- comment(subtitle,"@em{Draft printed on:} @today{}").
:- comment(author,"Francisco Bueno").
:- comment(module,"This module allows displaying a graph of
        crossed-references between modules. The graph is obtained using
	@tt{library('xrefs/mrefs')} and is displayed using daVinci (a
        windows-based graphics graph displayer developed by U. of Bremen,
	Germany), via @tt{library(davinci)}.

        See the documentation of these libraries for details.").

:- comment(bug,"@tt{quit/0} does nothing. daVinci has to be exited
        from its own window.").

%-----------------------------------------------------------------------------
% entry points

:- pred xmrefs 
        # "Displays a graph of crossed-references for the current files.".

xmrefs:- 
        mrefs_lgraph(Graph),
        ( davinci -> true ; true ),
        davinci_lgraph(Graph).

:- pred xmlinks
        # "Like @tt{xmrefs/0} but edges have no labels.".

xmlinks:- 
        mrefs_ugraph(Graph),
        ( davinci -> true ; true ),
        davinci_ugraph(Graph).

:- comment(quit/0,"Quits daVinci.").

quit:- davinci_quit.

:- comment(doinclude,set_files/1).
:- comment(doinclude,sourcename/1).
:- comment(sourcename/1,"See @tt{engine(streams_basic)}.").
:- comment(doinclude,set_flag/1).
