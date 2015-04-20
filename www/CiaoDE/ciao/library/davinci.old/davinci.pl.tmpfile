
:- module(davinci,
	[ davinci/0,
	  davinci_quit/0,
	  davinci_ugraph/1,
	  davinci_lgraph/1,
	  formatting/2,
	  ugraph2term/2
	],
	[ assertions,basicmodes,regtypes ]).

:- load_compilation_module('ciaodevelop/ciaopaths').

:- use_module(library(format),[format/3]).
:- use_module(library(read),  [read/1]).
:- use_module(library('graphs/lgraphs'),
	[ lgraph/2
	]).
:- use_module(library('graphs/ugraphs'),
	[ ugraph/1,
	  vertices_edges_to_ugraph/3
	]).
:- use_module(library(write), [write/2,writeq/1]).
:- use_module(library(system),[mktemp/2,shell/1]).

:- comment(author,"Francisco Bueno").
:- comment(module,
	"This library allows connecting a Ciao Prolog application
	 with daVinci V2.X
         (daVinci is developed by U. of Bremen, Germany).

         For the time being, communication is based on a one-way channel:
         messages are sent to a daVinci process started on demand (i.e., for
         each Prolog goal a new daVinci process is started). Messages are
         sent via writing the term as text, but are treated as terms from
         the Prolog side, since for daVinci they are text but have term
         syntax; the only difficulty lies in strings, for which special
         Prolog syntax is provided.

         Here are two simple examples of use:
         @begin{verbatim}
davinci_ugraph(['A'-['B','C'],'B'-['D'],'C'-['E'],'D'-[],'E'-['D'],'F'-['B']]).
         @end{verbatim}
         makes daVinci show a graph corresponding to the Prolog term
         representation of an unlabeled graph.

         @begin{verbatim}
davinci_lgraph(['A'-['B'-[from,a,to,b],'C'-ac],'B'-['D'-bd],'C'-['E'-ce],
	        'D'-[],'E'-['D'-ed],'F'-['B'-fb]]).
         @end{verbatim}
         makes daVinci show a graph corresponding to the Prolog term
         representation of a labeled graph. This will create the same graph
         as above, but showing labels in the edges.
        ").

:- comment(bug,"@tt{davinci/0} and @tt{davinci_quit/0} do nothing: daVinci
        is started on demand and has to be exited from its own window.").
:- comment(bug,"Each invocation of daVinci leaves a @tt{davinciXXXXXX}
        file in the directory. Remember to erase them.").

% -------------------------------------------------------------------------

command(Command,File):- atom_concat('daVinci ',File,Command).

% -------------------------------------------------------------------------
:- comment(davinci/0,"Start up a daVinci process.").

davinci.

% -------------------------------------------------------------------------
:- comment(davinci_quit/0,"Exit daVinci process.").

davinci_quit.

% -------------------------------------------------------------------------
:- pred davinci_put(+Term) : commandterm
	# "Send the @var{Term} command to daVinci.".

davinci_put(Term):-
	mktemp('davinciXXXXXX',Filename),
	open(Filename,write,In),
	formatting(Term,In),
	nl(In),
	close(In),
	command(Command,Filename),
	shell(Command).

:- comment(doinclude,commandterm/1).
:- regtype commandterm(Term)
	# "@var{Term} is a term representing a daVinci command.".
:- comment(commandterm(Term),
	"Syntactically, @var{Term} is a just a term. Semantically, it has
         to correspond to a command understood by daVinci; otherwise
         daVinci reports back an error. Two functors within the subterms
         of @var{Term} are interpreted in a special way: @tt{string/1}
         and @tt{text/1}. @tt{string(Term)} is given to daVinci as
         @tt{""Term""}; @tt{text(List)} is given as
         @tt{""Term1\nTerm2\n...Term\n""} for each Term in @tt{List}.
         If your term has functors @tt{string/1} and @tt{text/1} that you
         don't want to be interpreted this way, use them twice, i.e.,
         @tt{string(string(Term))} is given to daVinci as @tt{string(Term')},
         where @tt{Term'} is the interpretation of @tt{Term}.").

commandterm(Term):- term(Term).

:- pred formatting(+Term,+Out) : commandterm * stream
	# "Prints @var{Term} to @var{Out} as a daVinci command.".
 
formatting(Atom, In):-
        atomic(Atom), !,
        write(In, Atom).
formatting(L, In):-
        L = [_|_], !,
        format_list(L, In).
formatting(string(Text), In):- !,
	format_string(Text, In).
formatting(text(Text), In):- !,
	format_text(Text, In).
formatting(Functor, In):-
        format_struct(Functor, In).

format_list(L, In):-
        write(In, '['),
        format_list_elements(L, In),
        write(In, ']').

format_list_elements([Element], In):- !,
        formatting(Element, In).
format_list_elements([This|Rest], In):-
        formatting(This, In),
        write(In, ','),
        format_list_elements(Rest, In).

format_string(string(Arg),In):- !,
	format_struct(string(Arg),In).
format_string(Arg,In):-
	format(In,'"~w"',[Arg]).

format_text(text(Arg),In):- !,
	format_struct(text(Arg),In).
format_text(Arg,In):-
	write(In,'"'),
	write_list(Arg,In),
	write(In,'"').

write_list([A|As],In):-
	format(In,'~w\\n',[A]),
        write_list(As,In).
write_list([],_In).

format_struct(Struct, In):-
        functor(Struct, Name, Arity),
        write(In, Name),
        write(In, '('),
        format_args(Struct, 1, Arity, In),
        write(In, ')').

format_args(Functor, N, N, In):- !,
        arg(N, Functor, Arg),
        formatting(Arg, In).
format_args(Functor, N, M, In):-
        N < M,
        arg(N, Functor, Arg),
        formatting(Arg, In),
        write(In, ','),
        N1 is N + 1,
        format_args(Functor, N1, M, In).

% -------------------------------------------------------------------------
:- pred davinci_ugraph(+Graph) : specialugraph
	# "Send @var{Graph} to daVinci.".

davinci_ugraph(Graph):-
	ugraph2term(Graph,Term),
	davinci_put(Term).

:- comment(doinclude,specialugraph/1).
:- regtype specialugraph(Graph)
	# "@var{Graph} is an ugraph with special escape functors for
           daVinci.".
:- comment(specialugraph(Graph),
	"@var{Graph} is a term which denotes an ugraph as in
         @tt{library(ugraphs)}. Vertices of the form @tt{node(Term,List)}
         are interpreted as a vertex @tt{Term} with attributes @tt{List}.
         @tt{List} is a list of terms conforming the syntax of
         @tt{commandterm}, corresponding to daVinci's graph node attributes.
         If your vertex has functor @tt{node/2} and you don't want it to be
         interpreted this way, use it twice, i.e., @tt{node(node(T1,T2),[])}
         is given to daVinci as vertex @tt{node(T1,T2)}. A vertex is used
         both as label and name of daVinci's graph node; daVinci's graph edges
         have label @tt{V1-V2} where @tt{V1} is the source and @tt{V2} the
         sink of the edge. There is no support for multiple edges between the
         same two vertices.").

specialugraph(G):- ugraph(G).

:- pred ugraph2term(+Graph,-Term) : specialugraph(Graph)
	# "Converts @var{Graph} to daVinci's representation.".

ugraph2term([N-Ns|Graph],[Node|Term]):-
	node2term(N,V,As),
	Node=l(string(V),n(string(''),As,Edges)),
	edges(Ns,V,Edges),
	ugraph2term(Graph,Term).
ugraph2term([],[]).

node2term(node(N,As0),N,As):- !,
	( As0=[]
	-> As=[a(string('OBJECT'),string(N))]
	 ; As=As0
	).
node2term(N,N,As):-
	As=[a(string('OBJECT'),string(N))].

edges([N|Ns],Source,[Edge|Edges]):-
	node2term(N,V,_),
	Edge=l(string(Source-V),e(string(''),[],r(string(V)))),
	edges(Ns,Source,Edges).
edges([],_Source,[]).

 %% ugraph2davinciterm(U):-
 %%         ugraph2term(U, T),
 %%         formatting(T, user_output).

% -------------------------------------------------------------------------
:- pred davinci_lgraph(+Graph) : speciallgraph
	# "Send @var{Graph} to daVinci.".

davinci_lgraph(Graph):-
	retractall_fact(label(_)),
	lgraph2ugraph(Graph,UGraph),
	davinci_ugraph(UGraph).

:- comment(doinclude,speciallgraph/1).
:- regtype speciallgraph(Graph)
	# "@var{Graph} is an lgraph with special escape functors for
           daVinci.".
:- comment(speciallgraph(Graph),
	"@var{Graph} is an lgraph as in @tt{library(lgraphs)}, except that
         the weights are labels, i.e., they do not need to be integers.
         Vertices with functor @tt{node/2} are interpreted in a special way,
         as in @tt{specialugraph/1}. Edge labels are converted into special
         intermediate vertices for daVinci; duplicated labels are solved by
         adding dummy atoms of the form @tt{''}. There is no support for
         multiple edges between the same two vertices.").

speciallgraph(Graph):- lgraph(Graph,term).

% This one is similar to wgraph_to_ugraph/2 in SICStus3 library(wgraphs)
% except that the labels (weights) are converted into new vertices
% These new vertices are special, in the sense that they have daVinci
% attributes so that they will appear as text instead of box nodes
lgraph2ugraph(LGraph,UGraph):-
	lgraph2edges(LGraph,Edges),
	vertices_edges_to_ugraph([],Edges,UGraph).

lgraph2edges([N-LNs|LGraph],Edges):-
	edgelist2edges(LNs,N,Edges,Edges0),
	lgraph2edges(LGraph,Edges0).
lgraph2edges([],[]).

edgelist2edges([N2-L|LNs],N0,[N0-N1,N1-N2|Edges],Edges0):-
	davinci_special_node(L,N1),
	edgelist2edges(LNs,N0,Edges,Edges0).
edgelist2edges([],_N0,Edges,Edges).

davinci_special_node(L0,node(L,Attrs)):-
	Attrs=[a(string('OBJECT'),Name),
	       a(string('_GO'),string(text))],
	solve_dup_label(L0,L),
	asserta_fact(label(L)),
	node_name(L0,Name).

:- data label/1.

solve_dup_label(L,L1):-
	label(L), !,
	add_dummy(L,L0),
	solve_dup_label(L0,L1).
solve_dup_label(L,L).

add_dummy(L,[' '|L]):- L=[_|_], !.
add_dummy(L,[' ',L]).

node_name(L,Name):-
	list(L), !,
	Name=text(L).
node_name(L,string(L)).

% -------------------------------------------------------------------------

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*5+21,1999/12/17,16:28*20+'MET'), "Added
   @tt{library(davinci)}.  (Francisco Bueno Carrillo)").

% -------------------------------------------------------------------------
