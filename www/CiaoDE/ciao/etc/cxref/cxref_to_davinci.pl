:- module(cxref_to_davinci, [main/1]).

:- use_module(parse_cxref_words).
:- use_module(library(sets)).
:- use_module(library(ugraphs)).
:- use_module(library(strings)).
:- use_module(library(lists), [select/3]).
:- use_module(library(format)).
%PBC:- use_module('/home/clip/Systems/ciao/library.development/davinci').
:- use_module(library(davinci)).

%% I use the following cxref command to generate the cross reference file for
%% functions:
%% cxref *.[ch] -xref-function -DLINUX -Di86 -O/home/clip/Systems/ciao/etc/cxref

:- data default_davinci_file/1.
default_davinci_file('default.daVinci').

:- data default_graph_file/1.
default_graph_file('default.graph').


main(Args):-
        (
            select('-reverse', Args, NewArgs) ->
            Reverse = yes
        ;
            Reverse = no,
            NewArgs = Args
        ),
        handle_args(NewArgs, InputFile, OutputFile, Case),
        open(InputFile, read, InputStream),
        read_edges_verts(InputStream, Edges, Verts),
        close(InputStream),
        vertices_edges_to_ugraph(Verts, Edges, ThisUgraph),
        (
            Reverse = yes ->
            transpose(ThisUgraph, Ugraph)
        ;
            Ugraph = ThisUgraph
        ),
        (
            Case = batch ->
            write_ugraph_to_file(OutputFile, Ugraph)
        ;
            interactive(Ugraph)
        ).

 %%         ugraph2term(Ugraph, DVTerm),
 %%         open(OutputFile, write, OutputStream),
 %%         formatting(DVTerm, OutputStream),
 %%         close(OutputStream).

write_ugraph_to_file(OutputFile, Ugraph):-
        ugraph2term(Ugraph, DVTerm),
        open(OutputFile, write, OutputStream),
        formatting(DVTerm, OutputStream),
        close(OutputStream).

handle_args([In, Out], In, Out, batch):- !.
handle_args([In], In, _Out, interactive):- !.
handle_args(_, _, _, _):-
        usage.

usage:-
        usage_text(Text),
        format(user_error,"Usage: ~s~n",[Text]),
        halt.

usage_text("
        cxref_to_davinci [-reverse] <infile> <outfile>

        <infile> must be a function cross-reference file created using
        the flag -xref-function (plus the additional flags you deem adequate).
        The daVinci-readable form will be stored in <outfile>.

        cxref_to_davinci [-reverse] <infile>

        Enter interactive mode; allow navigation and construction of a
        graph which can be dumped to a daVinci file.

        In any case, the '-reverse' flag indicates that the graph is
        to be transposed prior to its use.
").


read_edges_verts(I, Edges, Verts):-
        get_line(I, FirstLine),
        read_rest_edges(FirstLine, I, Edges, Verts).

read_rest_edges([], _I, [], []).
read_rest_edges(LastLine, I, Edges, [Caller|RestCallers]):-
        parse_line(LastLine, Edges, RestEdges, Caller),
        get_line(I, NewLine),
        read_rest_edges(NewLine, I, RestEdges, RestCallers).

parse_line(Line, Edges, RestEdges, Caller):-
        get_list_of_words(LOW, Line, []),!,
        LOW = [_Ignored, Caller|Callees],
        gen_edges(Callees, Caller, Edges, RestEdges).

gen_edges([], _Caller, Edges, Edges).
gen_edges([End|Rest], Caller, [Caller-End|Other], RestEdges):-
        gen_edges(Rest, Caller, Other, RestEdges).


interactive(Ugraph):-
        format("Entering interactive mode~n~n", []),
        interact(Ugraph, []).

interact(Whole, CurrG):-
        get_command(Command),
        do_command(Command, Whole, CurrG, NewG), !,
        interact(Whole, NewG).

get_command(Command):-
        commandtext(CommandText),
        format("~s~nCommand: ", [CommandText]),
        get_line(Command).


commandtext(
"
     v: view current set of nodes
     V: view all nodes in whole graph
     f: dump current graph to file
     a: add node(s) to graph
     d: delete node(s) from graph
     e: expand graph by adding all successors of one node
     p: expand graph by adding all predecessors of one node
    ds: delete all successors of one node
    dp: delete all predecessors of one node
     t: two-way expand (successors and predecessors)
     S: print successors of node
     P: print predecessors of node
     s: save current subgraph
     l: load current subgraph
     h: halt
").


do_command("v", _G, G, G):- !,    format_nodes(G).
do_command("V", Whole, G, G):- !, format_nodes(Whole).
do_command("f", _G, G, G):- !, dump_subgraph(G).
do_command("a", G, CG, CG1):- !, add_some_nodes(G, CG, CG1).
do_command("d", G, CG, CG1):- !,  delete_some_nodes(G, CG, CG1).
do_command("e", G, CG, CG1):- !,  expand_node_suc(G, CG, CG1).
do_command("p", G, CG, CG1):- !,  expand_node_pred(G, CG, CG1).
do_command("ds", G, CG, CG1):- !,  delete_node_suc(G, CG, CG1).
do_command("dp", G, CG, CG1):- !,  delete_node_pred(G, CG, CG1).
do_command("t", G, CG, CG1):- !,  two_way_expand(G, CG, CG1).
do_command("P", G, CG, CG1):- !,  print_predecessors(G, CG, CG1).
do_command("S", G, CG, CG1):- !,  print_sucessors(G, CG, CG1).
do_command("s", _G, CG, CG):- !, save_subgraph(CG).
do_command("l", _G, _, CG):- !, load_subgraph(CG).
do_command("h", _, _, _):- !, halt.
do_command(_, _Graph, G, G):- format("Unrecognized command~n", []).

save_subgraph(G):-
        default_graph_file(DefFile),
        format("Enter graph file name (default: ~w): ", [DefFile]),
        get_line(Filename),
        (
            Filename = "" ->
            File = DefFile
        ;
            atom_codes(File, Filename),
            retract_fact(default_graph_file(_)),
            asserta_fact(default_graph_file(File))
        ),
        open(File, write, OutputStream),
        write(OutputStream, G),
        write(OutputStream, '.'),
        nl(OutputStream),
        close(OutputStream).

load_subgraph(G):-
        format("Enter file name: ", []),
        get_atom(File),
        retract_fact(default_graph_file(_)),
        asserta_fact(default_graph_file(File)),
        open(File, read, InputStream),
        read(InputStream, G),
        close(InputStream).

format_nodes(Graph):-
        vertices(Graph, V),
        format("~n--------------------------~n", []),
        format_nodes_count(V, 4),
        format("~n--------------------------~n~n", []).
format_nodes_count([], _).
format_nodes_count(V, 0):-
        nl,
        format_nodes_count(V, 4).
format_nodes_count([V|Vs], N):-
        N > 0,
        format("~w    ", [V]),
        N1 is N - 1,
        format_nodes_count(Vs, N1).


dump_subgraph(G):-
        default_davinci_file(DefFile),
        format("File name to dump to (default: ~w): ", [DefFile]),
        get_line(FileName),
        (
            FileName = "" ->
            File = DefFile
        ;
            atom_codes(File, FileName),
            retract_fact(default_davinci_file(_)),
            asserta_fact(default_davinci_file(File))
        ),
        write_ugraph_to_file(File, G).


%% Add a set of nodes.

add_some_nodes(Whole, PartGraph, NewPartGraph):-
        format("Enter node name(s): ", []),
        get_atom_set(NewNodes),
        vertices(Whole, V),
        diagnose(NewNodes, V, "not in complete graph", NodesToAdd),
        add_nodes(NodesToAdd, Whole, PartGraph, NewPartGraph).

add_nodes(NodesToAdd, Whole, G, G1):-
        add_vertices(G, NodesToAdd, G0),
        vertices(G0, V),
        edges(Whole, AllEdges),
        filter_edges(AllEdges, V, Edges),
        add_edges(G0, Edges, G1).

filter_edges([], _NodesInGraph, []).
filter_edges([S-E|Nodes], NodesInGraph, [S-E|RestNodes]):-
        ord_member(S, NodesInGraph),
        ord_member(E, NodesInGraph), !,
        filter_edges(Nodes, NodesInGraph, RestNodes).
filter_edges([_-_|Nodes], NodesInGraph, RestNodes):-
        filter_edges(Nodes, NodesInGraph, RestNodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% expand a node

expand_node_suc(Whole, CG, CG1):-
        get_node_names("Enter node name: ", CG, 
                       "not in current subgraph", NodesToExpand),
        (
            NodesToExpand = [] ->
            CG = CG1
        ;
            NodesToExpand = [Node],
            neighbors(Node, Whole, Ngh),
            add_nodes(Ngh, Whole, CG, CG1)
        ).

%% expand a node predecessors

expand_node_pred(Whole, CG, CG1):-
        get_node_names("Enter node name: ", CG, 
                       "not in current subgraph", NodesToExpand),
        (
            NodesToExpand = [] ->
            CG = CG1
        ;
            NodesToExpand = [Node],
            point_to(Node, Whole, AntiNgh),
            add_nodes(AntiNgh, Whole, CG, CG1)
        ).


%% expand predecessors and successors

two_way_expand(Whole, CG, CG1):-
        get_node_names("Enter node name: ", CG, 
                       "not in current subgraph", NodesToExpand),
        (
            NodesToExpand = [] ->
            CG = CG1
        ;
            NodesToExpand = [Node],
            point_to(Node, Whole, AntiNgh),
            add_nodes(AntiNgh, Whole, CG, CG0),
            neighbors(Node, Whole, Ngh),
            add_nodes(Ngh, Whole, CG0, CG1)
        ).


%% delete sucessors

delete_node_suc(_Whole, CG, CG1):-
        get_node_names("Enter node name: ", CG, 
                       "not in current subgraph", NodeToDelete),
        (
            NodeToDelete = [] ->
            CG = CG1
        ;
            NodeToDelete = [ThisNode],
            neighbors(ThisNode, CG, Ngh),
            del_vertices(CG, Ngh, CG1)
        ).


%% delete predecessors

delete_node_pred(_Whole, CG, CG1):-
        get_node_names("Enter node name: ", CG, 
                       "not in current subgraph", NodeToDelete),
        (
            NodeToDelete = [] ->
            CG = CG1
        ;
            NodeToDelete = [ThisNode],
            point_to(ThisNode, CG, AntiNgh),
            del_vertices(CG, AntiNgh, CG1)
        ).

print_sucessors(Whole, CG, CG):-
        get_node_names("Enter node name: ", Whole, 
                       "not in graph", NodesToInspect),
        (
            NodesToInspect = [NodeToInspect] ->
            neighbors(NodeToInspect, Whole, Nei),
            format("Succesors: ~w~n", [Nei])
        ;
            format("User input error: No info to display (no node or too "||
		  "many nodes)~n", [])
        ).

print_predecessors(Whole, CG, CG):-
        get_node_names("Enter node name: ", Whole,
                       "not in graph", NodesToInspect),
        (
            NodesToInspect = [NodeToInspect] ->
            point_to(NodeToInspect, Whole, AntiNei),
            format("Predecessors: ~w~n", [AntiNei])
        ;
            format("User input error: No info to display (no node or too "||
		   "many nodes)~n", [])
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% delete a node

delete_some_nodes(_Whole, PartGraph, NewPartGraph):-
        get_node_names("Enter node name: ", PartGraph, 
                       "not in current subgraph", NodesToDelete),
        del_vertices(PartGraph, NodesToDelete, NewPartGraph).


%%%%%%%%%%%

diagnose(OrigNodes, CompleteSet, Message, NodesToDealWith):-
        ord_intersection_diff(OrigNodes, CompleteSet,
                              NodesToDealWith, Difference),
        (
            Difference = [] ->
            true
        ;
            format("Node(s) ~w ~s.~n", [Difference,Message])
        ).


%%%%%%%%%%

get_node_names(PromptMessage, Container, FailMessage, ResultNodes):-
        format("~s", [PromptMessage]),
        get_atom_set(Nodes),
        vertices(Container, V),
        diagnose(Nodes, V, FailMessage, ResultNodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%

get_atom(Atom):-
        get_line(Str),
        atom_codes(Atom, Str).


get_atom_set(AtomSet):-
        get_line(Str),
        get_list_of_words(AtomList, Str, []),
        list_to_set(AtomList, [], AtomSet).

list_to_set([], Set, Set).
list_to_set([A|As], SIn, SOut):-
        insert(SIn, A, SMid),
        list_to_set(As, SMid, SOut).
