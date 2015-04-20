%%------------------------------------------------------------------------
%% TCL/TK low level library
%%------------------------------------------------------------------------

:- module(tcltk_low_level,[],[assertions,isomodes,regtypes]).

:- use_module(library(terms)).

%%------------------------------------------------------------------------
:- comment(title,
        "Low level interface library to Tcl/Tk").

:- comment(author,"Montse Iglesias Urraca").
%%------------------------------------------------------------------------

:- export(new_interp/1).
:- export(new_interp/2).
:- export(new_interp_file/2).

:- export(tcltk/2).
:- export(tcltk_raw_code/2).
%:- export(copy_stdin/1).
%:- export(receive_term/3).
%:- export(receive_term/2).
:- export(receive_result/2).
:- export(send_term/2). 

:- export(receive_event/2).
:- export(receive_list/2).
:- export(receive_confirm/2).
%:- export(tcl_error/1).
%:- export(tcl_result/1).
%:- export(delete_item_queue/1).
:- export(delete/1).


:- use_module(library(sockets)).
:- use_module(library(system)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(strings)).
:- use_module(library(lists)).
:- use_module(library(format),[format/3]).

:- set_prolog_flag(multi_arity_warnings, off).

%%-----------------------------------------------------------------------

:- comment(module,"The @lib{tcltk_low_level} library defines the low
   level interface used by the @lib{tcltk} library. Essentially it
   includes all the code related directly to the handling of sockets
   and processes. This library should normally not be used directly by
   user programs, which use @lib{tcltk} instead. On the other hand in
   some cases it may be useful to undertand how this library works in
   order to understand possible problems in programs that use the
   @lib{tcltk} library.").

:- comment(appendix,"

   Two sockets are created to connect the @em{TclInterpreter} and the
   prolog process: the @em{event_socket} and the @em{term_socket}.
   There are two global variables: @em{prolog_variables} and
   @em{terms}. The value of any of the variables in the goal that is
   bound to a term will be stored in the array @tt{prolog_variables}
   with the variable name as index.  The string which contains the
   printed representation of prolog @em{terms} is @em{Terms}.  These
   are the Tcl/Tk procedures which implement the interface (the code
   is inside the @lib{tcltk_low_level} library):

   @begin{description}

   @item{@tt{prolog}} Sends to @em{term_socket} the predicate
        tcl_result which contains the goal to execute. Returns the
        string executes and the goal.

   @item{@tt{prolog_event}} Adds the new @em{term} to the @em{terms}
        queue.

   @item{@tt{prolog_delete_event}} Deletes the first @em{term} of the
        @em{terms} queue.

   @item{@tt{prolog_list_events}} Sends all the @em{terms} of the
        @em{terms} queue by the @em{event_socket}. The last element
        will be @em{end_of_event_list}.

   @item{@tt{prolog_cmd}} Receives as an argument the tcltk
        code. Evaluates the code and returns through the
        @em{term_socket} the term @em{tcl_error} if there was a
        mistake in the code or the predicate @em{tcl_result} with the
        result of the command executed. If the argument is @em{prolog}
        with a goal to execute, before finishing, the predicate
        evaluated by prolog is received. In order to get the value of
        the variables, predicates are compared using the
        @em{unify_term} procedure.  Returs 0 when the sript runs
        without errors, and 1 if there is an error.

   @item{@tt{prolog_one_event}} Receives as an argument the @em{term}
        which is associated with one of the tk events. Sends through
        the @em{event_socket} the @em{term} and waits the unificated
        @em{term} by prolog. After that it calls the @em{unify_term}
        procedure to obtain the value of the @em{prolog_variables}.

   @item{@tt{prolog_thread_event}} Receives as an argument the
        @em{term} which is associated with one of the tk events. Sends
        through the @em{event_socket} the @em{term} and waits for the
        @em{term} unified by prolog. After that the @em{unify_term}
        procedure is called to obtain the value of the
        @em{prolog_variables}.  In this case the @em{term_socket} is
        non blocking.

   @item{@tt{convert_variables}} Receives as an argument a string
        which contains symbols that can't be sent by the sockets. This
        procedure deletes them from the input string and returns the
        new string.

   @item{@tt{unify_term}} Receives as argument a prolog term.

   @end{description}").
%%------------------------------------------------------------------------

:- regtype tclInterpreter(I) # "@var{I} is a reference to a @em{Tcl}
   interpreter.".

tclInterpreter(_).

:- regtype tclCommand(C) # "@var{C} is a @em{Tcl} command.".

tclCommand(_).

%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
%% CONSTRUCTOR / DESTRUCTOR
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
:- pred new_interp(-TclInterpreter) :: tclInterpreter 

        # "Creates two sockets to connect to the @em{wish} process,
           the term socket and the event socket, and opens a pipe to
           process @em{wish} in a new shell.".

:- pred new_interp_file(+FileName,-TclInterpreter) :: string * tclInterpreter

        # "Creates two sockets, the term socket and the event socket,
           and opens a pipe to process @em{wish} in a new shell
           invoked with a @var{FileName}. @var{FileName} is treated as
           a name of a sript file".

:- pred new_interp(-TclInterpreter,+Options) :: tclInterpreter * atom

        # "Creates two sockets, the term socket and the event socket,
           and opens a pipe to process @em{wish} in a new shell
           invoked with the @var{Options}.".

%%------------------------------------------------------------------------

new_interp(X) :-
        nonvar(X),
        !,
        fail.

new_interp('$wish$'(Strm,TermStream,EventStream)) :-
        current_host(Host),
        bind_socket(Port,1,Socket),
        number_codes(Port,PortCode),
        popen('wish',write,Strm),
        write_string(Strm,"set prolog_host "),
        write(Strm,Host),nl(Strm),flush_output(Strm),
        write_string(Strm,"set prolog_port "),
        write_string(Strm,PortCode),nl(Strm),
        flush_output(Strm),
        bind_socket(EPort,1,ESocket),
        number_codes(EPort,EPortCode),
        write_string(Strm,"set event_port "),
        write_string(Strm,EPortCode),nl(Strm),
        flush_output(Strm),
        send_initial_code(Strm),
        socket_accept(Socket,TermStream),
        socket_accept(ESocket,EventStream),
        true.


new_interp_file(_,X) :-
        nonvar(X),
        !,
        fail.

new_interp_file(File,'$wish$'(Strm,TermStream,EventStream)) :-
        current_host(Host),
        bind_socket(Port,1,Socket),
        number_codes(Port,PortCode),
        atom_concat(['bltwish <',File,' &'],Command),
        popen(Command,write,Strm),
        write_string(Strm,"set prolog_host "),
        write(Strm,Host),nl(Strm),flush_output(Strm),
        write_string(Strm,"set prolog_port "),
        write_string(Strm,PortCode),nl(Strm),
        flush_output(Strm),
        bind_socket(EPort,1,ESocket),
        number_codes(EPort,EPortCode),
        write_string(Strm,"set event_port "),
        write_string(Strm,EPortCode),nl(Strm),
        flush_output(Strm),
        send_initial_code(Strm),
        socket_accept(Socket,TermStream),
        socket_accept(ESocket,EventStream),
        true.

new_interp('$wish$'(Strm,TermStream,EventStream),Options) :-
        current_host(Host),
        bind_socket(Port,1,Socket),
        number_codes(Port,PortCode),
        atom_concat('wishx',Options,V),
        popen(V,write,Strm),
        write_string(Strm,"set prolog_host "),
        write(Strm,Host),nl(Strm),flush_output(Strm),
        write_string(Strm,"set prolog_port "),
        write_string(Strm,PortCode),nl(Strm),
        flush_output(Strm),
        bind_socket(EPort,1,ESocket),
        number_codes(EPort,EPortCode),
        write_string(Strm,"set event_port "),
        write_string(Strm,EPortCode),nl(Strm),
        flush_output(Strm),
        send_initial_code(Strm),
        socket_accept(Socket,TermStream),
        socket_accept(ESocket,EventStream),
        true.

%%------------------------------------------------------------------------
:- pred delete(+TclInterpreter) :: tclInterpreter 

        # "Terminates the @em{wish} process and closes the pipe, term
          socket and event socket. Deletes the interpreter
          @var{TclInterpreter} from the system".
%%------------------------------------------------------------------------

delete('$wish$'(Strm,TermStrm,EventStrm)) :-
% was commented
        write_string(Strm,"uplevel 0 exit"),
        nl(Strm),
        flush_output(Strm),
% end comment
        close(TermStrm),
        close(Strm),
        close(EventStrm).

%%------------------------------------------------------------------------
%:- pred delete_item_queue(+TclInterpreter) :: tclInterpreter 
%
%       # "".
%%------------------------------------------------------------------------

%delete_item_queue('$wish$'(Strm1,Strm2,Strm3)) :-
%       display('En el delete'),nl,
%       write_string(Strm1,"prolog_delete_event "),
%       nl(Strm1),
%       flush_output(Strm1),
%%      display('En el delete enviado'),nl,
%%      tcltk_raw_code("prolog_delete_event ",'$wish$'(Strm1,Strm2,Strm3)),
%       read_term(Strm3,Term,[]),
%       display(Term).

%%------------------------------------------------------------------------
send_initial_code(Strm) :-
        core(String),
        write_string(Strm,String),
        nl(Strm),
        flush_output(Strm),
        fail.

send_initial_code(_).

%%------------------------------------------------------------------------
%% SEND BASIC TCLTK CODE ITEMS TO WISH
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred tcltk_raw_code(+String,+TclInterpreter) :: string * tclInterpreter 

        # "Sends the tcltk code items of the @var{Stream} to the
          @var{TclInterpreter}".
%%------------------------------------------------------------------------

tcltk_raw_code(Str,'$wish$'(Strm,_,_)) :-
        string(Str,String,""),
        !,
        write_string(Strm,String),
        nl(Strm),
        flush_output(Strm).

copy_stdin('$wish$'(Strm,_,_)) :-
        !,
        copy_stdin_aux(Strm).

copy_stdin_aux(Strm) :-
        get_code(Byte),
        Byte =\= -1,
        !,
        put_code(Strm,Byte),
        flush_output(Strm),
        copy_stdin_aux(Strm).

copy_stdin_aux(_).

%%------------------------------------------------------------------------
%% MACRO IN ORDER TO SEND TCL/TK CODE TO WISH
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred tcltk(+Code,+TclInterpreter) :: tclCommand * tclInterpreter 

        # "Sends the @var{Code} converted to string to the @var{TclInterpreter}".
%%------------------------------------------------------------------------

tcltk(Code,'$wish$'(Strm,_,_)) :-
        !,
        nl(Strm),
        send_code(Code,Strm).

%%------------------------------------------------------------------------

send_code([],Strm) :-
        !,
        nl(Strm),
        flush_output(Strm).

send_code([Number|Nc],Strm) :-
        number(Number),
        !,
        number_codes(Number,NumberAsCode),
        write_string(Strm,NumberAsCode),
        write(Strm,' '),
        send_code(Nc,Strm).
        
send_code([chars(String)|Nc],Strm) :-
        !,
        send_code([String|Nc],Strm).

send_code([dq(Code)|Nc],Strm) :-
        write(Strm,'\"'),
        send_code(Code,Strm),
        write(Strm,'\" '),
        !,
        send_code(Nc,Strm).

send_code([sqb(Code)|Nc],Strm) :-
        write(Strm,'['),
        send_code(Code,Strm),
        write(Strm,'] '),
        !,
        send_code(Nc,Strm).

send_code([br(Code)|Nc],Strm) :-
        write(Strm,'{'),
%       display('in the send code'),nl,
        send_code(Code,Strm),
%       display('en send code'),nl,
        write(Strm,'} '),
        !,
        send_code(Nc,Strm).

send_code([min(Code)|Nc],Strm) :-
        atom(Code),
        !,
        write(Strm,'-'),
        write(Strm,Code),
        write(Strm,' '),
        send_code(Nc,Strm).

send_code([format(Fmt,Args)|Nc],Strm) :-
        format(Strm,Fmt,Args),
        !,
        send_code(Nc,Strm).

send_code([write(Term)|Nc],Strm) :-
        write(Strm,Term),
        !,
        send_code(Nc,Strm).

send_code([tcl(Var)|Nc],Strm) :-
        number_codes(Var,Str),
        atom_codes(Atom,Str),
        write(Strm,Atom),
        write(Strm,' '),
        send_code(Nc,Strm).

send_code([Atom|Nc],Strm) :-
        atom(Atom),
        !,
        write(Strm,Atom),
        write(Strm,' '),
        send_code(Nc,Strm).

send_code([Str|Nc],Strm) :-
        string(Str,String,""),
        !,
        write_string(Strm,String),
        write(Strm,' '),
        send_code(Nc,Strm).

send_code([_|Nc],Strm) :-
        !,
        send_code(Nc,Strm).

send_code(Not_a_list,Strm) :-
        !,
        send_code([Not_a_list],Strm).

%%------------------------------------------------------------------------
%% SEND A PROLOG TERM TO TCL/TK
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred send_term(+String,+TclInterpreter) :: string * tclInterpreter 

        # "Sends the goal executed to the
          @var{TclInterpreter}. @var{String} has the predicate with
          unified variables".
%%------------------------------------------------------------------------

send_term(Term,'$wish$'(_,Stream,_)) :-
        write_term(Stream,Term,[]),
        nl(Stream),flush_output(Stream).
        

%%------------------------------------------------------------------------
%% READ A PROLOG TERM FROM TCL/TK
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred receive_result(-Result,+TclInterpreter) :: string * tclInterpreter 

        # "Receives the @var{Result} of the last @em{TclCommand} into
           the @var{TclInterpreter}. If the @em{TclCommand} is not
           correct the @em{wish} process is terminated and a message
           appears showing the error".
%%------------------------------------------------------------------------
receive_result(Result,I) :-
        receive_term(Term,I),
        get_result(Term,Result,I).

get_result(:(tcltk_low_level,tcl_result(X)),Result,_) :-
        Result = X.

get_result(:(tcltk_low_level,tcl_error(_)),_,I) :-
        delete(I),
        fail.

receive_term(Term,'$wish$'(_,Stream,_)) :-
        read_term(Stream,Term,[]),
        exceptions(Term).

receive_term(Term,VarNames,'$wish$'(_,Stream,_)) :-
        read_term(Stream,Term,[variable_names(VarNames)]).

%%------------------------------------------------------------------------
%% EXCEPTIONS FROM TCL/TK
%%------------------------------------------------------------------------

exceptions(Term) :-
        catch(Term,_,_).
%       catch(Term,Error,handle(Error)).

tcl_error(Text) :-
        write('Tcl exception: '),
        write_string(Text),
        nl.

tcl_result(_).

%handle(X) :-
%       write(X),
%       nl,
%       halt.
%       abort.

%%________________________________________________________________________
%% READ A PROLOG LIST OF TERMS FROM TCLTK
%%________________________________________________________________________
%%------------------------------------------------------------------------
:- pred receive_event(-Event,+TclInterpreter) :: list * tclInterpreter 

        # "Receives the @var{Event} from the event socket of the
          @var{TclInterpreter}.".
%%------------------------------------------------------------------------

receive_event([Term],'$wish$'(_,_,Stream)) :-
%       display('before read'),nl,
        read_term(Stream,Term,[]).
%       display('after read'),nl.


%%------------------------------------------------------------------------

%receive_list([],_,end_of_event_list).

%receive_list(Nt,'$wish$'(_,_,Stream),_) :-
%       display('before calling a receive__list'),nl,
%       receive_list(Nt,'$wish$'(_,_,Stream)).


%receive_list([Term|Nt],'$wish$'(_,_,Stream)) :-
%       display('In the receive list '),
%       read_term(Stream,Term,[]),
%       display(Term),nl,
%       display('Before the receive list3'),nl,
%       receive_list(Nt,'$wish$'(_,_,Stream),Term),
%       display('In en of receive list'),nl.

%receive_list([Term|Nt],'$wish$'(_,_,Stream)) :-
%%      display('In the receive list '),
%       read_term(Stream,Term,[]),
%%      display(Term),nl,
%       ( Term = end_of_event_list -> Nt = [] 
%       ; receive_list(Nt,'$wish$'(_,_,Stream))).
%%      display('End of receive list ').

%%------------------------------------------------------------------------
:- pred receive_list(-List,+TclInterpreter) :: list * tclInterpreter 

        # "Receives the @var{List} from the event socket of the
          @var{TclInterpreter}.The @var{List} has all the predicates
          that have been inserted from Tcl/Tk with the command
          prolog_event.  It is a list of terms.".
%%------------------------------------------------------------------------
receive_list([Term|Nt],'$wish$'(_,_,Stream)) :-
        read_term(Stream,Term,[]),
        Term \== end_of_event_list,!,
        receive_list(Nt,'$wish$'(_,_,Stream)).
receive_list([end_of_event_list],_).

%%------------------------------------------------------------------------
:- pred receive_confirm(-String,+TclInterpreter) :: string * tclInterpreter 

        # "Receives the @var{String} from the event socket of the
          @var{TclInterpreter} when a term inserted into the event
          queue is managed.".
%%------------------------------------------------------------------------
receive_confirm(Term,'$wish$'(_,_,Stream)) :-
%       display('In confirm'),nl,
        read_term(Stream,Term,[]).
%%------------------------------------------------------------------------
%% EVALUATE IF AN ERROR OCCURS IN THE TCLTK SCRIPT
%%------------------------------------------------------------------------



%%------------------------------------------------------------------------
%% INITIAL CODE
%%------------------------------------------------------------------------

%core("wm withdraw .").

%%------------------------------------------------------------------------
:- comment(doinclude, core/1).

:- pred core(+String) :: string  

        # "@pred{core/1} is a set of facts which contain @var{String}s
           to be sent to the Tcl/Tk interpreter on startup.  They
           implement miscelaneous Tcl/Tk procedures which are used by
           the Tcl/Tk interface.".

%%------------------------------------------------------------------------

core("set event_socket [socket $prolog_host $event_port]").
%core("puts $event_port").
%core("puts $event_socket").

core("set term_socket [socket $prolog_host $prolog_port]").
%core("puts $prolog_port").
%core("puts $term_socket").

%core("[fconfigure $event_socket -blocking false]").

%core(" global prolog_variables").
core("set prolog_variables(X) 1").
core("set terms [list] ").

core("proc prolog_term {term} {").
core("global event_socket").
core(" puts  $event_socket $term. ").
core(" flush $event_socket ").
core("} ").

core("proc prolog {agoal} {").
%core(" prolog_term goal($agoal) ").
%core(" prolog_term execute($agoal) ").
core(" global term_socket").
core("  puts  $term_socket tcltk_low_level:tcl_result(execute($agoal)).").
core("  flush $term_socket").
core("  return execute($agoal) ").
%core("  return execute ").
core("} ").
core("proc prolog1 {agoal} {").
core(" global event_socket").
core("   puts  $event_socket $agoal.").
core("  flush $event_socket").
core("  return execute($agoal) ").
%core("  return execute ").
core("} ").

core("proc prolog_event {term} {").
core("global terms").
core(" set terms [concat $terms $term] ").
core(" return 0 ").
core("} ").

core("proc prolog_delete_event { } {").
core("global terms").
core("global event_socket").
core(" set terms [lreplace $terms 0 0 ] ").
core(" puts  $event_socket \"end_of_event_list.\" ").
core(" flush $event_socket ").
%core(" puts  finprologdelete ").
core(" return 0 ").
core("} ").

core("proc prolog_list_events { } {").
core("global event_socket").
core("global terms").
core(" set nterms [llength $terms] ").
core(" set x 0 ").
core(" while {$x<$nterms} { ").
core("    puts $event_socket [lindex $terms $x]. ").
core("    flush $event_socket ").
core("    incr x ").
core(" } ").
core(" puts  $event_socket \"end_of_event_list.\" ").
core(" flush $event_socket ").
core(" return 0 ").
core("} ").

% Execute command and sendresults back to prolog
% This is internally used by tcl_eval/3.
% return 0 when the scripts runs without errors, and 1 if there is an error

core("proc prolog_cmd {command} {").
core(" global term_socket").
%core("   set result [catch {uplevel $command} var]").
%core(" puts $command ").
core("   set error [catch {set result [uplevel $command]} var]").
%core("   puts $command").
%core("   puts $error").
%core("   if {$result} {").
core("   if {$error} {").
core("       set var [convert_variable $var ]").
core("       puts  $term_socket tcltk_low_level:tcl_error(\\""$var\\"").").
core("       flush $term_socket").
%%core("       return $result").
core("       return $error").
core("   } else { ").
core("       set result_aux [string compare [string range $result 0 6 ] execute ]").
core("       if { $result_aux } {").
core("          puts  $term_socket tcltk_low_level:tcl_result(\\""$result\\"").").
core("          flush $term_socket").
core("       } else { ").
core("          gets  $term_socket term ").
%core("          puts ahora_me_fijo").
%core("          puts $result ").
%core("          puts $term").
core("          set ret [unify_term $result $term]").
core("          set error $ret").
core("          } ").
%core("       return $result").
core("       return $error").
core("   } ").
core("} ").


% Execute command and send results back to prolog

core("proc prolog_one_event {a_term} {").
core(" global event_socket").
core(" global term_socket").
%% new global variable prolog_variable, wich contain the result of the unification
core(" global prolog_variables").
core("   set result 0").
core("   set result_var 0 ").
%core("   puts $a_term ").
%core("   puts $result ").

core("   puts  $event_socket $a_term.").
core("   flush $event_socket").
%core(" puts antesdegets ").
core("   gets  $term_socket result").
%core(" puts despuesdegets ").
core("   set ret [unify_term $a_term $result]").
%core(" puts despuesdeunify ").
%core("   gets  $event_socket $result_var").
%core("   puts $result_var").
core("} ").

% Execute command and send results back to prolog

core("proc prolog_thread_event {a_term} {").
core(" global event_socket").
core(" global term_socket").

%% new global variable prolog_variable, wich contain the result of the unification
core(" global prolog_variables").
core("   fconfigure $term_socket -blocking false").
core("   set result 0").
core("   set result_var 0 ").
core("   puts  $event_socket $a_term.").
core("   flush $event_socket").
core("   gets  $term_socket result").
core("   set ret [unify_term $a_term $result]").
core("} ").

core("proc convert_variable {var} {").
core("   set result \" \" ").
core("   while (1) { ").
core("      set long [string length $var] ").
core("      set pos [string first \"\\""\" $var] ").
core("      if { $pos == -1 } { ").
core("           set result ${result}${var} ").
core("           return $result } ").
core("      incr pos -1 ").
core("      set new [string range $var 0 $pos] ").
core("      incr pos ").
core("      incr pos ").
core("      set var [string range $var $pos $long] ").
core("      set result ${result}${new}'' ").
core("  } ").
core("} ").

% the value of the unification have to be introduced in the global array 
% prolog_variables with the variable name as index
core("proc unify_term {term result} {").
core(" global prolog_variables").

core("   set long [string length $term] ").

%Modified line:
core("   incr long -2 ").

% Instead of:
%core("  incr long -3 ").

core("   set term [string range $term 8 $long] ").
% the term without execute
core("   set ret [unify_term_aux $term $result ]").
core("   if { $ret == 0 } { ").
core("        return 0 } ").
%core("   else {          ").
core("        return 1 ").
%core("     } ").
core("} ").

core("proc unify_term_aux {term result} {").
core(" global prolog_variables").

core("   set n_c_t -1").
core("   set n_c_r -1").

core("   set pos_t [string first \"(\" $term] ").
core("   set pos_r [string first \"(\" $result] ").
% the name of the predicate has to be the same
% first returns -1 if dosent found the braquet
core("   if { $pos_t == -1 } { ").
core("      if { $pos_r == -1 } { ").
core("         set comp [string compare $term $result] ").
% compare returns 0 if the strings are equal
core("         if { $comp == 0 } { ").
core("            return 1 }").
%core("         else { ").
%core("            return 0 }").
core("            return 0 ").
core("         }").
core("      }").

% 
core("   if { $pos_t == $pos_r } { ").
core("        incr pos_t 1  ").
core("        incr pos_r 1  ").
core("        set long_t [string length $term] ").
core("        set long_r [string length $result] ").
core("        set term_aux [string range $term $pos_t $long_t] ").
core("        set result_aux [string range $result $pos_r $long_r] ").
core("        set long_t [string length $term_aux] ").
core("        set long_r [string length $result_aux] ").
core("        while {$long_t !=  0} { ").
core("             set long_t_1 [string first \",\" $term_aux] ").
core("             set long_r_1 [string first \",\" $result_aux] ").
core("             if { $long_t_1 == -1 || $long_r_1 == -1} { ").
core("                  set long_t_1 [string first \")\" $term_aux] ").
core("                  set long_r_1 [string first \")\" $result_aux] ").

% Modified code --------
core("                  set n_t [expr $long_t_1+1] ").
core("                  set n_r [expr $long_r_1+1] ").
core("                  set n_c_t [string first \")\" $term_aux $n_t] ").
core("                  set n_c_r [string first \")\" $result_aux $n_r]").

core("             } ").

core("             if {$n_c_t == -1} { ").
core("                  incr long_t_1 -1} else {set n_c_t -1}").
core("             if {$n_c_r == -1} { ").
core("                  incr long_r_1 -1} else {set n_c_r -1} ").
% ----------------------
core("             set term_aux_1 [string range $term_aux 0 $long_t_1] ").
core("             set result_aux_1 [string range $result_aux 0 $long_r_1] ").
core("             set prolog_variables($term_aux_1) $result_aux_1 ").
core("             incr long_t_1 2  ").
core("             incr long_r_1 2  ").
core("             if {$long_t <= $long_t_1 || $long_r <= $long_r_1 } { ").
%core(" puts $prolog_variables(Outputval)").
core("                  return 0 }").
core("             set term_aux [string range $term_aux $long_t_1 $long_t] ").
core("             set result_aux [string range $result_aux $long_r_1 $long_r] ").
core("             set long_t [string length $term_aux] ").
core("             set long_r [string length $result_aux] ").
core("        } ").
core("   } ").
core("} ").
