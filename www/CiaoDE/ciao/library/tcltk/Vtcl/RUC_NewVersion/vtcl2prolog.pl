:- module(_,[main/1],[assertions,isomodes,regtypes]).



:- use_module(library(lists)).
:- use_module(library(file_utils)).
:- use_module(library(strings)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(filenames)).
:- use_module(library(system)).


main(Args) :- 

% Arguments Verification
   verify_args(Args,Filename),
   !,
% Construction of the pl file from the argument Filename (tcl file)
   build_pl_file(Filename,Name),

%Construction of the main template
   build_main(Name). 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% verify_args print an usage message if there is an error in the arguments
verify_args(Args,A):-
      length(Args,L),
      (L =\= 1 -> usage ; 
      [A]=Args,
      verify_file(A)). 

% usage prints the correct use of the program in the current output
% and ends the execution
usage:- 
      write_string("\nUsage:\n\n\t vtcl2prolog  <tcl file>\n"),
      write_string("\nWhere <tcl file> is a valid vtcl project\n"),
      write_string("\nExample: vtcl2prolog foo.tcl\n\n"),
      halt(1).

% verify_file check the existence of the file passed as argument
verify_file(A):-
    file_exists(A).

% if the file does not exist it prints the error in the current output
verify_file(A):-
    write_string("\n The tcl file '"),
    write(A),
    write_string("' does not exist\n"),
    usage. 



build_pl_file(Filename,Name):-

% Open the file passed as argument  
        file_name_extension(Filename,Name,_),
        file_name_extension(Plname,Name,'.pl'),
      
        open(Plname,'write',Plfile),

        file_to_string(Filename,Filetcl),

% Write the first line of the code with the exported method
% and the libraries that the program needs
        write_string(Plfile,":- module("),
        write(Plfile,Name),
        write_string(Plfile,",["),
        write(Plfile,Name),        
        write_string(Plfile,"_tcl_open/0],[]).


:- use_module(tcltk).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(file_utils)).
:- use_module(library(strings)).
:- use_module(library(write)).


"),

% Write the main procedure...
% and the auxiliar procedures

 write(Plfile,Name),
 write_string(Plfile,"_tcl_open:-

    
     create_interpret(Interp),
     send_code_vtcl(Interp),

     vtcl_receive(Interp).    


create_interpret(Interp):- 
         
     tk_new([],Interp).


send_code_vtcl('$wish$'(Strm,_,_)):-

     code_vtcl(Scritcl),
     write_string(Strm,Scritcl),
     flush_output(Strm).


vtcl_receive(Interp):-
     tk_event_loop(Interp).


code_vtcl("),

% code_vtcl is a fact that codifies the Vtcl project code

   write(Plfile,Filetcl),
   write_string(Plfile,")."),
        
     close(Plfile).
        
build_main(Name):-

    open('main.pl','write',Maine),
    


    write_string(Maine,":-module(_,[main/0],[]).


:-use_module("),

    write(Maine,Name),
    write_string(Maine,").

%------------------------------------------------
% Here the programmer has to include the modules
% used by the application
%
%:-use_module(...).
% ... ... ...
%
%-----------------------------------------------


main:-

      "),

    write(Maine,Name),
    write_string(Maine,"_tcl_open.").
