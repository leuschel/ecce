:- module(emacs,[emacs_edit/1,emacs_edit_nowait/1,
	         emacs_eval/1,emacs_eval_nowait/1,
		 elisp_string/1],
  	        [assertions,regtypes,isomodes,functions,hiord]).

%% For checking below...
:- use_module(library(terms_check), [ instance/2 ]).

:- use_module(library(lists),[list_concat/2]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(system),[mktemp/2,exec/4,delete_file/1]).
%% TEMP:
%% :- use_module(library(file_utils),[stream_to_string/2]).
%% :- use_module(library(streams),[close_input/1]).

:- comment(title,"Calling emacs from Prolog").

:- comment(author,"The CLIP Group").

:- comment(module,"This library provides a @index{prolog-emacs
   interface}. This interface is complementary to (and independent
   from) the @concept{emacs mode}, which is used to develop programs
   from within the @apl{emacs} editor/environment. Instead, this library
   allows calling @apl{emacs} from a running Prolog program. This
   facilitates the use of  @apl{emacs} as a ``user interface'' for a Prolog
   program. Emacs can be made to:

   @begin{itemize}   

   @item Visit a file, which can then be edited.
 
   @item Execute arbitrary @index{emacs lisp} code, sent from Prolog.
 
   @end{itemize}   

   @noindent
   In order for this library to work correctly, the following is needed: 

   @begin{itemize}   

   @item You should be running the @apl{emacs} editor on the same machine 
         where the executable calling this library is executing. 
 
   @item This @apl{emacs} should be running the @index{emacs server}. This 
         can be done by including the following line in your @file{.emacs} 
         file:  

@begin{verbatim}   
;; Start a server that emacsclient can connect to.
(server-start)
@end{verbatim}   

         @noindent Or typing @tt{M-x server-start} within @apl{emacs}.
         
   @end{itemize}   

   @noindent This suffices for using @apl{emacs} to edit files. For
   running arbitrary code the following also needs to be added to the
   @file{.emacs} file:

   @begin{description}   

   @item{@tt{(setq enable-local-eval t)}} Allows executing lisp code 
   without asking. 

   @item{@tt{(setq enable-local-eval nil)}} Does not allow executing lisp 
   code without asking. 

   @item{@tt{(setq enable-local-eval 'maybe)}} Allows executing lisp code 
   only if user agrees after asking (asks interactively for every invocation). 

   @end{description}   

   @noindent @bf{Examples:}

   @noindent Assuming that a @tt{.pl} file loads this library, then:

   @begin{description}   

   @item{@tt{..., emacs_edit('foo'), ...}} Opens file @tt{foo} for
   editing in @apl{emacs}.

   @item{@tt{..., emacs_eval_nowait(""(run-ciao-toplevel)""), ...}}
   Starts execution of a Ciao top-level within @apl{emacs}.

   @end{description}   

   ").

%---------------------------------------------------------------------------
:- pred emacs_edit(+filename) 
   # "Opens the given file for editing in @apl{emacs}. Waits for editing to 
      finish before continuing.". 

emacs_edit(File) :-
	check_type(File,filename,'emacs_edit/1',1),
	emacs_edit_file(File,wait).

%---------------------------------------------------------------------------
:- pred emacs_edit_nowait(+filename) 
   # "Opens the given file for editing in @apl{emacs} and continues
      without waiting for editing to finish.". 

emacs_edit_nowait(File) :-
	check_type(File,filename,'emacs_edit_nowait/1',1),
	emacs_edit_file(File,nowait).

%---------------------------------------------------------------------------
:- pred emacs_eval(+elisp_string) 
   # "Executes in emacs the lisp code given as argument. Waits for the 
      command to finish before continuing.". 

emacs_eval(Command) :-
	check_type(Command,elisp_string,'emacs_eval/1',1),
	emacs_eval_expression(Command,wait).

%---------------------------------------------------------------------------
:- pred emacs_eval_nowait(+elisp_string) 
   # "Executes in emacs the lisp code given as argument and continues
      without waiting for it to finish.". 

emacs_eval_nowait(Command) :-
	check_type(Command,elisp_string,'emacs_eval_nowait/1',1),
	emacs_eval_expression(Command,nowait).

%---------------------------------------------------------------------------
:- regtype filename(F) 
   # "@var{F} is an atom which is the name of a file.". 

filename(L) :- atm(L).
%---------------------------------------------------------------------------
:- regtype elisp_string(L) 
   # "@var{L} is a string containing @apl{emacs} lisp code.". 

elisp_string(L) :- string(L).
%---------------------------------------------------------------------------

emacs_edit_file(File,Wait) :-
	emacs_process_file(File,Wait).

emacs_eval_expression(Command,Wait) :-
	mktemp('pl2emacsXXXXXX',TmpFileN),
	atom_concat([~tmp_dir,TmpFileN],TmpFile),
       	open(TmpFile,write,O),
	set_output(O),
	display_string(
          ~list_concat([~pre,
	                Command,
	                ~post(~atom_codes(TmpFile),~atom_codes(TmpFileN))
		        ])),
	close(O),
	emacs_process_file(TmpFile,Wait).


emacs_process_file(TmpFile,Wait) :-
	( Wait == wait 
	-> NoWait = ' '
	;  NoWait = ' --no-wait '),
	exec(~atom_concat(['emacsclient',NoWait,TmpFile]),_In,_Out,Err),
	local_stream_to_string(Err,Errors),
	(  Errors == []
	-> true
	;  atom_codes(AErrors,Errors),
	   %% delete_file(TmpFile),
	   throw(error(AErrors,'emacs/1')) ).
        %% The atom 'emacs/1' is a kludge to avoid bug in functions lib which 
        %% does not allow preventing / from bing interpreted as division.

local_stream_to_string(IO, String) :-
        get_code(IO,Code),
        get_string(Code, IO, String).

get_string(-1, _IO, []) :- 
	!.
%%        close_input(IO). %% Fails...
get_string(C, IO, [C|Cs]) :-
        get_code(IO, D),
        get_string(D, IO, Cs).

tmp_dir('/tmp/').
       
pre("

; At least one non-empty line before 'Local ...' needed to avoid complaints

(defun actions-hook ()

  ; Here, the functions to be evaluated

    ").

post(TmpFile,TmpFileN,~list_concat(["

   ; End of functions
   (kill-buffer \"", TmpFileN, "\")
   (delete-file \"", TmpFile, "\")
   )

;;; Local Variables:
;;; eval: (save-excursion (load-file \"", TmpFile, "\") (actions-hook) )
;;; End:

      "])).


% This should be imported from rtchecks:

check_type(Arg,Type,Pred,ArgN) :-
	\+ \+ system_dependent_disentailed(Type,Arg),
	!,
	%% The quote is just a kludge...
	throw(error(type_error(Type, Arg),^(Pred-ArgN))).
check_type(_,_,_,_).
	
%%% This is all from rt_checks:

% This is correct and useful in any system
system_dependent_disentailed(Prop,Arg):-
	system_dependent_incompatible(Prop,Arg),!.
% This is only correct for complete solvers (such as herbrand)
system_dependent_disentailed(Prop,Arg):-
	copy_term(Arg,OrigArg),
	Prop(Arg),
	!,
	\+(instance(OrigArg,Arg)),
	instance(Arg,OrigArg).

% This definition works in any system
system_dependent_incompatible(Prop,Arg):-
	\+(Prop(Arg)).
