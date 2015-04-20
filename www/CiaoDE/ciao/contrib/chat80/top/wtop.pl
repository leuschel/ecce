
:- module(wtop,[ hi/0, do/0 ],[ ]).
/* CIAO WEBCHAT HTML front end */

:- use_package(pillow).

:- use_module(library(format),[ format/2 ]).
:- use_module(library(llists),[ flatten/2 ]).
:- use_module(library(prolog_sys),[ statistics/2 ]).
:- use_module(library(write),[ write/1 ]).

:- use_module(readin,[ read_in/1, sentences/3 ]).  % parser
:- use_module(top,[ control/3 ]).		   % top level

hi :- html_protect(go).

go:-
	set_prolog_flag(write_strings,on),
 	get_form_input(Info),
	header(Header),
	output_html([
	  Header,
	  --]),
	get_form_value(Info,question,QuestionsI),
 	get_form_value(Info,omode,OModeValI),
 	get_form_value(Info,atime,ATimeValI),
	get_form_value(Info,execmode,EModeValI),
	form_default(QuestionsI,'What is the capital of Spain?',Questions),
	form_default(OModeValI,normal,OModeVal),
	form_default(EModeValI,local,EModeVal),
	form_default(ATimeValI,on,ATimeVal),
	process_question(Questions,OModeVal,ATimeVal,EModeVal,Info),
	output_html([
          --,
	  start_form,
	  bf('Please enter question(s) (end question(s) with ?):'),
	  \\,
	  textinput(question,[cols=56,rows=2],''),
	  \\,
	  input(submit,[value='Submit (text and/or options)']),
	  input(reset,[value='Reset (text and options)']),
	  --,
	  heading(4,'Question processing options:'),
	  radio(omode,normal,OModeVal),
	  ' Normal output (answers + processing messages)', \\,
 	  radio(omode,short,OModeVal),
 	  ' No output', \\,
	  checkbox(atime,ATimeVal),
	  ' Report actual answer computation time (walltime)', \\,
	  --,
	  heading(4,'Execution mode options:'),
          radio(execmode,local,EModeVal),
 	  'Use local execution', \\,
          radio(execmode,remote,EModeVal),
 	  'Use remote country database server (no parallelism)', \\,
          radio(execmode,parallel,EModeVal),
 	  'Use distributed execution for performance (select below)', \\,
          radio(execmode,change,EModeVal),
 	  'Change on-line agents (do not post question)', \\]),
	list_avail_agents(Info),
	signature(Signature),
	output_html([
%% 	  --,
%% 	  heading(4,'Server management options:'),
%% 	  radio(execmode,db_restart,EModeVal),
%%  	  'Restart database server', \\,
%% 	  radio(execmode,wc_restart,EModeVal),
%%  	  'Restart webchat server', \\,
%% 	  radio(execmode,db_kill,EModeVal),
%%  	  'Shut down database server', \\,
%% 	  radio(execmode,wc_kill,EModeVal),
%%  	  'Shut down webchat server', \\,
	  end_form,
	  Signature]).

do :-
      write('Question: '),
      read_in(P),
      process_question(P,_OModeVal,_ATimeVal,_EModeVal,_Info),
      ( P = [bye,'.'] 
      -> true 
       ; do ).

available_agents([aguirre,aguirre,alba,torquemada,sol]).

/* Output check boxes for the available agents */
list_avail_agents(Info) :- 
	available_agents(Agents),
	list_avail_agents_(Agents,Info).

list_avail_agents_([],_).
list_avail_agents_([Name|Rest],Info) :-
	get_form_value(Info,Name,Value),
	output_html([checkbox(Name,Value),tt(Name)]),
	list_avail_agents_(Rest,Info).

process_question(Questions,_OModeVal,_ATimeVal,_EModeVal,_Info) :-
	form_empty_value(Questions),
	!.
process_question(Questions0,_OModeVal,ATimeVal,_EModeVal,_Info) :- 
	what_on_earth_comes_in_this_argument(Questions0,Questions),
	sentences(QuestionS,Questions,[]),
 	statistics(walltime,_),
	process_q(QuestionS,Answers),
	statistics(walltime,[_,Time]),
        html_write_answers(Answers),
	report_time(ATimeVal,Time).

what_on_earth_comes_in_this_argument(Questions0,Questions):-
	atomic(Questions0), !,
	atom_codes(Questions0,Questions).
what_on_earth_comes_in_this_argument(Questions0,Questions):-
	add_blanks(Questions0,Questions1),
	flatten(Questions1,Questions).

add_blanks([],[]).
add_blanks([L|Ls],[L," "|Qs]):-
	add_blanks(Ls,Qs).

/* These are the different execution modes (mchat calls are remote calls!) */
process_q([Q|Qs],[ans(Q,A,T)|As]):-
	control(Q,A,T),
	process_q(Qs,As).
process_q([],[]).

html_write_answers([]).
html_write_answers([ans(S,A,_T)|RA]) :-
	output_html([\\,bf('Parsed question: ')]),
	html_write_question(S),
	output_html([\\,bf('Answer: ')]),
	format("~s",[A]),
	output_html([\\]),
	html_write_answers(RA).

html_write_question([]).
html_write_question([W|Ws]) :- 
	write(W),
	write(' '),
	html_write_question(Ws).

report_time(on,Time) :- 
	!,
	N is Time / 1000,
	number_codes(N,S),
	atom_codes(A,S),
	output_html(
          [heading(4,['(Processed in ', A, ' seconds walltime.)'])]).
report_time(_,_).

mailto(MailList, ref(Ref,tt(MailList)) ) :-
	atom_concat('mailto:',MailList,Ref).

signature([
	--,
	h1([
	       image('/images/pillow_d_t.gif',[alt='PiLLoW']), 
	       font([color=blue],[' Powered by ', 
	       image('/images/ciao_st.gif',[alt='CIAO']), 
	       ' Prolog.'])
	   ]),
	   --,
	   'For any problems or suggestions please contact ',
	   WebMaster,
	   '.',
	   end
	  ]) :- mailto('webmaster@clip.dia.fi.upm.es',WebMaster).

header([
	cgi_reply,
	start,
	begin(body,
	[background='http://www.clip.dia.fi.upm.es/images/Clip_bg.gif']),
	 title('CIAO WEBCHAT Interface'), 
	 h1([
		ref('http://www.clip.dia.fi.upm.es/', 
		image('http://www.clip.dia.fi.upm.es/images/clip.gif', 
		[alt="CLIP Lab"])),
		 ' ',
		 image('/images/ciao_st.gif',[alt='CIAO']),
		 ' WEBCHAT Interface'
	    ])
       ]).

html_write(Term):-
	output_html([begin(pre)]),
	write(Term),
	output_html([end(pre)]).
