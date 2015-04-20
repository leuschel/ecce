%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% RUNTIME SUPPORT FOR OBJECT MANIPULATION
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

:- module(objects_rt,[
	% -- Predicates --
	new/2,
	static_new/2,
	instance_of/2,
	derived_from/2,
	interface/2,
	instance_codes/2,
	destroy/1,
	use_class/1,
	% -- Properties --
	constructor/1,
	class_name/1,
	interface_name/1,
	instance_id/1,
	class_source/1,
	interface_source/1,
	method_spec/1,
	virtual_method_spec/1
        ]).

:- use_package(assertions).
:- use_package(basicmodes).

%%------------------------------------------------------------------------

:- use_module(engine(internals)).
:- use_module(library(compiler),[use_module/1]).
:- use_module(library(prolog_sys),[statistics/2,new_atom/1]).
:- use_module(library(system),[time/1,current_host/1,get_pid/1,mktemp/2]).

%%------------------------------------------------------------------------

:- multifile '$class$'/1.
:- multifile 'class$super'/2.
:- multifile 'class$attr_template'/4.
:- multifile 'class$initial_state'/3.
:- multifile 'class$default_cons'/1.
:- multifile 'class$constructor'/4.
:- multifile 'class$destructor'/3.
:- multifile 'id$fromclass'/2.
:- multifile 'class$implements'/2.
:- multifile 'class$call'/3.

:- data      'id$fromclass'/2.
:- data      'id$recycled'/1.

'class$call'(Module,Goal,From) :-
	atom(Module),
	last_module_exp(Goal,goal,From,Module,Exp),
	!,
	'$meta_call'(Exp).

%%------------------------------------------------------------------------
%% DOC HEADERS
%%------------------------------------------------------------------------

:- comment(title,
	"Run time usage of objects").

:- comment(author,"Angel Fernandez Pineda").

:- comment(usage,
	"This library is automatically loaded when using the 
	  @em{objects} package:
@begin{verbatim}
    :- module(@em{ModuleName},@em{Exports},[objects]).
@end{verbatim}
         Nothing special needs to be done.
        ").

:- comment(title,
	"Handling with objects").

:- comment(subtitle,
	"Run-time support for O'Ciao object manipulation").

:- comment(author,"Angel Fernandez Pineda").

:- comment(module,
	"This library provides run-time support for @concept{object} creation 
         and manipulation. Objects are also called @concept{class instances},
         or simply @concept{instances}.

         Objects in Ciao are treated as normal modules. This is, an object
         is a run-time generated Prolog module, which may be identified by
         an unique term across the whole application. 

         This is a very simple example of how to create an instance,
         and how to make calls to it:

         @begin{verbatim}
            AnObj new myclass,
            AnObj:mymethod.
         @end{verbatim}

         In order to make any object accessible from code,
          an usage relationship
         must be established between the class (from which instances are
         derived) and the code itself. Refer to @pred{use_class/1} 
         predicate or @decl{use_class/1} declaration in order to do so.
         ").

%%------------------------------------------------------------------------

:- comment(hide,static_new/2).
:- comment(hide,'$class$'/1).
:- comment(hide,'id$fromclass'/2).
:- comment(hide,'class$call'/3).
:- comment(hide,'class$initial_state'/3).
:- comment(hide,'class$attr_template'/4).
:- comment(hide,'class$super'/2).
:- comment(hide,'class$default_cons'/1).
:- comment(hide,'class$constructor'/4).
:- comment(hide,'class$destructor'/3).
:- comment(hide,'class$implements'/2).
:- comment(hide,static_new/2).

%%------------------------------------------------------------------------
%% PROPERTIES
%%------------------------------------------------------------------------

:- prop constructor(Cons) # 
	"@var{Cons} is a term whose functor matches a class name.".

constructor(Cons) :-
	functor(Cons,_,_).

:- prop instance_id(ID) #
	"@var{ID} is an unique term which identifies an object.".

instance_id(ID) :-
	nonvar(ID),
	term(ID).

:- prop class_name(ClassName) #
	"@var{ClassName} is an atom denoting a class.".

class_name(Class) :-
	atom(Class).

:- prop interface_name(Interface) #
	"@var{Interface} is an unique atom which identifies a public 
         interface.".

interface_name(ID) :-
	atom(ID).

:- prop class_source(Source) #
	"@var{Source} is a valid path to a prolog file containing 
         a class declaration (without .pl extension).".

class_source(X) :-
	atom(X).

:- prop interface_source(Source) #
	"@var{Source} is a valid path to a prolog file containing 
         a class declaration or an interface declaration
         (without .pl extension).".

interface_source(X) :-
	atom(X).

:- prop method_spec(Spec) #
	"@var{Spec} is a method or attribute specification.".

:- comment(method_spec/1,
	"There is no difference between method or attribute specifications,
         and habitual predicate specifications.
         It is just a Functor/Arity term.
        ").

method_spec(F/A) :-
	atom(F),
	A >= 0.

:- prop virtual_method_spec(Spec) #
	"@var{Spec} is a method specification.".

virtual_method_spec(Spec) :-
	method_spec(Spec).

%%------------------------------------------------------------------------
%%
%% DYNAMIC CLASS LOADING
%%
%%------------------------------------------------------------------------

:- comment(use_class/1,
	"The behaviour of this predicate is identical to that provided
         by the declaration of the same name @decl{use_class/1}. It allows
         user programs to dynamically load classes. Whether the given
         source is not a class it will perform a @pred{use_module/1} 
         predicate call.").

:- pred use_class(ClassSource) : class_source(ClassSource) #
	"Dynamically loads the given @var{ClassSource}".

use_class(Class) :-
	use_module(Class).

%%------------------------------------------------------------------------
%%
%% INSTANCE CREATION 
%%
%%------------------------------------------------------------------------

:- meta_predicate(new(?,addmodule)).

:- comment(new/2,
	"Dynamic instance creation takes place by the ways of this predicate.

         It takes a free variable as first argument which will be
         instantiated to an internal object identifier. 

         Second argument must be instantiated to a @concept{class constructor}. 
         Class constructors are designed to perform an initialization on
         the new created instance. Notice that instance initialization may
         involve some kind of computation, not only @em{state initialization}.

         A class constructor is made by a functor, 
         which must match the intended
         class name, and any number of parameters. For example:

         @begin{verbatim}
            Obj new myclass(1500,'hello, world!!!')
         @end{verbatim}

         Those parameters depends (obviously) on the constructors defined 
         at the class source. If no constructors where defined, 
         no parameters are needed. This is called the 
         @concept{default constructor}. An example:

         @begin{verbatim}
            Obj new myclass
         @end{verbatim}
         
         The default constructor can not be called if there is any constructor
         available at the class source.

         Instantiation will raise an exception and fail 
         whenever any of this conditions occur:
         @begin{itemize}
          @item First argument is not a free variable.
          @item Second argument functor is a class, 
                but there is no usage relationship with it.
          @item Second argument functor is not a class.
          @item The given constructor is unknown.
          @item The given constructor fails 
                (notice that default constructor never fails). 
         @end{itemize}

         Objects may also be statically declared, 
         refer to @decl{instance_of/2} declaration.
         ").

:- pred new(InstanceVar,Constructor) : 
	(var(InstanceVar),constructor(Constructor)) => 
         instance_id(InstanceVar) #
	"Creates a new instance of the class specified by @var{Constructor} 
         returning its identifier in @var{InstanceVar}".

new(Var,_,_) :-
	nonvar(Var),
	!,
	throw(instantiation_error('1st argument must be free variable')),
	fail.

new(_,Constructor,_) :-
	var(Constructor),
	!,
	throw(instantiation_error('class not given')),
	fail.

new(ID,Constructor,FromModule) :-
	!,
	functor(Constructor,Class,_),
	create_unique_id(Instance),
	cons(Instance,Constructor,FromModule),
	functor(ID,Class,1),
	arg(1,ID,Instance),
	true.

%%------------------------------------------------------------------------
%% MACRO FOR CONSTRUCTOR CALLING
%%------------------------------------------------------------------------

% 1st case: given constructor was defined

cons(ID,Cons,From) :-
	'class$constructor'(Cons,ID,From,MetaCall),
	functor(Cons,Class,_),
	!,
	instantiation(ID,Class),
	( '$meta_call'(MetaCall) -> 
	  true
	  ;
	  (destroy(ID),!,fail)
	),
	true.

% 2nd case: no constructor given , use default constructor.

cons(ID,Class,_) :-
	atom(Class),
	'class$default_cons'(Class),
	!,
	instantiation(ID,Class).

% 3th case: given constructor was not defined.

cons(_,Cons,_) :-
	!,
	throw(instantiation_error(invalid_constructor(Cons))),
	fail.

%%------------------------------------------------------------------------
%% GENERATE UNIQUE IDS
%%------------------------------------------------------------------------
%% current_host/1 should also be used ...
%% pid is not necesary since walltime/1 is process-dependant.

%:- data next_id/2.

create_unique_id(ID) :-
	retract_fact('id$recycled'(ID)),
	!.

%% This one is too slow...
%create_unique_id(ID,Class) :-
%	retract_fact(next_id(Class,N)),
%	!,
%	M is N+1,
%	asserta_fact(next_id(Class,M)),
%	number_codes(N,Codes),
%	atom_codes(NID,Codes),
%	atom_concat(NID,Class,ID),
%	'$erase_atom'(NID).

%create_unique_id(ID,Class) :-
%	atom_concat('0',Class,ID),
%	asserta_fact(next_id(Class,1)).

%% This one may create not so unique ids...
%create_unique_id(ID,_) :-
%	goal_self(ThreadID),
%	number_codes(ThreadID,TIDCodes),
%	atom_codes(TID,TIDCodes),
%	get_pid(ProcessID),
%	number_codes(ProcessID,PIDCodes),
%	atom_codes(PID,PIDCodes),
%	statistics(walltime, [W,_]),
%	number_codes(W,WCodes),
%	atom_codes(WallTime,WCodes),
%	atom_concat(TID,PID,Aux),
%	atom_concat(Aux,WallTime,ID),
%	'$erase_atom'(Aux),
%	'$erase_atom'(WallTime),
%	true.


% This one is provisional...
%create_unique_id(ID,_) :-
%	create_unique_id_aux(AnID),
%	( 'id$fromclass'(AnID,_) -> create_unique_id(ID,_) ; ID = AnID),
%	true.
%create_unique_id_aux(ID) :-
%	eng_self(ThreadID),
%	number_codes(ThreadID,TIDCodes),
%	atom_codes(TID,TIDCodes),
%	statistics(walltime, [W,_]),
%	number_codes(W,WCodes),
%	atom_codes(WallTime,WCodes),
%	statistics(symbols,[S,_]),
%	number_codes(S,SCodes),
%	atom_codes(U,SCodes),
%	atom_concat(TID,U,Aux),
%	atom_concat(Aux,WallTime,ID),
%	'$erase_atom'(Aux),
%	'$erase_atom'(WallTime),
%	true.

create_unique_id(ID) :-
	new_atom(ID).
	
	

%%------------------------------------------------------------------------
%%
%% CLASS INSTANTIATION
%%
%% Internal use: parameters to instantiation/2 are assumed to be ckecked.
%%------------------------------------------------------------------------

instantiation(Obj,Class) :-
	asserta_fact('id$fromclass'(Obj,Class)),
	state_creation(Obj,Class),
	give_initial_state(Obj,Class),
	true.

%%------------------------------------------------------------------------
%% CREATE AN OBJECT'S STATE
%%
%% RealF will be an atom containing ":classname::attributename"
%%------------------------------------------------------------------------

state_creation(Obj,Class) :-
	'class$attr_template'(Class,RealF,A,Kind),
%	inform_user(['ATTR ',RealF/A]),
	atom_concat(Obj,RealF,NewF),
	'$define_predicate'(NewF/A,(interpreted)),
	functor(InstPred,NewF,A),
	'$set_property'(InstPred,Kind),
	fail.

state_creation(_,_).

%%------------------------------------------------------------------------
%% GENERATE INITIAL STATE 
%%------------------------------------------------------------------------

give_initial_state(Instance,Class) :-
	'class$initial_state'(Class,F,Args),
	atom_concat(Instance,F,QfdF),
	Fact =.. [QfdF|Args],
	term_to_meta(Fact,TMFact),
	assertz_fact(TMFact),
	fail.

give_initial_state(Instance,Class) :-
	'class$super'(Class,Super),
	!,
	give_initial_state(Instance,Super).

give_initial_state(_,_).

%%------------------------------------------------------------------------
%%
%% INSTANCE DESTRUCTION
%%
%%------------------------------------------------------------------------

:- comment(destroy/1,
	"As well as instances are created, they must be destroyed when
         no longer needed in order to release system resources.

         Unfortunately, current O'Ciao implementation does not support
         automatic instance destruction, so user must manually call
         @em{destroy/1} in order to do so.

         The programmer @bf{must ensure} that no other references to 
         the involved object are left in memory when destroy/1 is called.
         If not, unexpected results may be obtained.
        ").

:- pred destroy(Instance) : (instance_id(Instance)) #
	"Destroys the object identified by @var{Instance}.".

destroy(Object) :-
	nonvar(Object),
	arg(1,Object,ID),
	'id$fromclass'(ID,Class),
	!,
	call_destructor_if_any(ID,Class),
	state_destruction(ID,Class),
	retract_fact('id$fromclass'(ID,_)),
	asserta_fact('id$recycled'(ID)),
%	'$erase_atom'(ID),
	true.

%%------------------------------------------------------------------------

call_destructor_if_any(ID,Class) :-
	'class$destructor'(Class,ID,MetaCall),
	'$meta_call'(MetaCall),
	fail.

call_destructor_if_any(ID,Class) :-
	'class$super'(Class,Super),
	!,
	call_destructor_if_any(ID,Super).

call_destructor_if_any(_,_).

%%------------------------------------------------------------------------

state_destruction(Obj,Class) :-
	'class$attr_template'(Class,RealF,A,_),
	atom_concat(Obj,RealF,OldF),
	functor(Aux,OldF,A),
	abolish(Aux),
%	'$erase_atom'(OldF),
	fail.

state_destruction(_,_).

%%------------------------------------------------------------------------

:- redefining(abolish/1).

abolish(Head) :-
	'$current_clauses'(Head, Root), 
        '$current_instance'(Head, _, Root, Ptr, no_block),
	'$erase'(Ptr),
        '$unlock_predicate'(Root),
	fail.
abolish(Head) :-
	'$abolish'(Head).

%%------------------------------------------------------------------------
%%
%% INSTANCE AND CLASS RUN-TIME INFO
%%
%%------------------------------------------------------------------------

:- comment(derived_from/2,
	"Test whether an object identifier was derived directly from
         a class, by the usage of @pred{new/2} or a static instance
         declaration (@decl{instance_of/2}).
        ").

:- pred derived_from(Instance,Class): 
	(instance_id(Instance),class_name(Class)) #
	"Test derivation of @var{Instance} from @var{Class}".

:- pred derived_from(Instance,Class): 
	(instance_id(Instance),var(Class)) => class_name(Class) #
	"Retrieves the @var{Class} responsable of the derivation 
         of @var{Instance}.".

derived_from(Object,Class) :-
	nonvar(Object),
	functor(Object,Class,1),
	arg(1,Object,ID),
	'id$fromclass'(ID,Class).

%%------------------------------------------------------------------------

:- comment(instance_of/2,
	"This predicate is used to perform dynamic type checking.
         You may check whether a particular instance belongs to a 
         particular class or related descendants.

         instance_of/2 is used to perform static @concept{semantic analisys}
         over object oriented code constructions.

         By the use of instance_of/2 you may help to perform such analisys.
        ").

:- pred instance_of(Instance,Class) :
	(instance_id(Instance),class_name(Class)) #
	"Test whether @var{Instance} was derived from any descendant of
         @var{Class}, or that class itself".

:- pred instance_of(Instance,Class) :
	(instance_id(Instance),var(Class)) => class_name(Class) #
	"Retrieves, on backtracking, the inheritance line of @var{Instance}
         commencing on the creation class (that specified on call to 
         @pred{new/2}) and continuing on the rest of ascendant classes,
         if any.".

instance_of(ID,ClassOrAscendant) :-
	derived_from(ID,Class),
	!,
	instance_of_aux(Class,ClassOrAscendant).

instance_of_aux(Class,Class).

instance_of_aux(Class,Ascendant) :-
	'class$super'(Class,Super),
	!,
	instance_of_aux(Super,Ascendant).

%%------------------------------------------------------------------------

:- comment(interface/2,
	"This predicate is used to ensure a given interface to be implemented
         by a given instance.
        ").

:- pred interface(Instance,Interface) :
	( instance_id(Instance),interface_name(Interface) ) #
        "Check whether @var{Instance} implements the given 
         @var{Interface}.".

:- pred interface(Instance,Interfaces) :
	(instance_id(Instance),var(Interfaces)) => interface_name(Interfaces) #
        "Retrieves on backtracking all the implemented @var{Interfaces}
         of @var{Instance}.".

interface(ID,Itf) :-
	derived_from(ID,Class),
	!,
	'class$implements'(Class,Itf).

%%------------------------------------------------------------------------
%%
%% INSTANCE ID CONVERSION MACRO
%%
%%------------------------------------------------------------------------

:- comment(instance_codes/2,
	"Retrieves a character string representation from an object
         identifier and vice-versa.
        ").

:- pred instance_codes(Instance,String) : 
	( instance_id(Instance),var(String) ) => string(String) #
        "Retrieves a @var{String} representation of given @var{Instance}.".

:- pred instance_codes(Instance,String) : 
	( var(Instance),string(String) ) => instance_id(Instance) #
        "Reproduces an @var{Instance} from its @var{String} representation.
         Such an instance must be alive across the application:
         this predicate will fail whether the involved instance 
         has been destroyed.
        ".

instance_codes(Object,Codes) :-
	nonvar(Object),
	!,
	arg(1,Object,ID),
	atom_codes(ID,Codes).

instance_codes(Object,Codes) :-
	atom_codes(ID,Codes),
	'id$fromclass'(ID,Class),
	functor(Object,Class,1),
	arg(1,Object,ID).

%%------------------------------------------------------------------------
%%
%% SUPPORT FOR STATIC OBJECT DECLARATION
%%
%%------------------------------------------------------------------------

:- meta_predicate static_new(?,addmodule).

static_new(ID,Cons,_) :-
	functor(Cons,Class,_),
	'id$fromclass'(ID,OtherClass),
        %display('NORRRL'),
	!,
	( Class = OtherClass -> true
	;
	  (throw(instance_already_in_use(ID,OtherClass)),fail)
	).

static_new(ID,Cons,From) :-
	functor(Cons,Class,_),
	instantiation(ID,Class),
	( 'class$constructor'(Cons,ID,From,MetaCall) -> 
          static_new_aux(MetaCall)
	;
	  true
	),
	true.

static_new_aux(MetaCall) :-
	'$meta_call'(MetaCall),
	!.

static_new_aux(_).

%%------------------------------------------------------------------------

%%------------------------------------------------------------------------

:- comment(bug,
	"Not really a bug: when loading code which declares 
         static instances from the toplevel shell, 
         predicate @pred{use_module/1}) will not work
         properly: those instances may be not
         correctly created, and predicates will fail whenever they are not
         supposed to do. This may be avoided by reloading again 
         the involved module, but make sure it is modified and saved to
         disk before doing so.
        ").

%%------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).


:- comment(version(1*7+51,2001/01/25,21:33*00+'CET'), "Changed obsolete
   library(dynmods) to library(compiler).  (Daniel Cabeza Gras)").

:- comment(version(1*5+4,1999/11/29,19:11*21+'MET'), "use_class/1 predicate 
   has been moved to this module (Angel Fernandez Pineda).").

:- comment(version(1*3+96,1999/11/10,17:38*35+'MET'), "An imported library
   changed its name (dummy->prolog_sys) (Angel Fernandez Pineda)").

:- comment(version(1*3+75,1999/10/13,16:03*21+'MEST'), "Removed internals on
   usage relationships. Any source may create instances from any class.  (Angel
   Fernandez Pineda)").

:- comment(version(1*3+73,1999/10/06,15:27*28+'MEST'), " Unique instance
   identifiers were not so unique. Now, fixed.  (Angel Fernandez Pineda)").

:- comment(version(1*3+72,1999/10/06,15:17*15+'MEST'), "Added
   instance_codes/2 macro being in benefit of Mr. Programmer.  (Angel
   Fernandez Pineda)").

:- comment(version(1*3+68,1999/10/04,15:54*19+'MEST'), "System performance
   has been improved. Documentation has been revised, too.  (Angel
   Fernandez Pineda)").

:- comment(version(1*3+59,1999/09/24,14:48*00+'MEST'), "New implementation
   for statically declared objects. Run-time expansor is no longer needed
   because of static objects.  (Angel Fernandez Pineda)").

:- comment(version(1*3+58,1999/09/23,12:56*31+'MEST'), "now, meta-arguments
   may be declared for constructors.  (Angel Fernandez Pineda)").

:- comment(version(1*3+57,1999/09/21,14:24*41+'MEST'), "Enhanced 
   implementation technology due to new features on Ciao prolog
   compiler. Removed published/2, no longer supported. 
   Meta-arguments are now working, really !!! I promise !!!. 
   (Angel Fernandez Pineda)").

:- comment(version(1*3+31,1999/07/12,19:13*05+'MEST'), "Added published/2
   predicate.  (Angel Fernandez Pineda)").

:- comment(version(1*3+28,1999/07/12,15:41*55+'MEST'), "Added interface/2
   predicate.  (Angel Fernandez Pineda)").

:- comment(version(1*3+22,1999/07/07,11:25*38+'MEST'), "Improved
   documentation.  (Angel Fernandez Pineda)").

:- comment(version(1*3+18,1999/07/05,19:34*47+'MEST'), "Added support for
   static object declaration (Angel Fernandez Pineda)").

:- comment(version(1*3+16,1999/07/05,14:55*19+'MEST'), "Method creation has
   been boosted (Angel Fernandez Pineda)").

:- comment(version(1*3+7,1999/06/28,16:45*19+'MEST'), "Instance destruction
   has been revised (Angel Fernandez Pineda)").

:- comment(version(1*3+3,1999/06/16,18:06*36+'MEST'), "Improved unique
   instance-ID generation (Angel Fernandez Pineda)").

:- comment(version(1*3+1,1999/06/15,15:39*53+'MEST'), "new/2 implementation
   optimized.  (Angel Fernandez Pineda)").

:- comment(version(0*9+61,1999/04/26,15:28*08+'MEST'), "Now, default
   constructor can not be called whenever any other constructor is
   available. (Angel Fernandez Pineda)").

:- comment(version(0*9+60,1999/04/26,15:27*45+'MEST'), "Added documentation
   on error messages (Angel Fernandez Pineda)").

:- comment(version(0*9+47,1999/04/13,15:34*20+'MEST'), "Simplified
   documentation.  (Angel Fernandez Pineda)").

:- comment(version(0*9+46,1999/04/13,15:33*14+'MEST'), "Improved new/2
   performance. Meta-arguments now working only for compiled code 
   (not interpreted). (Angel Fernandez Pineda)").

:- comment(version(0*9+45,1999/04/13,15:32*41+'MEST'), "Full O'Ciao
   implementation has changed. This is a big one, people !!!  (Angel
   Fernandez Pineda)").

:- comment(version(0*9+24,1999/03/25,17:46*45+'MET'), "Updated some
   documentation.  (Angel Fernandez Pineda)").

:- comment(version(0*9+20,1999/03/25,16:53*51+'MET'), "Destructors are
   working (Angel Fernandez Pineda)").

:- comment(version(0*9+13,1999/03/22,17:58*36+'MET'), "Virtual methods now
   implemented (Angel Fernandez Pineda)").

:- comment(version(0*9+9,1999/03/18,19:20*30+'MET'), "Fixed big bug on
   method re-inheritance/re-exportation (Angel Fernandez Pineda)").

:- comment(version(0*8+41,1999/03/02,19:35*33+'MET'), "Fixed minor bug on
   super identification (Angel Fernandez Pineda)").

:- comment(version(0*8+36,1999/02/25,18:38*05+'MET'), "Incorporated
   documentation for O'Ciao.  (Angel Fernandez Pineda)").

%%------------------------------------------------------------------------
