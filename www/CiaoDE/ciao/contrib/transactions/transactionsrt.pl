%%transactionsrt.pl
%%(c)2003 Nick D. Pattengale
%% nickp@cs.unm.edu

:- module(transactionsrt, [], _).

:- use_package(assertions).

:- comment(title,"Transactional Semantics").

:- comment(copyright, "@copyright 2003 Nick D. Pattengale").

:- comment(author,"Nick D. Pattengale - @email{nickp@@cs.unm.edu}").

:- comment(summary,"This package was authored for a class project.
The class was CS550 - Programming Languages and Systems at
The University of New Mexico (Fall 2003).  The course
focused on a next generation logic programming framework
called Ciao.  Anticipating my involvement on an unrelated 
project to add transactional semantics to an object-oriented
database (objectbase) in Common Lisp, I chose a transactions
oriented term project for CS550.  This is the result of
that effort.

I spent the initial time on this project reading up on
database transactions.  My main source of information was
the database text that I used while an undergraduate
(TODO: cite Elmasri/Navathe).  The challenge in this phase
of the project (planning/design) was mapping the concepts
from the world of relational databases to logic programming.
Here are some of my conclusions.

Prolog traditionally implements database-like functionality
by using the builtin dynamic database.  The dynamic database
is more powerful than a traditional relational database.
This is because the dynamic database stores predicates,
which are an abstraction for relations.  Predicates provide
a finite representation for a (potentially) infinite set
of relations.

Dynamic predicates are good, however such an boost in
expressive power is not typically needed for doing 
database-like storage/retrieval from typical programs.
The implementors of Ciao offer a nice tradeoff called
data facts.  Data facts ensure a finite set of relations,
whereas retrieval of data facts is still declarative (i.e.
backtracking is still for free).

Thus the use of data facts enables relational database
functionality in all its glory.  Moreover, it is useful
to consider/realize that a data predicate is the conceptual
equivalent of a table in a relational database.

Another important
distinction between so-called \"traditional\" database
systems (MySQL, Oracle, etc...) and Ciao data_facts is that
the former are typically run as a system \"service\" or
\"server\"  whereas database access in Ciao using
data_facts is \"built-in,\" or \"transparent.\"  I'd 
argue that the Ciao approach is much more elegant
and convenient.  However, the approach does beg the
issue how to implement an authoritative datastore 
across multiple Ciao program instances.

That said, the scope of this project is to provide
transactional semantics across multiple Ciao threads
accessing the same dynamic database.  Because of this
scoping, the transaction management is implemented
@index{cooperatively}.  This means that all of the
transaction enabling calls are expanded inline into
user code so that the proper cooperation occurs
without the introduction of extra threads.  This
approach means that I was limited in the techniques
available for handling certain aspects (e.g. deadlock
detection using timeouts) that can only be implemented
preemptively from an external thread.  For a more detailed
discussion of how this work may be extended to a
\"service\"-like implementation see the \"Bugs and
Planned Improvements\" section in this module.

@p

--

@p

Earlier I said this project is about transactions, so 
what is a transaction?  The need for transactions arise in
multi-user/concurrent systems.  When there is more than
one subject accessing the database there is potential
for confusion, and even worse, inacurracy. The notion
of transaction addresses these potential problems.  A
transaction is a grouping of operations that is meant
to be executed under ACID (Atomicity, Consistency,
Isolation, and Durability - see Elmasri/Navathe) constraints.

In SQL, a transaction is specified something like

@begin{verbatim}
BEGIN TRANSACTION

...

COMMIT
@end{verbatim}

Such a declaration results in 1) the database system
enforces @index{atomicity} - if anything goes wrong before
the commit point the transaction will be aborted and
all side effects will be rolled back and 2) the
database system enforces @index{isolation} - all read/write
on the database for the duration of the transaction will 
appear to have occurred sequentially.

This module enables one to denote a predicate as
transactional.  This is done using the transactional
declaration.  For example

@begin{verbatim}

:- transactional(deposit/2)

@end{verbatim}

will expand the definition of @code{deposit/2} to 1)
be executed in the context of a transaction id (so
that the system can execute @index{atomic} commit/abort properly)
and 2) obtain the correct read/write locks upon acess
to data facts in order to enforce @index{isolation}.").

:- comment(module,"@section{Atomicity}

In order for an executing transaction to be atomic, when it is done executing
either 1) it is commited indicating that the entire transaction ran or 2)
it is rolled-back indicating that somewhere during the execution something went
wrong and database modifications were undone.

This package implements atomicity by keeping a \"journal.\"  Often referred to
as the DBMS Journal, this record contains a transcript of all modifications to 
the database.  Since this module implements transactions cooperatively, calls
to writers (assert_fact, retract_fact, etc...) are expanded to not only perform
the intended operation but also assert a record of the operation in a
data_fact called dbmsj.

With the existence of the dbmsj, the rollback procedure needed when a 
transaction aborts is as simple as backtracking over all operations in
the transaction and executing inverse operations.  Note that, in light
of this approach, it is non-trivial (well, impossible without forcing
the low-level to cough up a transcript of all the facts that were retracted)
to rollback a retractall_fact operation.  Thus retractall_fact operations
are not supported here.
	
@p
	
@section{Isolation}

We expand transactions such that locks are obtained according
to the rules of strict two-phase locking.  Strict two-phase locking
mandates that a transaction does not release any lock that it
owns until the transaction either aborts or commits.  Such
a scheme guarantees a serializable schedule of operations.  This
package implements shared/exclusive read/write locks as a
low-level locking mechanism (Elmasri/Navathe).").


:- comment(usage, "@decl{:- use_package(transactions)} or by including
	@lib{transactions} on a modules' package list.").

%:- comment(bug, "Preemptive Service TODO: elaborate").


%%actual expansion done in transactionstr.pl
:- comment(doinclude,transactional/1).
:- decl transactional(PredSpec) : predname
       # "Declares the predicate @var{PredSpec} as @decl{transactional}.
          Any use of predicates that were declared @decl{data}
	  from within a @decl{transactional} body will 
          appropriately and invisibly acquire locks before proceeding.  
          If a @decl{transactional} predicate is required to abort, 
	  it will be restarted after a one second refractory period.".

:- comment(doinclude,asserta_fact/1).
:- pred asserta_fact(Fact) : callable
	#"Like normal @code{asserta_fact}, but obtains a write lock first.".

:- comment(doinclude,assertz_fact/1).
:- pred assertz_fact(Fact) : callable
	#"Like normal @code{assertz_fact}, but obtains a write lock first.".

:- comment(doinclude,retract_fact/1).
:- pred retract_fact(Fact) : callable
	#"Like normal @code{retract_fact}, but obtains a write lock first.".

:- comment(doinclude,data/1).
:- decl data(PredSpec) : predname
	#"Like normal @code{data(F/A)}, but expands use from within 
        transactions to do proper locking.".