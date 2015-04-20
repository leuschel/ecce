:- module(_,[],[assertions]).
:- comment(title,"Internals").
:- comment(author,"J. Navas").


:- comment(module,"
@section{Introduction}
@p The scope of this project is to provide @em{transactional semantics}
across multiple Ciao threads accessing the same database. Also, this
implementation will have several optimizations that will do it unique with
respect to other implementations. 

@p In the following sections, we will comment the more important aspects in
order to implement the transactional semantics. Basically, these aspects
are: concurrency, logging and recovery. Additionally, we will list some
possible optimizations by using @em{program analysis} and @em{program
transformation}.

@section{Concurrency}

@p It expands transactions such that locks are obtained according to the rules
of @em{STRICT TWO-PHASE LOCKING}. Strict 2PL mandates that a transaction
does not release any exclusive (write) lock that it owns until the
transaction either aborts or commits.  Such a scheme guarantees a
@em{serializable} schedule of operations.  This package implements
shared/exclusive read/write locks as a low-level locking mechanism (see
@cite{ElmasriNavathe0}).


@p In the scheme shared/exclusive or read/write locks there are three locking
operations: @em{read_lock(X,T)},@em{write_lock(X,T)} and
@em{unlock(X)}. Note it is allowed both upgrading and downgrading in
order to make the concurrency more efficient.

@begin{verbatim}
read_lock(X,T)
  B: if LOCK(X)=\"unlocked\"
        LOCK(X)=\"read_locked\"
        no_of_reads(X) = 1
     else if LOCK(X)=\"read_locked\"
             no_of_reads(X) ++
          else if LOCK(X,T)=\"write_locked\"
                  LOCK(X)=\"read_locked\"
                  no_of_reads(X) = 1   /*downgrading*/
             else
                  wait(until LOCK(X)=\"unlocked\")
                  go to B
@end{verbatim}

@begin{verbatim}
write_lock(X,T)
  B: if LOCK(X)=\"unlocked\"
        LOCK(X)=\"write_locked\"
     else if LOCK(X,T)=\"read_locked\" and no_of_reads(X) = 1
             LOCK(X)=\"write_locked\"    /*upgrading*/
        else
            wait(until LOCK(X)=\"unlocked\")
            go to B
@end{verbatim}

@begin{verbatim}
unlock(X)
  B: if LOCK(X)=\"write_locked\"
        LOCK(X)=\"unlocked\"
        \"wake up one of the waiting transactions\"
     else if LOCK(X)=\"read_locked\"
             no_of_reads(X) --
             if no_of_reads(X) == 0
                LOCK(X)= \"unlocked\"
                \"wake up one of the waiting transactions\"       
@end{verbatim}        

@p The use of locking implies two problems: @em{deadlock} and
@em{starvation}.  Therefore, we must apply some techniques which solve
them.

@subsection{Deadlock}

@begin{itemize}

@item @em{Detection}. In this case, a simple way to detect a state of
@em{deadlock} is for the system to construct and maintain a wait-for
graph. That is, one node is created in the wait-for graph for each
transaction that is currently execution. Whenever a transaction Ti is
waiting to lock an item X that is currently locked by a transaction Tj, a
directed edge (Ti -> Tj) is created in the wait-for graph. Then Tj releases
the lock(s) on the items that Ti was waiting for, the directed edge is
dropped from the wait-for graph. We have a state of deadlock if and only if
the wait-for graph has a cycle.

@item @em{Solution}. In principle, the transaction which provokes the
deadlock is chosen to be aborted. We propose an optimization based on
computing the cost for each transaction belonging to the conflict set and
selection of the shortest runtime transaction.

@subsection{Starvation}

One solution for @em{starvation} is to have a fair waiting scheme, such
as using a first-come-first-serve queue; transactions are enabled to lock
an item in the order in which they originally requested the lock.  Another
possibility is the priority queue. In our case, we will use predicates
defined in the @em{Low-level concurrency/multithreading primitives}
library to implement the concurrency based on locks among transactions.
O.S. will select which thread or proces to be woken.

@section{Logging}

@p To be able to recover from failures that affect transactions, the system
maintains a log to keep track of all transactions operations that affect
the values of database items. This information may be needed to permit
recovery from failures. We now list the types of entries (LOG RECORDS):

@begin{itemize}  
  
@item @em{[start\_transaction,T]}: Indicates that transaction T has
  started execution.
  
@item @em{[write\_item,T,X,old\_value,new\_value]}: Indicates that
  transaction T has changed the value of database item X from old\_value to
  new\_value.
  
@item @em{[read\_item,T,X]}: Indicates that transaction T has read the
  value of database item X.
  
@item @em{[commit,T]}: Indicates that transaction T has completed
  successfully, and affirms that its effect can be committed to the
  database.
  
@item @em{[abort,T]}: Indicates that transaction T has been aborted.
  
@item @emph{[checkpointing]}: Indicates all transactions that have their
  [commit,T] entries in the log before a [checkpointing] entry do not need
  to have their WRITE operations redone in case of system crash.

@end{itemize}


@p Taking a CHECKPOINTING consists of the following actions:

@begin{itemize}
@item Suspend execution of transactions temporarily.
@item Force-write all main memory buffers that have been modified to disk.  
@item Write a [checkpointing] record to the log, and force-write the log to
  disk.
@item Resume executing transactions.
@end{itemize}

@p Fuzzy checkpointing allows to perform the Step 4 without having to wait for
step 2.

@p Protocols for recovery that avoid cascading rollbacks do not require READ
operations. Also, some recovery protocols do not include the field
New\_value in the write operation.

The entry [Ti,x,v] can be removed from the log if
@begin{itemize}
@item Ti has aborted or  
@item Ti has committed but some other committed transaction wrote into x
  after Ti did. (undo rule)
@end{itemize}

@p Notice that even if v is the last committed value of x and is stored in the
stable database copy of x, [Ti,x,v] cannot be deleted from the log.

@section{Recovery}

@p If a transaction T is rolled back, any transaction S that has read the
value of some data item X written by T must also be rolled. Similarly, once
S is rolled back, any transaction R that has read the value of some data
item Y written by S must also be rolled back; and so on. This phenomenon is
called @em{cascading rollback}. In our case, we will not allow the use
cascading rollback, since when a transaction modifies a data item no
different transaction can read it.

@begin{itemize}
  
@item If we use @em{dynamic (data) predicates} is not necessary the use of
  recovery.
  
@item If we use @em{persistent predicates (LOCAL persdb)}, we will use
  the algorithm UNDO/NO-REDO. The use of UNDO is clear since a transaction
  T can write in the database before it commits. Hence, in case of failure
  these operations has to be undo. The redo would be necessary if once a
  transaction T commits there exist operations which have not been written
  into the database. Since we will use the persdb library, this ensures
  that once that the transaction T commits all data item are written into
  the database.
  
@item If we use @em{REMOTE persdb} is clear the necessity of UNDO but the
  use of either NO-REDO or REDO depends on when data item is written into the
  database.

@end{itemize}


@subsection{Recovery based on Immediate Update with Concurrent Execution}

@begin{verbatim}
PROCEDURE RIU_M

1. Use two lists of transactions maintained by the system: the committed
   transactions since the last checkpoint and the active transactions.

2. Undo all the write_item operations of the active (uncommitted)
   transactions, using the UNDO procedure. The operations should be undone
   in the reverse of the order in which they were written into the log.
@end{verbatim}

@begin{verbatim}
PROCEDURE UNDO(WRITE_OP)

Undoing a write_item operation WRITE_OP consists of examining its log entry
[write_item,T,X,old_value,new_value] and setting the value of item X in the
database to old_value which is the before image (BFIM). Undoing a number of
write_item operations from one or more transactions from the log must
proceed in the reverse order from the order in which the operations were
written in the log.

@end{verbatim}

@section{Future Work: Optimizations}

@p All the possible optimizations are based on Static Analysis. The idea is
to infer at compile-time information about the state of a
program. Note that these possible improvements are not demonstrated currently
and so, they are only different possibilities on which we would have to
work with much more details. These optimizations can be divided into two
types:

@begin{itemize}
  
@item Improve some of the techniques used in the control of concurrency or
  recovery. 
  
@item Avoid the abortion of transactions. There are several possible
  reasons for a transaction to fail in the middle of an execution:

  @begin{enumerate}    
  @item A computer failure (system crash). A hardware, software, or network
    error occurs in the computer system during transaction execution.   
  @item A transaction or system error. Some operation in the transaction
    may cause it to fail, such as integer overflow or division by zero.
    Transaction failure may also occur because of erroneous parameter
    values or because of a logical programming error. In addition, the user
    may interrupt the transaction during its execution.    
  @item Local errors or exception conditions detected by the transaction.
    For example, data for the transaction may not be found. Notice that an
    exception condition, such as insufficient account balance in a banking
    database, may cause a transaction, such as a fund withdrawal, to be
    canceled. This exception should be programmed in the transaction
    itself, and hence would not be considered a failure.    
  @item Concurrency control enforcement. The concurrency control method may
    decide to abort the transaction, to be restarted later, because it
    violates serializability or because several transactions are in a state
    of deadlock.
  @item Disk failure or physical problems and catastrophes.  
  @end{enumerate} 
@end{itemize}
  

  We will not worry about the first and fith point. However, we can get
  some optimizations with the rest of points:

  @begin{itemize}
    
  @item @em{Transaction or system error}. There can be many different
    sort of errors. Incorrect types, exceptions (i.e. division by zero) or
    more general errors such as predicates that fail, predicates that do
    not terminate, etc.  Some of these errors can be avoided by the CiaoPP
    system providing @em{type analysis}, @em{non-failure analysis}, or
    @em{termination analysis}, respectively.
    
  @item @em{Concurrency control enforcement}.  When the code of a
  transaction is known at compile-time, we can carry out a sharing
  analysis in order to find out whether two transactions are independent or
  not. If two transactions are independent, it is not necessary to carry
  out the control of concurrency (locking, deadlock, etc)

  @item @em{Concurrency control enforcement}. When there is a deadlock, we
  have to choose a ''victim''. This victim could be the transaction whose
  running time is less. Ideally, it should be able to predict the future
  behaviour of active transactions, e.g. based on the transactions type
  (deposits are short, audits are long,..)

  @item @em{Others}. Choose the best time for carrying out
  @em{checkpointing} based on an analysis of cost of each transaction. If
  the next transaction has a long running time, maybe it is better to
  execute the checkpointing operation.

  @end{itemize}

").
