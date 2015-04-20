
/* Quintus Prolog Manual page


L-3-83: hash_term/2

Synopsis: 
       hash_term(+Term, -HashValue) 
       Provides an efficient way to calculate an integer hash value for the
       ground term Term. 
Arguments: 
       Term 
              <term> 
       HashValue 
              <term> is an integer or variable 
Description: 
              If the first argument passed to hash_term/2 is ground, an integer hash
              value corresponding to that term is calculated and returned in the second
              argument. If the first argument is not ground, a new variable is returned in
              the second argument. 
              For example: 

                                   | ?- hash_term(foo(name,2,module), H).

                                  H = 1391

                                  | ?- hash_term(foo(X), H).
                                  X = _4734,
                                  H = _4755

                                  | ?-

                          

Tips: 
       hash_term/2 is provided primarily as a tool for the construction of sophisticated
       Prolog clause access schemes. Its intended use is to generate hash values for
       ground terms that will be used with first argument clause indexing, yielding
       compact and efficient multi-argument or deep argument indexing. 
       hash_term/2 is most easily used when a known pattern of access to a predicate is
       desired and both arguments of the call and arguments of the predicate are known
       to be ground. In the following simple but typical example, hash_term/2 calls are
       used together with Prolog's database manipulation predicates (assert/1 and
       clause/2) to calculate and add an additional argument to the clauses actually
       stored in the Prolog database: 



                 add_pred_info(Name, Arity, Module, Info) :-
                         hash_term([Name,Arity,Module], Hash),
                         assert(info(Hash,Name,Arity,Module,Info)).

                 get_pred_info(Name, Arity, Module, Info) :-
                         hash_term([Name,Arity,Module], Hash),
                         clause(info(Hash,Name,Arity,Module,Info), _).

                 

       This example assumes that the name, arity and module to be stored in the Prolog
       database are ground when add_pred_info/4 is called, and that they are also ground
       when get_pred_info/4 is called. The predicate that is actually asserted, info/5, has
       an additional argument calculated by hash_term/2; info/5 would not normally be
       called directly. A predicate using hash_term/2 to delete the stored information
       would also be straightforward. 
       If the first argument passed to hash_term/2 is not ground, hash_term/2 returns a
       variable. Thus, if add_pred_info/4 is called with the name, arity or module not
       ground, the info/5 information will be asserted with a variable as its first
       argument, so it will not be indexed. If get_pred_info/4 is called with the name,
       arity or module not ground, info/5 will simply be searched sequentially. Prolog's
       normal semantics will be retained, although access will be considerably less
       efficient. 
       It is possible to use hash_term/2 in more complex indexing schemes as well by
       checking instantiation when adding, accessing, and deleting clauses; however, it
       is up to the user to ensure appropriate instantiation patterns in calls. The tradeoff
       between run-time argument checking and reduced indexing effectiveness depends
       on the degree of discrimination otherwise afforded by normal first argument
       indexing. The efficiency gained by fast multi-argument indexing can often more
       than make up for such additional run-time costs. 
       It is also possible to use such indexing techniques on compiled predicates using
       term expansion. Note that calculated hash values are not dependent on transitory
       information like atom numbers or internal pointers. Hash values are consistent
       across saving and restoring or multiple invocations of an application. 
       Calculation of hash values is very fast, and indices constructed using the
       techniques sketched above are also very compact, as the only additional cost is
       for storing the additional (hash value) argument. When a solution to a complex
       indexing problem can be constructed using hash_term/2 it will probably be
       preferable to solutions using other techniques. 
*/

