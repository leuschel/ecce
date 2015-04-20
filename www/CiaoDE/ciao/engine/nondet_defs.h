/*
  static unsigned int predicate_property_bits(register struct definition *d);
  static BOOL current_stream_data(Argdecl, struct stream_node *streamptr)
 */

typedef enum {X5, X2} WhichChain;

void pop_frame(Argdecl);
void push_frame(Argdecl, int arity);
void pop_choicept(Argdecl);
void push_choicept(Argdecl, struct try_node *alt);
BOOL nd_atom_concat(Argdecl);
BOOL current_atom(Argdecl);
BOOL nd_current_atom(Argdecl);
BOOL current_clauses(Argdecl);
BOOL current_stream(Argdecl);
BOOL nd_current_stream(Argdecl);
BOOL prolog_repeat(Argdecl);
BOOL nd_repeat(Argdecl);
BOOL current_predicate(Argdecl);
BOOL nd_current_predicate(Argdecl);
BOOL predicate_property(Argdecl);
BOOL nd_predicate_property(Argdecl);
struct instance *current_instance(Argdecl);
struct instance *current_instance_nolog(Argdecl);
BOOL first_instance(Argdecl);
BOOL close_predicate(Argdecl);
BOOL open_predicate(Argdecl);
BOOL next_instance(Argdecl, struct instance **ipp);
BOOL next_instance_conc(Argdecl, struct instance **ipp);
void move_queue(InstanceHandle **srcq, 
                InstanceHandle **destq,
                struct instance *destinst);
void jump_to_next_instance(struct instance *x2_p_insp,
                           struct instance *x5_p_insp,
                           struct instance **ipp,
                           struct instance **x2,
                           struct instance **x5);
void remove_link_chains(struct node **topdynamic,
                        struct node  *chpttoclear);

