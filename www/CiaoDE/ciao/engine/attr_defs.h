BOOL bu1_detach_attribute(Argdecl, register TAGGED x);
BOOL bu2_attach_attribute(Argdecl, register TAGGED var, register TAGGED constr);
BOOL bu2_update_attribute(Argdecl, register TAGGED x, register TAGGED constr);
TAGGED fu1_get_attribute(Argdecl, register TAGGED x);
TAGGED fu1_type(Argdecl, register TAGGED t0);
void collect_one_pending_unification(Argdecl);
void collect_pending_unifications(Argdecl,int wake_count);
