
/*
  static void shuntVariables(Argdecl);
  static void markTrail(Argdecl);
  static void markFrames(struct frame *frame, INSN *l);
  static void markChoicepoints(Argdecl);
  static void markVariable(TAGGED *start);
  static void updateRelocationChain(TAGGED *curr, TAGGED *dest);
  static void sweepTrail(Argdecl);
  static void sweepFrames(struct frame *frame, INSN *l);
  static void sweepChoicepoints(Argdecl);
  static void compressHeap(Argdecl);
 */

BOOL gc_usage(Argdecl);
BOOL gc_mode(Argdecl);
BOOL gc_trace(Argdecl);
BOOL gc_margin(Argdecl);
void compressTrail(Argdecl, BOOL from_gc);
void GarbageCollect(Argdecl);
