/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* GARBAGE COLLECTION SUPPORT MACROS */

#define gc_Reverse(curr,next) \
{  REGISTER TAGGED *temp; \
   temp= TagToPointer(*next); \
   *next= gc_PutValue((TAGGED)curr,*next); \
   curr= next; \
   next= temp; }

#define gc_Undo(curr,next)  gc_Reverse(next,curr)

#define gc_Advance(curr,next) \
{  REGISTER TAGGED *temp; \
   temp= TagToPointer(*curr); \
   *curr= gc_PutValue((TAGGED)next,*curr); \
   HeapDecr(curr); \
   next= TagToPointer(*curr); \
   *curr= gc_PutValue((TAGGED)temp,*curr); }

#define gc_TrailStart		TagToPointer(w->segment_node->trail_top)
#define gc_HeapStart		(w->segment_node->global_top)
#define gc_StackStart		(w->segment_node->local_top)
#define gc_ChoiceStart		(w->segment_node)



#define gc_ReverseChoice(cp,prevcp,alt) \
{ \
  struct try_node *m_alt = alt; \
  cp = prevcp; \
  alt = cp->next_alt; \
  cp->next_alt = m_alt; \
  prevcp = ChoiceCharOffset(cp,-alt->node_offset); \
}

#define gc_UndoChoice(cp,prevcp,alt) \
{ \
  struct try_node *m_alt = alt; \
  prevcp = cp; \
  alt = cp->next_alt; \
  cp->next_alt = m_alt; \
  cp = ChoiceCharOffset(cp,alt->node_offset); \
}
