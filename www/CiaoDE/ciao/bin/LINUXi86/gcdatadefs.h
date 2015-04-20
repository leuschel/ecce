/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/*    GARBAGE COLLECTION DATA & MACRO DEFINITIONS */

#define GC_MARKMASK  2
#define GC_FIRSTMASK 1

#define gc_IsMarked(x)  ((x)&GC_MARKMASK)
#define gc_IsFirst(x)   ((x)&GC_FIRSTMASK)
#define gc_IsForM(x)   ((x)&(GC_FIRSTMASK|GC_MARKMASK))
#define gc_MarkM(x)  ((x)|= GC_MARKMASK)
#define gc_MarkF(x)  ((x)|= GC_FIRSTMASK)
#define gc_UnmarkM(x)  ((x)&=(~GC_MARKMASK))
#define gc_UnmarkF(x)  ((x)&=(~GC_FIRSTMASK))
#define gc_PutValue(p,x) Deposit(p,POINTERMASK,x)
#define gc_PutValueFirst(p,x) Deposit(p,POINTERMASK|GC_FIRSTMASK,x)

#define PreHeapRead(X)		(*++(X))
#define HeapPop(X)		(*--(X))

