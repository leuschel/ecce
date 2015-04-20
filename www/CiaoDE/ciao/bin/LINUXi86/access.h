/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP */

/* This file contains access macros for the various WAM areas.
   The point is to hide the memory model (SRI/vanilla) as well as
   implementation details such as growth directions of stacks.
*/


#define Offset(X,O)	((TAGGED *)(X) + (O))
#define CharOffset(X,O)	((TAGGED *)((char *)(X) + (O)))

/* THE HEAP */

	/* assuming heap growth in positive direction */

#define OnHeap(t)       (HeapYounger(Heap_End,t) && !HeapYounger(Heap_Start,t))
#define OffHeaptop(t,H)          (!HeapYounger(H,t))
#define HeapYounger(X,Y)	((TAGGED *)(X)>(TAGGED *)(Y))
#define HeapDifference(X,Y)	((TAGGED *)(Y) - (TAGGED *)(X))
#define HeapCharDifference(X,Y)	((char *)(Y) - (char *)(X))
#define HeapOffset(X,O)		((TAGGED *)(X) + (O))
#define HeapCharOffset(X,O)	((TAGGED *)((char *)(X) + (O)))
#define HeapNext(X)		(*(X)++)
#define HeapPush(H,X)		(*(H) = (X), (H)++) /* X often contains H */
#define HeapDecr(H)		(--(H))

/* THE FRAME STACK */

	/* assuming stack growth in positive direction */

#define OnStack(t) (StackYounger(Stack_End,t) && !StackYounger(Stack_Start,t))
#define OffStacktop(t,H)         (!StackYounger(H,t))
#define StackYounger(X,Y)	((TAGGED *)(X)>(TAGGED *)(Y))
#define StackDifference(X,Y)	((TAGGED *)(Y) - (TAGGED *)(X))
#define StackCharDifference(X,Y)	((char *)(Y) - (char *)(X))
#define StackOffset(X,O)	((TAGGED *)(X) + (O))
#define StackCharOffset(X,O)	((struct frame *)((char *)(X) + (O)))
#define StackNext(P)		(*(P)++)
#define StackDecr(P)		(--(P))

/* THE TRAIL */

	/* assuming trail growth in positive direction */

#define OnTrail(t)  (TrailYounger(Trail_End,t) && !TrailYounger(Trail_Start,t))
#define OffTrailtop(t,P)	(!TrailYounger(P,t))
#define TrailYounger(X,Y)	((TAGGED *)(X)>(TAGGED *)(Y))
#define TrailDifference(X,Y)	((TAGGED *)(Y) - (TAGGED *)(X))
#define TrailCharDifference(X,Y)	((char *)(Y) - (char *)(X))
#define TrailOffset(X,O)	((TAGGED *)(X) + (O))
#define TrailCharOffset(X,O)	((TAGGED *)((char *)(X) + (O)))
#define TrailPop(P)		(*--(P))
#define TrailNext(P)		(*(P)++)
#define TrailPush(P,X)		(*(P)++ = (X))
#define TrailPushCheck(P,X)     trail_push_check(Arg,X)



/* THE CHOICEPOINT STACK */

	/* assuming choicestack growth in negative direction */

#define OnChoice(t) (ChoiceYounger(t,Choice_Start) && !ChoiceYounger(t,Choice_End))
#define OffChoicetop(t,B)	ChoiceYounger(t,B)
#define ChoiceYounger(X,Y)	((TAGGED *)(X)<(TAGGED *)(Y))
#define ChoiceOffset(X,O)	((TAGGED *)(X) - (O))
#define ChoiceCharOffset(X,O)	((struct node *)((char *)(X) - (O)))
#define ChoiceDifference(X,Y)	((TAGGED *)(X) - (TAGGED *)(Y))
#define ChoiceCharDifference(X,Y)	((char *)(X) - (char *)(Y))
#define ChoicePrev(P)		(*(P)++)
#define ChoiceNext(P)		(*--(P))
#define ChoicePush(P,X)		(*--(P) = (X))



#if defined(USE_TAGGED_CHOICE_START)
#define ChoiceFromInt(Y) (ChoiceCharOffset(Tagged_Choice_Start,Y))
#define ChoiceToInt(Y)	 (ChoiceCharDifference(Tagged_Choice_Start,Y))
#else
#define ChoiceFromInt(Y) ((struct node *)ChoiceOffset(Choice_Start,(GetSmall(Y))))
#define ChoiceToInt(Y)	 (MakeSmall(ChoiceDifference(Choice_Start,Y)))
#endif

#define RefHeap(To,From) \
{ To = *(From); }

#define RefCar(To,From) \
{ To = CTagToCar(From); }

#define RefCdr(To,From) \
{ To = CTagToCdr(From); }

#define RefArg(To,From,I) \
{ To = CTagToArg(From,I); }

#define RefHeapNext(To,From) \
{ To = *(From)++; }

#define PushRefHeapNext(To,From) \
{ *(To)++ = *(From)++; }

#define RefStack(To,From) \
{ To = *(From); }

#define HeapPushRefStack(To,From) \
{ *(To)++ = *(From); }

#define RefHVA(To,From) \
{ To = CTagToHVA(From); }

#define RefCVA(To,From) \
{ To = CTagToCVA(From); }

#define RefSVA(To,From) \
{ To = CTagToSVA(From); }

#define LoadSVA(Y)		{Y = TagSVA(&Y); }
#define Load2SVA(X,Y)		{X = Y = TagSVA(&Y); }
#define PreLoadHVA(X,H)		{X = TagHVA(H); }
#define ConstrHVA(H)		{HeapPush(H,TagHVA(H)); }
#define LoadHVA(To,H)		{HeapPush(H,To = TagHVA(H)); }
#define Load2HVA(To1,To2,H)	{HeapPush(H,To1 = To2 = TagHVA(H)); }
#define LoadCVA(To,H)		{HeapPush(H,To = TagCVA(H)); }
