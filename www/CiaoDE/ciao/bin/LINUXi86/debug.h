/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* Debugging flags */

#if !defined(_DEBUG_H_)
#define _DEBUG_H_

#if defined(DEBUG)
extern int debug_c;                                             /* Shared */
extern BOOL debug_gc, 
            debug_threads, 
            debug_choicepoints, 
            debug_concchoicepoints, 
            debug_mem,  
            debug_conc;
#endif

extern BOOL stop_on_pred_calls, predtrace;

#endif /* _DEBUG_H */
