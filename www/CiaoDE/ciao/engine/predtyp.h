/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* Emulator "pseudo instructions" - never seen by main dispatchers. */

/* These six must be first, in this order. */

#define ENTER_COMPACTCODE 0
#define ENTER_COMPACTCODE_INDEXED 1
#define ENTER_PROFILEDCODE 2
#define ENTER_PROFILEDCODE_INDEXED 3
#define ENTER_FASTCODE 4
#define ENTER_FASTCODE_INDEXED 5
#define ENTER_UNDEFINED 6
#define ENTER_C 7
#define ENTER_INTERPRETED 8
#define BUILTIN_ABORT 9
#define BUILTIN_APPLY 10
#define BUILTIN_CALL 11
#define BUILTIN_SYSCALL 12
#define BUILTIN_NODEBUGCALL 13
#define BUILTIN_TRUE 14
#define BUILTIN_FAIL 15
#define BUILTIN_CURRENT_INSTANCE 16
#define BUILTIN_RESTORE 17
#define BUILTIN_COMPILE_TERM 18
#define BUILTIN_GELER 19
#define BUILTIN_INSTANCE 20
#define BUILTIN_DIF 21
#define SPYPOINT 22
#define WAITPOINT 23
#define BREAKPOINT 24

#define TABLE 25           /* These are used to inform the gc */
#define EMUL_INFO 26
#define OTHER_STUFF 27

