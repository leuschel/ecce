:- module(parameters,_).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */


/* last value is default value */

available_parameters(abstraction,97,
	'Abstraction operation',
	'Abs',108).
available_parameters(instchecks,99,
	'Instance Check (at the global level)',
	'InstCheck',118).
available_parameters(msv,109,
	'More specific resolution steps',
	'Msv',115).
available_parameters(negsolve,110,
	'Negative solve rules',
	'NgSlv',103).
available_parameters(partition,112,
	'Partitioning Function',
	'Part',101).
available_parameters(postprune,114,
	'post-unfolding pRuning',
	'Prun',110).
available_parameters(selectionrule,115,
	'Selection rule for unfolding',
	'Sel',116).
available_parameters(whistle,119,
	'Whistle for termination',
	'Whstl',102).
