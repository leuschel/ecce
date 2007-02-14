/* ----------- */
/* COMPILE.PRO */
/* ----------- */
/* File: '$BIMTOOLS_PATH/compile.pro'. */

/* :- ensure_consulted('$BIMTOOLS_PATH/unify.pro'). */

/* ================= */
/* C O M P I L I N G */
/* ================= */

/* ----------------- */
/* program_compile/2 */
/* ----------------- */
/* program_compile(NonGroundProg,GroundProg) */

program_compile([],[]).
program_compile([clause(Head,Body)|T],[CompClause|CT]) :-
	structify(Head,SHead),
	l_structify(Body,SBody),
	copy(SHead,CHead),
	copy(SBody,CBody),
	numbervars(struct(clause,[CHead|CBody]),1,_VarIndex),
	CompClause = clause(CHead,CBody),
	program_compile(T,CT).


term_compile(NonGround,Ground) :-
	copy(NonGround,CNG),
	structify(CNG,Ground),
	numbervars(Ground,1,_VarIndex).

l_term_compile(NonGroundLs,GroundLs) :-
	copy(NonGroundLs,CNGLs),
	l_structify(CNGLs,GroundLs),
	numbervars(GroundLs,1,_Varindex).


structify(X,X) :- var(X),!.
structify(Y,struct(F,SArgs)) :-
	Y =..[F|Args],
	l_structify(Args,SArgs).
l_structify([],[]).
l_structify([H|T],[SH|ST]) :-
	structify(H,SH),
	l_structify(T,ST).


/* ===================== */
/* D E C O M P I L I N G */
/* ===================== */

term_decompile(Ground,NonGround) :-
	unnumbervars(Ground,NGS),
	destructify(NGS,NonGround).

l_term_decompile(Ground,NonGround) :-
	unnumbervars(Ground,NGS),
	l_destructify(NGS,NonGround).



destructify(X,X) :- var(X),!.
destructify(struct(F,SArgs),Y) :-
	l_destructify(SArgs,Args),
	Y =..[F|Args].
l_destructify([],[]).
l_destructify([H|T],[DSH|DST]) :-
	destructify(H,DSH),
	l_destructify(T,DST).

