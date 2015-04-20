theory PN = Main:

types

 state = "nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat"

consts

 paths:: "(state list) set"

 t0 :: "nat"
 t1 :: "nat"
 t2 :: "nat"
 t3 :: "nat"
 t4 :: "nat"
 t5 :: "nat"
 t6 :: "nat"
 t7 :: "nat"
 t8 :: "nat"
 t9 :: "nat"
 t10 :: "nat"
 t11 :: "nat"
 t12 :: "nat"

 trans:: "(state \<times> state \<times> nat) set"

 start :: "state"

defs

 t0_def [simp]: "t0 \<equiv> 0"
 t1_def [simp]: "t1 \<equiv> 1"
 t2_def [simp]: "t2 \<equiv> 2"
 t3_def [simp]: "t3 \<equiv> 3"
 t4_def [simp]: "t4 \<equiv> 4"
 t5_def [simp]: "t5 \<equiv> 5"
 t6_def [simp]: "t6 \<equiv> 6"
 t7_def [simp]: "t7 \<equiv> 7"
 t8_def [simp]: "t8 \<equiv> 8"
 t9_def [simp]: "t9 \<equiv> 9"
 t10_def [simp]: "t10 \<equiv> 10"
 t11_def [simp]: "t11 \<equiv> 11"
 t12_def [simp]: "t12 \<equiv> 12"

 start_def [simp]: "start \<equiv> (0,0,0,0,0,(Suc 0),(Suc 0),(Suc 0),0,0,0,0,0,(Suc 0))"

 trans_def: "trans \<equiv> {(x,y,n). (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=(((Suc A),B,C,D,E,F,G,H,I,J,K,L,M,N),(A,(Suc B),C,D,E,F,G,H,I,J,K,L,M,N),t0))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,(Suc E),F,G,H,I,(Suc J),K,L,M,N),((Suc A),B,(Suc C),D,E,F,G,H,I,J,K,L,M,N),t1))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,(Suc E),F,(Suc G),H,I,J,K,L,M,N),((Suc A),B,C,(Suc D),E,F,G,H,I,J,K,L,M,N),t2))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,(Suc B),C,(Suc D),E,F,G,H,I,J,K,L,M,N),(A,B,C,D,E,(Suc F),(Suc G),H,I,J,K,L,M,N),t3))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,(Suc B),(Suc C),D,E,F,G,H,I,J,K,L,M,N),(A,B,C,D,E,(Suc F),G,H,I,(Suc J),K,L,M,N),t4))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,(Suc F),G,H,I,J,K,L,M,N),(A,B,C,D,(Suc E),F,G,H,I,J,K,L,M,N),t5))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,G,H,I,J,(Suc K),L,M,N),(A,B,C,D,E,F,G,(Suc H),I,J,K,L,M,N),t6))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,G,(Suc H),I,J,K,L,M,N),(A,B,C,D,E,F,G,H,(Suc I),J,K,L,M,N),t7))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,(Suc G),H,(Suc I),J,K,L,M,N),(A,B,C,D,E,F,G,H,I,(Suc J),K,L,M,N),t8))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,G,H,I,(Suc J),K,L,M,N),(A,B,C,D,E,F,(Suc G),H,I,J,(Suc K),L,M,N),t9))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,G,H,I,J,(Suc K),L,M,N),(A,B,C,D,E,F,G,H,I,J,K,L,(Suc M),N),t10))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,G,H,I,J,K,(Suc L),M,N),(A,B,C,D,E,F,G,H,(Suc I),J,K,L,M,(Suc N)),t11))
\<or> (\<exists>  N M L K J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,G,H,I,J,K,L,(Suc M),(Suc N)),(A,B,C,D,E,F,G,H,I,J,K,(Suc L),M,N),t12))
}"

inductive paths
intros

 zero: "[(start)] \<in> paths"
 step: "\<lbrakk> (y,z,n) \<in> trans; y#l \<in> paths \<rbrakk> \<Longrightarrow> z#(y#l) \<in> paths"

consts

 coverrel:: "(state \<times> state) set"

 sat__1__2 :: "nat \<Rightarrow> state"
 sat_eu__2__3 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__4 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__5 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__6 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__7 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__8 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__9 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__10 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__11 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__12 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__13 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__14 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__15 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__16 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__17 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"
 sat_eu__2__18 :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> state"

defs

 sat__1__2_def: "sat__1__2 A \<equiv> (0,0,0,0,0,(Suc 0),(Suc 0),A,0,0,0,0,0,(Suc 0))"
 sat_eu__2__3_def: "sat_eu__2__3 A B C D \<equiv> (0,0,0,0,0,(Suc 0),(Suc 0),A,B,0,C,0,D,(Suc 0))"
 sat_eu__2__4_def: "sat_eu__2__4 A B C D \<equiv> (0,0,0,0,0,(Suc 0),(Suc 0),A,B,0,C,(Suc 0),D,0)"
 sat_eu__2__5_def: "sat_eu__2__5 A B C D \<equiv> (0,0,0,0,0,(Suc 0),0,A,B,(Suc 0),C,(Suc 0),D,0)"
 sat_eu__2__6_def: "sat_eu__2__6 A B C D \<equiv> (0,0,0,0,0,(Suc 0),0,A,B,(Suc 0),C,0,D,(Suc 0))"
 sat_eu__2__7_def: "sat_eu__2__7 A B C D \<equiv> (0,0,0,0,(Suc 0),0,0,A,B,(Suc 0),C,0,D,(Suc 0))"
 sat_eu__2__8_def: "sat_eu__2__8 A B C D \<equiv> (0,0,0,0,(Suc 0),0,0,A,B,(Suc 0),C,(Suc 0),D,0)"
 sat_eu__2__9_def: "sat_eu__2__9 A B C D \<equiv> ((Suc 0),0,(Suc 0),0,0,0,0,A,B,0,C,(Suc 0),D,0)"
 sat_eu__2__10_def: "sat_eu__2__10 A B C D \<equiv> (0,0,0,0,(Suc 0),0,(Suc 0),A,B,0,C,(Suc 0),D,0)"
 sat_eu__2__11_def: "sat_eu__2__11 A B C D \<equiv> (0,0,0,0,(Suc 0),0,(Suc 0),A,B,0,C,0,D,(Suc 0))"
 sat_eu__2__12_def: "sat_eu__2__12 A B C D \<equiv> ((Suc 0),0,0,(Suc 0),0,0,0,A,B,0,C,0,D,(Suc 0))"
 sat_eu__2__13_def: "sat_eu__2__13 A B C D \<equiv> ((Suc 0),0,0,(Suc 0),0,0,0,A,B,0,C,(Suc 0),D,0)"
 sat_eu__2__14_def: "sat_eu__2__14 A B C D \<equiv> (0,(Suc 0),0,(Suc 0),0,0,0,A,B,0,C,(Suc 0),D,0)"
 sat_eu__2__15_def: "sat_eu__2__15 A B C D \<equiv> (0,(Suc 0),0,(Suc 0),0,0,0,A,B,0,C,0,D,(Suc 0))"
 sat_eu__2__16_def: "sat_eu__2__16 A B C D \<equiv> ((Suc 0),0,(Suc 0),0,0,0,0,A,B,0,C,0,D,(Suc 0))"
 sat_eu__2__17_def: "sat_eu__2__17 A B C D \<equiv> (0,(Suc 0),(Suc 0),0,0,0,0,A,B,0,C,0,D,(Suc 0))"
 sat_eu__2__18_def: "sat_eu__2__18 A B C D \<equiv> (0,(Suc 0),(Suc 0),0,0,0,0,A,B,0,C,(Suc 0),D,0)"


 coverrel_def: "coverrel \<equiv>{(x,y). \<exists>  A. x=(sat__1__2 A) \<and> y=(sat_eu__2__3 A 0 0 0)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__3 A B C D) \<and> y=(sat_eu__2__11 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__3 A B (Suc C) D) \<and> y=(sat_eu__2__3 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__3 (Suc A) B C D) \<and> y=(sat_eu__2__3 A (Suc B) C D)}
\<union> {(x,y). \<exists>  B A C D. x=(sat_eu__2__3 A (Suc B) C D) \<and> y=(sat_eu__2__6 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__3 A B (Suc C) D) \<and> y=(sat_eu__2__3 A B C (Suc D))}
\<union> {(x,y). \<exists>  D A B C. x=(sat_eu__2__3 A B C (Suc D)) \<and> y=(sat_eu__2__4 A B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__4 A B C D) \<and> y=(sat_eu__2__10 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__4 A B (Suc C) D) \<and> y=(sat_eu__2__4 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__4 (Suc A) B C D) \<and> y=(sat_eu__2__4 A (Suc B) C D)}
\<union> {(x,y). \<exists>  B A C D. x=(sat_eu__2__4 A (Suc B) C D) \<and> y=(sat_eu__2__5 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__4 A B (Suc C) D) \<and> y=(sat_eu__2__4 A B C (Suc D))}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__4 A B C D) \<and> y=(sat_eu__2__3 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__5 A B C D) \<and> y=(sat_eu__2__8 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__5 A B (Suc C) D) \<and> y=(sat_eu__2__5 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__5 (Suc A) B C D) \<and> y=(sat_eu__2__5 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__5 A B C D) \<and> y=(sat_eu__2__4 A B (Suc C) D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__5 A B (Suc C) D) \<and> y=(sat_eu__2__5 A B C (Suc D))}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__5 A B C D) \<and> y=(sat_eu__2__6 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__6 A B C D) \<and> y=(sat_eu__2__7 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__6 A B (Suc C) D) \<and> y=(sat_eu__2__6 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__6 (Suc A) B C D) \<and> y=(sat_eu__2__6 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__6 A B C D) \<and> y=(sat_eu__2__3 A B (Suc C) D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__6 A B (Suc C) D) \<and> y=(sat_eu__2__6 A B C (Suc D))}
\<union> {(x,y). \<exists>  D A B C. x=(sat_eu__2__6 A B C (Suc D)) \<and> y=(sat_eu__2__5 A B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__7 A B C D) \<and> y=(sat_eu__2__16 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__7 A B (Suc C) D) \<and> y=(sat_eu__2__7 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__7 (Suc A) B C D) \<and> y=(sat_eu__2__7 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__7 A B C D) \<and> y=(sat_eu__2__11 A B (Suc C) D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__7 A B (Suc C) D) \<and> y=(sat_eu__2__7 A B C (Suc D))}
\<union> {(x,y). \<exists>  D A B C. x=(sat_eu__2__7 A B C (Suc D)) \<and> y=(sat_eu__2__8 A B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__8 A B C D) \<and> y=(sat_eu__2__9 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__8 A B (Suc C) D) \<and> y=(sat_eu__2__8 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__8 (Suc A) B C D) \<and> y=(sat_eu__2__8 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__8 A B C D) \<and> y=(sat_eu__2__10 A B (Suc C) D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__8 A B (Suc C) D) \<and> y=(sat_eu__2__8 A B C (Suc D))}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__8 A B C D) \<and> y=(sat_eu__2__7 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__9 A B C D) \<and> y=(sat_eu__2__18 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__9 A B (Suc C) D) \<and> y=(sat_eu__2__9 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__9 (Suc A) B C D) \<and> y=(sat_eu__2__9 A (Suc B) C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__9 A B (Suc C) D) \<and> y=(sat_eu__2__9 A B C (Suc D))}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__9 A B C D) \<and> y=(sat_eu__2__16 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__10 A B C D) \<and> y=(sat_eu__2__13 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__10 A B (Suc C) D) \<and> y=(sat_eu__2__10 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__10 (Suc A) B C D) \<and> y=(sat_eu__2__10 A (Suc B) C D)}
\<union> {(x,y). \<exists>  B A C D. x=(sat_eu__2__10 A (Suc B) C D) \<and> y=(sat_eu__2__8 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__10 A B (Suc C) D) \<and> y=(sat_eu__2__10 A B C (Suc D))}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__10 A B C D) \<and> y=(sat_eu__2__11 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__11 A B C D) \<and> y=(sat_eu__2__12 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__11 A B (Suc C) D) \<and> y=(sat_eu__2__11 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__11 (Suc A) B C D) \<and> y=(sat_eu__2__11 A (Suc B) C D)}
\<union> {(x,y). \<exists>  B A C D. x=(sat_eu__2__11 A (Suc B) C D) \<and> y=(sat_eu__2__7 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__11 A B (Suc C) D) \<and> y=(sat_eu__2__11 A B C (Suc D))}
\<union> {(x,y). \<exists>  D A B C. x=(sat_eu__2__11 A B C (Suc D)) \<and> y=(sat_eu__2__10 A B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__12 A B C D) \<and> y=(sat_eu__2__15 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__12 A B (Suc C) D) \<and> y=(sat_eu__2__12 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__12 (Suc A) B C D) \<and> y=(sat_eu__2__12 A (Suc B) C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__12 A B (Suc C) D) \<and> y=(sat_eu__2__12 A B C (Suc D))}
\<union> {(x,y). \<exists>  D A B C. x=(sat_eu__2__12 A B C (Suc D)) \<and> y=(sat_eu__2__13 A B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__13 A B C D) \<and> y=(sat_eu__2__14 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__13 A B (Suc C) D) \<and> y=(sat_eu__2__13 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__13 (Suc A) B C D) \<and> y=(sat_eu__2__13 A (Suc B) C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__13 A B (Suc C) D) \<and> y=(sat_eu__2__13 A B C (Suc D))}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__13 A B C D) \<and> y=(sat_eu__2__12 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__14 A B C D) \<and> y=(sat_eu__2__4 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__14 A B (Suc C) D) \<and> y=(sat_eu__2__14 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__14 (Suc A) B C D) \<and> y=(sat_eu__2__14 A (Suc B) C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__14 A B (Suc C) D) \<and> y=(sat_eu__2__14 A B C (Suc D))}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__14 A B C D) \<and> y=(sat_eu__2__15 A (Suc B) C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__15 A B C D) \<and> y=(sat_eu__2__3 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__15 A B (Suc C) D) \<and> y=(sat_eu__2__15 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__15 (Suc A) B C D) \<and> y=(sat_eu__2__15 A (Suc B) C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__15 A B (Suc C) D) \<and> y=(sat_eu__2__15 A B C (Suc D))}
\<union> {(x,y). \<exists>  D A B C. x=(sat_eu__2__15 A B C (Suc D)) \<and> y=(sat_eu__2__14 A B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__16 A B C D) \<and> y=(sat_eu__2__17 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__16 A B (Suc C) D) \<and> y=(sat_eu__2__16 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__16 (Suc A) B C D) \<and> y=(sat_eu__2__16 A (Suc B) C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__16 A B (Suc C) D) \<and> y=(sat_eu__2__16 A B C (Suc D))}
\<union> {(x,y). \<exists>  D A B C. x=(sat_eu__2__16 A B C (Suc D)) \<and> y=(sat_eu__2__9 A B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__17 A B C D) \<and> y=(sat_eu__2__6 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__17 A B (Suc C) D) \<and> y=(sat_eu__2__17 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__17 (Suc A) B C D) \<and> y=(sat_eu__2__17 A (Suc B) C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__17 A B (Suc C) D) \<and> y=(sat_eu__2__17 A B C (Suc D))}
\<union> {(x,y). \<exists>  D A B C. x=(sat_eu__2__17 A B C (Suc D)) \<and> y=(sat_eu__2__18 A B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__18 A B C D) \<and> y=(sat_eu__2__5 A B C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__18 A B (Suc C) D) \<and> y=(sat_eu__2__18 (Suc A) B C D)}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__18 (Suc A) B C D) \<and> y=(sat_eu__2__18 A (Suc B) C D)}
\<union> {(x,y). \<exists>  C A B D. x=(sat_eu__2__18 A B (Suc C) D) \<and> y=(sat_eu__2__18 A B C (Suc D))}
\<union> {(x,y). \<exists>  A B C D. x=(sat_eu__2__18 A B C D) \<and> y=(sat_eu__2__17 A (Suc B) C D)}
"

lemma "l \<in> paths \<Longrightarrow> \<exists> y. ((hd l),y) \<in> coverrel"
 apply(erule paths.induct)
   apply(simp only: start_def
                    coverrel_def)
   apply(simp only: sat__1__2_def sat_eu__2__3_def sat_eu__2__4_def sat_eu__2__5_def sat_eu__2__6_def sat_eu__2__7_def sat_eu__2__8_def sat_eu__2__9_def sat_eu__2__10_def sat_eu__2__11_def sat_eu__2__12_def sat_eu__2__13_def sat_eu__2__14_def sat_eu__2__15_def sat_eu__2__16_def sat_eu__2__17_def sat_eu__2__18_def)
   apply(simp)
   apply(blast)
  apply(simp only: trans_def)
  apply(clarify)
  apply(((erule disjE)?,
           simp only: coverrel_def,
           simp,
           ((erule disjE)?,
              simp only: sat__1__2_def sat_eu__2__3_def sat_eu__2__4_def sat_eu__2__5_def sat_eu__2__6_def sat_eu__2__7_def sat_eu__2__8_def sat_eu__2__9_def sat_eu__2__10_def sat_eu__2__11_def sat_eu__2__12_def sat_eu__2__13_def sat_eu__2__14_def sat_eu__2__15_def sat_eu__2__16_def sat_eu__2__17_def sat_eu__2__18_def,
              simp|blast)+)+)
