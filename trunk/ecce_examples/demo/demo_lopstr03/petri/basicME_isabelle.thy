theory PN = Main:

types

 state = "nat \<times> nat \<times> nat \<times> nat \<times> nat"

consts

 paths:: "(state list) set"

 t1 :: "nat"
 t2 :: "nat"
 t3 :: "nat"
 t4 :: "nat"

 trans:: "(state \<times> state \<times> nat) set"

 start :: "state"

defs

 t1_def [simp]: "t1 \<equiv> 0"
 t2_def [simp]: "t2 \<equiv> 1"
 t3_def [simp]: "t3 \<equiv> 2"
 t4_def [simp]: "t4 \<equiv> 3"

 start_def [simp]: "start \<equiv> ((Suc 0),(Suc 0),(Suc 0),0,0)"

 trans_def: "trans \<equiv> {(x,y,n). (\<exists>  E D C B A. (x,y,n)=(((Suc A),(Suc B),(Suc C),D,E),(A,(Suc B),C,(Suc D),E),t1))
\<or> (\<exists>  E D C B A. (x,y,n)=(((Suc A),(Suc B),(Suc C),D,E),(A,B,(Suc C),D,(Suc E)),t2))
\<or> (\<exists>  E D C B A. (x,y,n)=((A,B,C,(Suc D),E),((Suc A),B,(Suc C),D,E),t3))
\<or> (\<exists>  E D C B A. (x,y,n)=((A,B,C,D,(Suc E)),((Suc A),(Suc B),C,D,E),t4))
}"

inductive paths
intros

 zero: "[(start)] \<in> paths"
 step: "\<lbrakk> (y,z,n) \<in> trans; y#l \<in> paths \<rbrakk> \<Longrightarrow> z#(y#l) \<in> paths"

consts

 coverrel:: "(state \<times> state) set"

 sat__1__2 :: "nat \<Rightarrow> state"
 sat_eu__2__3 :: "nat \<Rightarrow> state"
 sat_eu__2__4 :: "nat \<Rightarrow> state"
 sat_eu__2__5 :: "nat \<Rightarrow> state"

defs

 sat__1__2_def: "sat__1__2 A \<equiv> (A,(Suc 0),(Suc 0),0,0)"
 sat_eu__2__3_def: "sat_eu__2__3 A \<equiv> (A,(Suc 0),(Suc 0),0,0)"
 sat_eu__2__4_def: "sat_eu__2__4 A \<equiv> (A,(Suc 0),0,(Suc 0),0)"
 sat_eu__2__5_def: "sat_eu__2__5 A \<equiv> (A,0,(Suc 0),0,(Suc 0))"


 coverrel_def: "coverrel \<equiv>{(x,y). \<exists>  A. x=(sat__1__2 A) \<and> y=(sat_eu__2__3 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__3 (Suc A)) \<and> y=(sat_eu__2__4 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__3 (Suc A)) \<and> y=(sat_eu__2__5 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__4 A) \<and> y=(sat_eu__2__3 (Suc A))}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__5 A) \<and> y=(sat_eu__2__3 (Suc A))}
"

lemma "l \<in> paths \<Longrightarrow> \<exists> y. ((hd l),y) \<in> coverrel"
 apply(erule paths.induct)
   apply(simp only: start_def
                    coverrel_def)
   apply(simp only: sat__1__2_def sat_eu__2__3_def sat_eu__2__4_def sat_eu__2__5_def)
   apply(simp)
   apply(blast)
  apply(simp only: trans_def)
  apply(clarify)
  apply(((erule disjE)?,
           simp only: coverrel_def,
           simp,
           ((erule disjE)?,
              simp only: sat__1__2_def sat_eu__2__3_def sat_eu__2__4_def sat_eu__2__5_def,
              simp|blast)+)+)
