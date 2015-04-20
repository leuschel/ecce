theory PN = Main:

types

 state = "nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat \<times> nat"

consts

 paths:: "(state list) set"

 start :: "nat"
 alert :: "nat"
 do :: "nat"
 off_hook :: "nat"
 no_signal :: "nat"
 do2 :: "nat"
 on_hook :: "nat"
 abort :: "nat"
 disconnect_tone :: "nat"
 on_hook2 :: "nat"
 on_hook3 :: "nat"
 repeat :: "nat"

 trans:: "(state \<times> state \<times> nat) set"

 start :: "state"

defs

 start_def [simp]: "start \<equiv> 0"
 alert_def [simp]: "alert \<equiv> 1"
 do_def [simp]: "do \<equiv> 2"
 off_hook_def [simp]: "off_hook \<equiv> 3"
 no_signal_def [simp]: "no_signal \<equiv> 4"
 do2_def [simp]: "do2 \<equiv> 5"
 on_hook_def [simp]: "on_hook \<equiv> 6"
 abort_def [simp]: "abort \<equiv> 7"
 disconnect_tone_def [simp]: "disconnect_tone \<equiv> 8"
 on_hook2_def [simp]: "on_hook2 \<equiv> 9"
 on_hook3_def [simp]: "on_hook3 \<equiv> 10"
 repeat_def [simp]: "repeat \<equiv> 11"

 start_def [simp]: "start \<equiv> ((Suc 0),0,0,0,0,0,0,0,0,0)"

 trans_def: "trans \<equiv> {(x,y,n). (\<exists>  J I H G F E D C B A. (x,y,n)=(((Suc A),B,C,D,E,F,G,H,I,J),(A,(Suc B),C,D,E,F,G,H,I,J),start))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,(Suc B),C,D,E,F,G,H,I,J),(A,B,(Suc C),D,E,F,G,H,I,J),alert))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,(Suc C),D,E,F,G,H,I,J),(A,B,C,(Suc D),E,F,G,H,I,J),do))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,(Suc D),E,F,G,H,I,J),(A,B,C,D,(Suc E),F,G,H,I,J),off_hook))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,(Suc D),E,F,G,H,I,J),(A,B,C,D,E,F,G,H,(Suc I),J),no_signal))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,D,(Suc E),F,G,H,I,J),(A,B,C,D,E,(Suc F),G,H,I,J),do2))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,(Suc F),G,H,I,J),(A,B,C,D,E,F,G,H,(Suc I),J),on_hook))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,(Suc F),G,H,I,J),(A,B,C,D,E,F,(Suc G),H,I,J),abort))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,(Suc F),G,H,I,J),(A,B,C,D,E,F,G,(Suc H),I,J),disconnect_tone))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,(Suc G),H,I,J),(A,B,C,D,E,F,G,H,(Suc I),J),on_hook2))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,G,(Suc H),I,J),(A,B,C,D,E,F,G,H,(Suc I),J),on_hook3))
\<or> (\<exists>  J I H G F E D C B A. (x,y,n)=((A,B,C,D,E,F,G,H,(Suc I),J),(A,(Suc B),C,D,E,F,G,H,I,(Suc J)),repeat))
}"

inductive paths
intros

 zero: "[(start)] \<in> paths"
 step: "\<lbrakk> (y,z,n) \<in> trans; y#l \<in> paths \<rbrakk> \<Longrightarrow> z#(y#l) \<in> paths"

consts

 coverrel:: "(state \<times> state) set"

 sat__1__2 :: " \<Rightarrow> state"
 sat_eu__2__3 :: " \<Rightarrow> state"
 sat_eu__2__4 :: "nat \<Rightarrow> state"
 sat_eu__2__5 :: "nat \<Rightarrow> state"
 sat_eu__2__6 :: "nat \<Rightarrow> state"
 sat_eu__2__7 :: "nat \<Rightarrow> state"
 sat_eu__2__8 :: "nat \<Rightarrow> state"
 sat_eu__2__9 :: "nat \<Rightarrow> state"
 sat_eu__2__10 :: "nat \<Rightarrow> state"
 sat_eu__2__11 :: "nat \<Rightarrow> state"

defs

 sat__1__2_def: "sat__1__2 \<equiv> ((Suc 0),0,0,0,0,0,0,0,0,0)"
 sat_eu__2__3_def: "sat_eu__2__3 \<equiv> ((Suc 0),0,0,0,0,0,0,0,0,0)"
 sat_eu__2__4_def: "sat_eu__2__4 A \<equiv> (0,(Suc 0),0,0,0,0,0,0,0,A)"
 sat_eu__2__5_def: "sat_eu__2__5 A \<equiv> (0,0,(Suc 0),0,0,0,0,0,0,A)"
 sat_eu__2__6_def: "sat_eu__2__6 A \<equiv> (0,0,0,(Suc 0),0,0,0,0,0,A)"
 sat_eu__2__7_def: "sat_eu__2__7 A \<equiv> (0,0,0,0,(Suc 0),0,0,0,0,A)"
 sat_eu__2__8_def: "sat_eu__2__8 A \<equiv> (0,0,0,0,0,0,0,0,(Suc 0),A)"
 sat_eu__2__9_def: "sat_eu__2__9 A \<equiv> (0,0,0,0,0,(Suc 0),0,0,0,A)"
 sat_eu__2__10_def: "sat_eu__2__10 A \<equiv> (0,0,0,0,0,0,(Suc 0),0,0,A)"
 sat_eu__2__11_def: "sat_eu__2__11 A \<equiv> (0,0,0,0,0,0,0,(Suc 0),0,A)"


 coverrel_def: "coverrel \<equiv>{(x,y). \<exists> . x=(sat__1__2) \<and> y=(sat_eu__2__3)}
\<union> {(x,y). \<exists> . x=(sat_eu__2__3) \<and> y=(sat_eu__2__4 0)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__4 A) \<and> y=(sat_eu__2__5 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__5 A) \<and> y=(sat_eu__2__6 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__6 A) \<and> y=(sat_eu__2__7 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__6 A) \<and> y=(sat_eu__2__8 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__7 A) \<and> y=(sat_eu__2__9 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__8 A) \<and> y=(sat_eu__2__4 (Suc A))}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__9 A) \<and> y=(sat_eu__2__8 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__9 A) \<and> y=(sat_eu__2__10 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__9 A) \<and> y=(sat_eu__2__11 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__10 A) \<and> y=(sat_eu__2__8 A)}
\<union> {(x,y). \<exists>  A. x=(sat_eu__2__11 A) \<and> y=(sat_eu__2__8 A)}
"

lemma "l \<in> paths \<Longrightarrow> \<exists> y. ((hd l),y) \<in> coverrel"
 apply(erule paths.induct)
   apply(simp only: start_def
                    coverrel_def)
   apply(simp only: sat__1__2_def sat_eu__2__3_def sat_eu__2__4_def sat_eu__2__5_def sat_eu__2__6_def sat_eu__2__7_def sat_eu__2__8_def sat_eu__2__9_def sat_eu__2__10_def sat_eu__2__11_def)
   apply(simp)
   apply(blast)
  apply(simp only: trans_def)
  apply(clarify)
  apply(((erule disjE)?,
           simp only: coverrel_def,
           simp,
           ((erule disjE)?,
              simp only: sat__1__2_def sat_eu__2__3_def sat_eu__2__4_def sat_eu__2__5_def sat_eu__2__6_def sat_eu__2__7_def sat_eu__2__8_def sat_eu__2__9_def sat_eu__2__10_def sat_eu__2__11_def,
              simp|blast)+)+)
