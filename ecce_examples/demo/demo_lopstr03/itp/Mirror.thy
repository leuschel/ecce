theory Mirror = PreList:

  datatype 'a tree = Tip                ("[]")
                   | Node "'a tree" 'a "'a tree"

  consts mirror :: "'a tree => 'a tree" 


  primrec
  "mirror([]) = []"
  "mirror((Node ls x rs)) = Node (mirror(rs)) x (mirror(ls))"



  


lemma mirror_mirror [simp]: "mirror(mirror(xs)) = xs"
apply (induct_tac xs)
ML "set trace_simp"
apply(simp)
apply(auto)
done


