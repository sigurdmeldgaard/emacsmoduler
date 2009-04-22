(*
      Example proof document for Isabelle/Isar Proof General.
   
      Example-Xsym.thy,v 9.1 2008/03/14 20:42:52 makarius Exp
*)

theory "Example-Xsym" imports Main begin

text {* Proper proof text -- \textit{naive version}. *}

theorem and_comms: "A \<and> B \<longrightarrow> B \<and> A"
proof
  assume "A \<and> B"
  then show "B \<and> A"
  proof
    assume B and A
    then show ?thesis ..
 qed
qed


text {* Proper proof text -- \textit{advanced version}. *}

theorem "A \<and> B \<longrightarrow> B \<and> A"
proof
  assume "A \<and> B"
  then obtain B and A ..
  then show "B \<and> A" ..
qed


text {* Unstructured proof script. *}

theorem "A \<and> B \<longrightarrow> B \<and> A"
  apply (rule impI)
  apply (erule conjE)
  apply (rule conjI)
  apply assumption
  apply assumption
done

end
