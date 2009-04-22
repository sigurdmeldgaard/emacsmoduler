(*
    Example proof script for Coq Proof General (Coq V8 syntax).

    example.v,v 9.0 2008/01/30 15:22:07 da Exp
*)

Module Example.

Goal forall (A B:Prop),(A /\ B) -> (B /\ A).
  intros A B.
  intros H.
  elim H.
  split.
  assumption.
  assumption.
Save and_comms.

End Example.



