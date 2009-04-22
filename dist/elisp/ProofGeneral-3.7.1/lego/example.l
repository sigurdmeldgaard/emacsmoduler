(*
    Example proof script for Lego Proof General.
 
    example.l,v 9.0 2008/01/30 15:22:19 da Exp
*)

Module example Import lib_logic;

Goal {A,B:Prop}(A /\ B) -> (B /\ A);
intros;
Refine H;
intros;
andI;
Immed;
Save and_comms;
