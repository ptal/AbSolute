(** Decomposition of the disjunctive constraint. *)

open Core
open Bounds
open Lang.Ast

module Make(B: Bound_sig.S) =
struct
  type task = {
    name: string;
    duration: B.t;
  }

  let ty = if B.is_continuous then Real else Int

  let non_overlap t1 t2 =
    let s1,s2 = Var t1.name, Var t2.name in
    let d1 = Cst (B.to_rat (B.neg t1.duration), ty) in
    let d2 = Cst (B.to_rat (B.neg t2.duration), ty) in
    let c1 = Cmp (Binary (s1, SUB, s2), LEQ, d1) in
    let c2 = Cmp (Binary (s2, SUB, s1), LEQ, d2) in
    Or (c1, c2)

  let overlap t1 t2 =
    let s1,s2 = Var t1.name, Var t2.name in
    let d1 = Cst (B.to_rat t1.duration, ty) in
    let c1 = Cmp (s1, LEQ, s2) in
    let c2 = Cmp (Binary (s2, SUB, s1), LT, d1) in
    And (c1, c2)

  let disjunctive tasks =
    let non_overlap t1 t2 = [non_overlap t1 t2] in
    Tools.for_all_asymmetric_pairs tasks non_overlap
end
