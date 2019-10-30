(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Decomposition of the disjunctive constraint. *)

open Core
open Bounds
open Lang.Ast

module Make(B: Bound_sig.S) =
struct
  type task = {
    start: string;
    duration: B.t;
  }

  let non_overlap t1 t2 =
    let s1,s2 = Var t1.start, Var t2.start in
    let d1 = Cst (B.to_rat (B.neg t1.duration), B.concrete_ty) in
    let d2 = Cst (B.to_rat (B.neg t2.duration), B.concrete_ty) in
    let c1 = Cmp (Binary (s1, SUB, s2), LEQ, d1) in
    let c2 = Cmp (Binary (s2, SUB, s1), LEQ, d2) in
    Or (c1, c2)

  let overlap_before t1 t2 =
    let s1,s2 = Var t1.start, Var t2.start in
    let d1 = Cst (B.to_rat t1.duration, B.concrete_ty) in
    let c1 = Cmp (s1, LEQ, s2) in
    let c2 = Cmp (Binary (s2, SUB, s1), LT, d1) in
    And (c1, c2)

  let disjunctive tasks =
    let non_overlap t1 t2 = [non_overlap t1 t2] in
    conjunction (Tools.for_all_asymmetric_pairs tasks non_overlap)

  let at_instant task instant =
    let i = B.to_rat instant in
    let before_i = Cmp (Var task.start, LEQ, Cst (i, B.concrete_ty)) in
    let i' = Bound_rat.sub_up i (B.to_rat task.duration) in
    let after_i = Cmp (Var task.start, GT, Cst (i', B.concrete_ty)) in
    And (before_i, after_i)

  let precedence s1 s2 d =
    let d = Cst(Bound_rat.neg (B.to_rat d), B.concrete_ty) in
    Cmp (Binary (Var s1, SUB, Var s2), LEQ, d)
end
