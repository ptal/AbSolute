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
open Lang.Rewritting

module type S = sig
  module D: Bound_sig.S
  type param_or_var =
    | Param of D.t
    | Variable of string
  type task = {
    start: string;
    duration: param_or_var;
  }
  val non_overlap: task -> task -> formula
  val overlap_before: task -> task -> formula
  val disjunctive: task list -> formula
  val at_instant: task -> param_or_var -> formula
  val precedence: string -> string -> param_or_var -> formula
end

module Make(D: Bound_sig.S) =
struct
  module D=D

  type param_or_var =
    | Param of D.t
    | Variable of string

  type task = {
    start: string;
    duration: param_or_var;
  }

  let to_neg_expr = function
    | Param p -> Cst (D.to_rat (D.neg p), D.concrete_ty)
    | Variable x -> Unary (NEG, (Var x))

  let to_expr = function
    | Param p -> Cst (D.to_rat p, D.concrete_ty)
    | Variable x -> Var x

  let to_sub_expr = function
    | Param p1, Param p2 -> Cst (Bound_rat.sub_up
        (D.to_rat p1)
        (D.to_rat p2), D.concrete_ty)
    | pv1, pv2 ->
        let e1, e2 = to_expr pv1, to_expr pv2 in
        Binary (e1, SUB, e2)

  let non_overlap t1 t2 =
    let s1,s2 = Var t1.start, Var t2.start in
    let d1,d2 = to_neg_expr t1.duration, to_neg_expr t2.duration in
    let c1 = Cmp (Binary (s1, SUB, s2), LEQ, d1) in
    let c2 = Cmp (Binary (s2, SUB, s1), LEQ, d2) in
    Or (c1, c2)

  let overlap_before t1 t2 =
    let s1,s2 = Var t1.start, Var t2.start in
    let d1 = to_expr t1.duration in
    let c1 = Cmp (s1, LEQ, s2) in
    let c2 = Cmp (Binary (s2, SUB, s1), LT, d1) in
    And (c1, c2)

  let disjunctive tasks =
    let non_overlap t1 t2 = [non_overlap t1 t2] in
    conjunction (Tools.for_all_asymmetric_pairs tasks non_overlap)

  let at_instant task instant =
    let before_i = Cmp (Var task.start, LEQ, to_expr instant) in
    let after_i = Cmp (Var task.start, GT, to_sub_expr (instant, task.duration)) in
    And (before_i, after_i)

  let precedence s1 s2 d =
    Cmp (Binary (Var s1, SUB, Var s2), LEQ, to_neg_expr d)
end
