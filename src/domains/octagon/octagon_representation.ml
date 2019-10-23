(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Lang
open Lang.Ast
open Bounds
open Dbm

let rec normalize_expr e =
  let neg e = Unary (NEG, e) in
  match e with
  | Unary (NEG, Cst(c,a)) -> Cst (Bound_rat.neg c, a)
  | Unary (NEG, Unary (NEG, e)) -> normalize_expr e
  | Unary (NEG, Binary (x, SUB, y)) -> normalize_expr (Binary (neg x, ADD, y))
  | Unary (NEG, Binary (x, ADD, y)) -> normalize_expr (Binary (neg x, SUB, y))
  | Unary (op, x) -> Unary(op, normalize_expr x)
  | Binary (x, SUB, Unary (NEG, y)) -> normalize_expr (Binary (x, ADD, y))
  | Binary (x, ADD, Unary (NEG, y)) -> normalize_expr (Binary (x, SUB, y))
  | Binary (x, op, y) -> Binary (normalize_expr x, op, normalize_expr y)
  | e -> e

let normalize (e1, op, e2) = ((normalize_expr e1), op, (normalize_expr e2))

let rec generic_rewrite c =
  match normalize c with
  | e1, GEQ, e2 -> generic_rewrite (Unary (NEG, e1), LEQ, Unary (NEG, e2))
  | e1, GT, e2 -> generic_rewrite (Unary (NEG, e1), LT, Unary (NEG, e2))
  | Var x, op, Var y -> generic_rewrite (Binary (Var x, SUB, Var y), op, Cst (Bound_rat.zero, Int))
  | e1, EQ, e2 -> (generic_rewrite (e1, LEQ, e2))@(generic_rewrite (e1, GEQ, e2))
  | c -> [c]

module type Octagon_rep_sig =
sig
  module B: Bound_sig.S
  type t
  type var_kind = unit
  type var_id = dbm_interval
  type rconstraint = B.t dbm_constraint
  val empty: t
  val extend: t -> (var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> formula -> rconstraint list
  val relax: t -> formula -> rconstraint list
  val negate: rconstraint -> rconstraint
end

module Octagon_rep(B: Bound_sig.S) =
struct
  type var_id = dbm_interval
  type var_kind = unit
  type rconstraint = B.t dbm_constraint

  module B = B
  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=var_id
    let compare = compare end)

  type t = {
    (* maps each variable name to its DBM interval. *)
    env: var_id Env.t;
    (* reversed mapping of `env`. *)
    renv: var REnv.t;
  }

  let empty = {env=Env.empty; renv=REnv.empty}
  let extend repr (v,itv) = {
    env=(Env.add v itv repr.env);
    renv=(REnv.add itv v repr.renv);
  }

  let to_logic_var repr itv = REnv.find itv repr.renv
  let to_abstract_var repr v = Env.find v repr.env

  let dim_of_var repr v =
    let itv = to_abstract_var repr v in
    let k1, k2 = (itv.lb.l / 2), (itv.lb.c / 2) in
    if k1 <> k2 then failwith "Octagon_rep.dim_of_var: only variable with a canonical plane are defined on a single dimension."
    else k1

  (** If the bound is discrete, it reformulates strict inequalities `<` into the inequality `<=` (dual `>` is handled by `generic_rewrite`).
      Contrarily to `relax`, the rewritten constraint is logically equivalent to the initial constraint.
      For example, it rewrites `x - y < d` into `x - y <= d - 1`. *)
  let reformulate c =
    if B.is_continuous then c
    else
      match c with
      | e1, LT, Cst (d, a) -> normalize (e1, LEQ, Cst (Bound_rat.sub_up d Bound_rat.one, a))
      | c -> c

  (** `x <= d` ~> `x + x <= 2d` *)
  let x_leq_d x c = {v=make_var (x+1) x; d=(B.mul_up c B.two)}

  (** `-x <= d` ~> `-x - x <= 2d` *)
  let minus_x_leq_d x c = {v=make_var x (x+1); d=(B.mul_up c B.two)}

  (* NOTE: we invert `x` and `y` because dbm_var is defined as {x; y} where `x` is the line and `y` the column.
     In an expression such as `X - Y <= 10`, `X` is the column and `Y` the line. *)

  (** `x + y <= d` *)
  let x_plus_y_leq_d x y d = {v=make_var (y+1) x; d=d}

  (** `x - y <= d` *)
  let x_minus_y_leq_d x y d = {v=make_var y x; d=d}

  (** `-x + y <= d` *)
  let minus_x_plus_y_leq_d x y d = {v=make_var (y+1) (x+1); d=d}

  (** `-x - y <= d` *)
  let minus_x_minus_y_leq_d x y d = {v=make_var y (x+1); d=d}

  let map_to_dim r f x d = f ((dim_of_var r x)*2) (B.of_rat_up d)
  let map2_to_dim r f x y d = f ((dim_of_var r x)*2) ((dim_of_var r y)*2) (B.of_rat_up d)

  (* Try creating an octagonal constraint from a normalized form.
     The constraint should be processed by `generic_rewrite` first. *)
  let try_create r = function
    | Var x, LEQ, Cst (d, _) -> Some (map_to_dim r x_leq_d x d)
    | Unary (NEG, Var x), LEQ, Cst (d, _) -> Some (map_to_dim r minus_x_leq_d x d)
    | Binary (Var x, ADD, Var y), LEQ, Cst (d, _) ->  Some (map2_to_dim r x_plus_y_leq_d x y d)
    | Binary (Var x, SUB, Var y), LEQ, Cst (d, _) ->  Some (map2_to_dim r x_minus_y_leq_d x y d)
    | Binary (Unary (NEG, Var x), ADD, Var y), LEQ, Cst (d, _) ->  Some (map2_to_dim r minus_x_plus_y_leq_d x y d)
    | Binary (Unary (NEG, Var x), SUB, Var y), LEQ, Cst (d, _) ->  Some (map2_to_dim r minus_x_minus_y_leq_d x y d)
    | _ -> None

  let unwrap_all constraints =
    if List.for_all Tools.is_some constraints then
      List.map Tools.unwrap constraints
    else []

  let rewrite_atom repr c =
    generic_rewrite c |>
    List.map (fun c -> try_create repr (reformulate c)) |>
    unwrap_all

  let rewrite repr formula =
    try Rewritting.mapfold_conjunction (rewrite_atom repr) formula
    with Wrong_modelling _ -> []

  let relax_atom repr c =
    List.flatten (List.map (fun c ->
      match c with
      | e1, LT, e2 -> rewrite_atom repr (e1, LEQ, e2)
      | _ -> []
    ) (generic_rewrite c))

  let relax repr formula =
    try Rewritting.mapfold_conjunction (relax_atom repr) formula
    with Wrong_modelling _ -> []

  let negate c =
    if is_rotated c.v then
      { v=(Dbm.inv c.v); d=B.neg (B.succ c.d) }
    else
      { v=(Dbm.inv c.v); d=B.mul_up (B.neg (B.succ (B.div_down c.d B.two))) B.two }
end
