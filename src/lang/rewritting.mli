(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Ast

(** Every atoms `a` in `expr` is transformed to `a binop expr'`. *)
val distribute: (binop * expr) -> expr -> expr

(** Propagate constants in expression. *)
val expand: expr -> expr

(** Simplify elementary functions. *)
val simplify_fp: expr -> expr

val simplify_formula: formula -> formula

val left_hand_side: (expr * cmpop * expr) -> (cmpop * expr)
val left_hand: formula -> (cmpop * expr)
val is_arith: formula -> bool

(** Iterate on expression. *)
val iter_expr: (expr -> unit) -> expr -> unit

(** Iterate on expressions and all subformulas. *)
val iter_subformula: (expr -> unit) -> (formula -> unit) -> formula -> unit

(** Map constraints of a Boolean formula. *)
val map_constraint: (bconstraint -> bconstraint) -> formula -> formula

(** Inversion of the comparison operator.
    For example, <= becomes >=.
    See also `neg`. *)
val inv: cmpop -> cmpop

(** Negate the comparison operator. *)
val neg: cmpop -> cmpop

(** Negate a logic formula. *)
val neg_formula: formula -> formula
val neg_bconstraint: bconstraint -> bconstraint

val replace_cst_expr: (var * i) -> expr -> expr
val replace_cst_formula: (var * i) -> formula -> formula

module Variables: Set.S with type elt = var

val get_vars_expr: expr -> var list
val get_vars_set_expr: expr -> Variables.t
val get_vars_formula: formula -> var list
val get_vars_set_formula: formula -> Variables.t
val vars_of_bconstraint: bconstraint -> var list

(* True if the constraint is fully defined over the set of variables `vars`. *)
val is_defined_over: var list -> bconstraint -> bool

val from_cst_to_expr: (var * (i * i)) -> bconstraint list
val csts_to_expr: (var * (i * i)) list -> bconstraint list

val replace_var_in_expr: (var -> expr) -> expr -> expr

(** Traverse the formula `f` and raise `Wrong_modelling` if we meet a formula containing something else than conjunctions or predicates. *)
val mapfold_conjunction: (bconstraint -> 'a list) -> formula -> 'a list

(* [quantify env f] Given a variable environment `env`, existentially quantify the formula `f`.
   It adds `Exists` in front of `f` for each variable occuring in `f`. *)
val quantify: (var * Types.var_ty) list -> formula -> qformula

val conjunction: formula list -> formula
val qf_conjunction: qformula -> qformula -> qformula
