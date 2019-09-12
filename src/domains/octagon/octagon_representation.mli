(** This module provides functions to detect and rewrite arbitrary constraint into an equivalent and relaxed octagonal constraints, if possible. *)

open Lang.Ast
open Bounds
open Dbm

module type Octagon_rep_sig =
sig
  module B: Bound_sig.S
  type t
  type var_kind = unit
  type var_id = dbm_interval
  type rconstraint = B.t dbm_constraint

  val empty: t

  (** Initialize the rewriter with the map between variable's name and DBM interval. *)
  val extend: t -> (var * var_id) -> t

  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id

  (** Create octagonal constraints from an initial constraint.
    If the list is empty, it is not possible to rewrite the constraint.
    Multiple elements mean the constraint has been decomposed into several octagonal constraints. *)
  val rewrite: t -> formula -> rconstraint list

  (** Relax the constraint into an octagonal version, if possible.
      The list returned is empty if the constraint cannot be relaxed or is already octagonal.
      For continuous bound, it rewrites strict inequalities `<`,`>` into the inequality `<=`,`>=`.
      For discrete bound, it always returns the empty list. *)
  val relax: t -> formula -> rconstraint list

  (** Negate the constraint.
      It might be a relaxation approximation of the negation for continuous domain. *)
  val negate: rconstraint -> rconstraint
end

module Octagon_rep(B: Bound_sig.S): Octagon_rep_sig with module B=B
