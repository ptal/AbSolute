(** This module equips `Box_dom` with reified constraints. *)

open Abstract_domain
open Box_dom
open Box_representation
open Csp

type rbox_constraint =
  | BoxConstraint of box_constraint
  | ReifiedConstraint of box_reified_constraint
and box_reified_constraint = box_var * rbox_constraint list

module type Reified_box_rep_sig =
sig
  type t
  type var_kind = unit
  type var_id = box_var
  type rconstraint = rbox_constraint

  val empty: t
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> bconstraint -> rconstraint list
  (* This is a temporary function. We should generalized Representation_sig to formula rather than only constraint. *)
  val rewrite_reified: t -> var -> bconstraint list -> rconstraint list
  val relax: t -> bconstraint -> rconstraint list
  val negate: rconstraint -> rconstraint
end

module Reified_box_rep: Reified_box_rep_sig

module type Box_reified_sig =
sig
  type t
  module I: Vardom_sig.Vardom_sig
  module B = I.B
  module R = Reified_box_rep
  type bound = B.t
  type itv = I.t

  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (I.B.t * I.B.t)
  val project_itv: t -> R.var_id -> itv
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> kleene
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> kleene
  val print: R.t -> Format.formatter -> t -> unit
end

module Make(Box: Box_sig) : Box_reified_sig

module BoxReifiedZ(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
module BoxReifiedQ(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
module BoxReifiedF(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
