(* open Abstract_domain
open Box_dom

(* This module equips `Box_dom` with reified constraints. *)

type box_reified_constraint = Csp.var * Csp.bconstraint list

module type Box_reified_sig =
sig
  type t
  module I: Itv_sig.ITV
  module B = I.B
  module R = Logical_representation
  type bound = B.t
  type itv = I.t

  val init: Csp.var list -> Csp.bconstraint list -> box_reified_constraint list -> t
  val get: t -> R.rvar -> itv
  val project_one: t -> R.rvar -> (bound * bound)
  val project: t -> R.rvar list -> (R.rvar * (bound * bound)) list
  val weak_incremental_closure: t -> R.rconstraint -> t
  val closure: t -> t
  val incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> kleene
  val volume: t -> float
  val state_decomposition: t -> kleene
  val print: Format.formatter -> t -> unit
  val split: t -> t list
end

module Make(Box: Box_sig) : Box_reified_sig

module BoxReifiedZ(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
module BoxReifiedQ(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
module BoxReifiedF(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
 *)