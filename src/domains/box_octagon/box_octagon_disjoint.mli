open Csp
open Octagon
open Box_dom
open Abstract_domain
open Box_representation

module type Box_oct_rep_sig = functor (Oct_rep: Representation_sig) ->
sig
  type t = {
    box_rep: Box_rep.t;
    oct_rep: Oct_rep.t;
  }
  type var_kind = BoxKind of Box_rep.var_kind | OctKind of Oct_rep.var_kind
  type var_id = BoxVar of Box_rep.var_id | OctVar of Oct_rep.var_id
  type reified_octagonal = Box_rep.var_id * Oct_rep.rconstraint list
  type rconstraint =
    BoxConstraint of Box_rep.rconstraint
  | OctConstraint of Oct_rep.rconstraint
  | ReifiedConstraint of reified_octagonal

  val empty: t
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> bformula -> rconstraint list
  (* This is a temporary function. We should generalized Representation_sig to formula rather than only constraint. *)
  val rewrite_reified: t -> var -> bconstraint list -> rconstraint list
  val relax: t -> bformula -> rconstraint list
  val negate: rconstraint -> rconstraint
end

module Box_oct_rep: Box_oct_rep_sig

module type Box_octagon_disjoint_sig =
sig
  module B : Bound_sig.BOUND
  module R : Representation_sig
  type t
  type bound = B.t
  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (B.t * B.t)
  val lazy_copy: t -> int -> t list
  val copy: t -> t

  (** This closure filters the box and octagon with regards to the (reified) constraints in `box_oct`.
      Besides reducing the domain of the variables, the entailed constraints are removed from `box_oct`. *)
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> Kleene.t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> Kleene.t
  val print: R.t -> Format.formatter -> t -> unit
end

module Make
  (BOX: Box_functor)
  (Octagon: Octagon_sig) : Box_octagon_disjoint_sig with module R = Box_oct_rep(Octagon.R)
