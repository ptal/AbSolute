open Var_store
open Abstract_domain
open Box_representation

module type Box_sig =
sig
  type t
  module I: Vardom_sig.Vardom_sig
  module R = Box_rep
  type itv = I.t
  type bound = I.B.t

  (** Creates an empty box. *)
  val empty: t

  (** Add a variable into the box. *)
  val extend: t -> R.var_kind -> (t * R.var_id)

  (** Projection of a variable according to its location in the box. *)
  val project: t -> R.var_id -> (I.B.t * I.B.t)

  (** `project_itv box v` projects the interval of the variable `v`. *)
  val project_itv: t -> R.var_id -> itv

  (** See `Abstract_domain.lazy_copy`. *)
  val lazy_copy: t -> int -> t list

  (** See `Abstract_domain.copy`. *)
  val copy: t -> t

    (** Closure of the store with regards to all constraints in the box.
      A fixed point is reached when no constraint can be propagated anymore.
      The entailed constraints are removed from the box.
      Throw `Bot_found` one of the constraints is unsatisfiable. *)
  val closure: t -> t

  (** Add the constraint `c` into the box.
      Precondition: The variables in `c` must range over the current store.
      The store of variables is left unchanged, and `closure` should be applied to reduce the box. *)
  val weak_incremental_closure: t -> R.rconstraint -> t

  (** Equivalent to `closure (weak_incremental_closure box c)`. *)
  val incremental_closure: t -> R.rconstraint -> t

  (** Return the entailment status of the constraint in `box`. *)
  val entailment: t -> R.rconstraint -> kleene

  (** See `Abstract_domain.split`. *)
  val split: t -> t list

  (** Compute the volume of the box.
      We return `1.` if the box only contain singleton domain. *)
  val volume: t -> float

  (** Characterize the state of the box.
      `True` is returned when all the constraints are entailed.
      `False` is never returned because `Bot_found` is raised in case of unsatisfiability (in `incremental_closure` or `closure`). *)
  val state_decomposition: t -> kleene

  (** Print the variables store and the remaining constraints (those not yet entailed). *)
  val print: R.t -> Format.formatter -> t -> unit
end

module type Box_functor = functor (B: Bound_sig.BOUND) -> Box_sig with module I.B = B

module Make
  (B: Bound_sig.BOUND)
  (VARDOM: Vardom_sig.Vardom_functor)
  (STORE: Var_store_functor)
  (CLOSURE: Hc4.Box_closure_sig)
  (SPLIT: Box_split.Box_split_sig) : Box_sig

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor
