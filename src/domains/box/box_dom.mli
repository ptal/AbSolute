open Box_representation

module type Box_sig =
sig
  type t
  module Vardom: Vardom_sig.Vardom_sig
  module R : Box_rep_sig with module Vardom = Vardom
  type vardom = Vardom.t
  type bound = Vardom.B.t

  (** Creates an empty box. *)
  val empty: t

  (** Add a variable into the box. *)
  val extend: t -> R.var_kind -> (t * R.var_id)

  (** Projection of a variable according to its location in the box. *)
  val project: t -> R.var_id -> (Vardom.B.t * Vardom.B.t)

  (** `project_vardom box v` projects the interval of the variable `v`. *)
  val project_vardom: t -> R.var_id -> vardom

  (** See `Abstract_domain.lazy_copy`. *)
  val lazy_copy: t -> int -> t list

  (** See `Abstract_domain.copy`. *)
  val copy: t -> t

    (** Closure of the store with regards to all constraints in the box.
      A fixed point is reached when no constraint can be propagated anymore.
      The entailed constraints are removed from the box.
      Throw `Bot_found` one of the constraints is unsatisfiable. *)
  val closure: t -> t

  (** Propagated and add the constraint `c` into the box.
      Precondition: The variables in `c` must range over the current store. *)
  val weak_incremental_closure: t -> R.rconstraint -> t

  (** Return the entailment status of the constraint in `box`. *)
  val entailment: t -> R.rconstraint -> Kleene.t

  (** See `Abstract_domain.split`. *)
  val split: t -> t list

  (** Compute the volume of the box.
      We return `1.` if the box only contain singleton domain. *)
  val volume: t -> float

  (** Characterize the state of the box.
      `True` is returned when all the constraints are entailed.
      `False` is never returned because `Bot_found` is raised in case of unsatisfiability (in `incremental_closure` or `closure`). *)
  val state_decomposition: t -> Kleene.t

  (** Print the variables store and the remaining constraints (those not yet entailed). *)
  val print: R.t -> Format.formatter -> t -> unit

  (** See `Var_store.delta`.
      It returns the variables modified since last call. *)
  val delta: t -> R.var_id list
end

module type Box_functor = functor (B: Bound_sig.BOUND) -> Box_sig with module Vardom.B = B

module Make
  (B: Bound_sig.BOUND)
  (VARDOM: Vardom_sig.Vardom_functor)
  (SPLIT: Box_split.Box_split_sig) : Box_sig

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor
