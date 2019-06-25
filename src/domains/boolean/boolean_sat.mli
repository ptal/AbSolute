module type Boolean_sat_sig =
sig
  type t

  module R = Boolean_rep

  (** Boolean are representable on integers.
      NOTE: We use `Bound_int` instead of introducing a new `Bound_bool`. *)
  module B = Bound_int

  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (B.t * B.t)
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> Kleene.t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> Kleene.t
  val print: R.t -> Format.formatter -> t -> unit
end

module Boolean_sat: Boolean_sat_sig
