open Box_representation

module type Box_closure_sig = functor (R: Box_rep_sig) ->
sig
  module R : Box_rep_sig
  module Store = R.Store

  val incremental_closure: Store.t -> R.rconstraint -> Store.t
  val entailment: Store.t -> R.rconstraint -> Kleene.t

  (** Perform the filtering of the constraint.
      Returns `(store, b)` where `store` is the resulting store and `b` is true if the constraint is entailed. *)
  val incremental_closure: Store.t -> R.rconstraint -> (Store.t * bool)
  val entailment: Store.t -> R.rconstraint -> Kleene.t

end with module R=R

module Make : Box_closure_sig
