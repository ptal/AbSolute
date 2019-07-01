open Var_store
open Box_representation

module type Box_closure_sig = functor (S: Var_store_sig)(R: Representation_sig) ->
sig
  module Store : Var_store_sig
  val incremental_closure: Store.t -> R.rconstraint -> Store.t
  val entailment: Store.t -> R.rconstraint -> Kleene.t

  (** Perform the filtering of the constraint.
      Returns `(store, b)` where `store is the resulting store and `b` is true if the constraint is entailed. *)
  val incremental_closure: Store.t -> R.rconstraint -> (Store.t * bool)
  val entailment: Store.t -> R.rconstraint -> Kleene.t

end with module Store=S

module Make : Box_closure_sig
