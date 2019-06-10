open Var_store
open Abstract_domain
open Box_representation

module type Box_closure_sig = functor (S: Var_store_sig) ->
sig
  module Store : Var_store_sig

  (** Perform the filtering of the constraint.
      Returns `(store, b)` where `store is the resulting store and `b` is true if the constraint is entailed. *)
  val incremental_closure: Store.t -> box_constraint -> (Store.t * bool)
  val entailment: Store.t -> box_constraint -> kleene
end with module Store=S

module Make : Box_closure_sig
