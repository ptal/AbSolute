open Var_store
open Box_representation

module type Box_closure_sig = functor (S: Var_store_sig) ->
sig
  module Store : Var_store_sig
  val incremental_closure: Store.t -> box_constraint -> Store.t
  val entailment: Store.t -> box_constraint -> Kleene.t
end with module Store=S

module Make : Box_closure_sig
