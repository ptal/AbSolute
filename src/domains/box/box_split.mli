open Var_store
open Box_representation

module type Variable_order = functor (S: Var_store_sig) ->
sig
  module Store : Var_store_sig
  val select: Store.t -> (Store.key * Store.cell) option
end with module Store=S

module type Value_order = functor (I: Vardom_sig.Vardom_sig) ->
sig
  module I: Vardom_sig.Vardom_sig
  val select: I.t -> box_expr
end with module I=I

module type Distributor =
sig
  val distribute: box_var -> box_expr -> box_constraint list
end

module type Box_split_sig = functor (S: Var_store_sig) ->
sig
  module Store : Var_store_sig
  val split: Store.t -> box_constraint list
end with module Store=S

module Input_order : Variable_order
module First_fail : Variable_order
module Anti_first_fail : Variable_order

module Middle : Value_order
module Lower_bound : Value_order
module Upper_bound : Value_order

module Assign : Distributor
module Bisect : Distributor

module Make
  (Variable: Variable_order)
  (Value: Value_order)
  (Distrib: Distributor) : Box_split_sig

module First_fail_bisect : Box_split_sig
module First_fail_LB : Box_split_sig
module Anti_first_fail_LB : Box_split_sig
module Anti_first_fail_UB : Box_split_sig
module MSLF_simple : Box_split_sig
