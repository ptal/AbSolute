open Box_representation

module type Variable_order = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  val select: R.Store.t -> (R.var_id * R.var_dom) option
end with module R=R

module type Value_order = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  val select: R.var_dom -> R.Vardom.B.t
end with module R=R

module type Distributor = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  val distribute: R.var_id -> R.Vardom.B.t -> R.rconstraint list
end with module R=R

module type Box_split_sig = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  val split: R.Store.t -> R.rconstraint list
end with module R=R

module Input_order : Variable_order
module First_fail : Variable_order
module Anti_first_fail : Variable_order

module Middle : Value_order
module Lower_bound : Value_order
module Upper_bound : Value_order

module Assign : Distributor
module Bisect : Distributor

module Make
  (VARIABLE: Variable_order)
  (VALUE: Value_order)
  (DISTRIB: Distributor) : Box_split_sig

module First_fail_bisect : Box_split_sig
module First_fail_LB : Box_split_sig
module Anti_first_fail_LB : Box_split_sig
module Anti_first_fail_UB : Box_split_sig
module MSLF_simple : Box_split_sig
