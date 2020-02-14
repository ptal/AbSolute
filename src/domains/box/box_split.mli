(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)


open Box_interpretation

module type Variable_order = functor (R: Box_interpretation_sig) ->
sig
  module R: Box_interpretation_sig
  type t = R.var_id list
  val select: t -> R.Store.t -> t * (R.var_id * R.var_dom) option
end with module R=R

module type Value_order = functor (R: Box_interpretation_sig) ->
sig
  module R: Box_interpretation_sig
  val select: R.var_dom -> R.Vardom.B.t
end with module R=R

module type Distributor = functor (R: Box_interpretation_sig) ->
sig
  module R: Box_interpretation_sig
  val distribute: R.t -> R.var_id -> R.Vardom.t -> R.rconstraint list
end with module R=R

module type Box_split_sig = functor (R: Box_interpretation_sig) ->
sig
  type t = R.var_id list
  module R: Box_interpretation_sig
  val split: t -> R.t -> R.Store.t -> t * R.rconstraint list
end with module R=R

module Input_order : Variable_order
module First_fail : Variable_order
module Anti_first_fail : Variable_order

module Middle : Value_order
module Lower_bound : Value_order
module Upper_bound : Value_order

module Assign : Distributor

(** X <= m \/ X > m *)
module Bisect : Distributor

(** X >= m \/ X < m *)
module ReverseBisect : Distributor

module Make
  (VARIABLE: Variable_order)
  (VALUE: Value_order)
  (DISTRIB: Distributor) : Box_split_sig

module First_fail_bisect : Box_split_sig
module First_fail_LB : Box_split_sig
module Anti_first_fail_LB : Box_split_sig
module Anti_first_fail_UB : Box_split_sig
module MSLF_simple : Box_split_sig
