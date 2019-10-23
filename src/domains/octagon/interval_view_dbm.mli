(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Bounds
open Dbm

(** An octagon is a n-dimensional geometrical shape represented by the intersection of n n-dimensional boxes.
    It equips the DBM representation with functions to view elements in the DBM as intervals. *)

(** This module signature provides function to view DBM's values as interval bounds.
    Rational: Conversion of an element in the DBM to an interval depends on the bound type. *)
module type Interval_view_sig = functor (B: Bound_sig.S) ->
sig
  module B: Bound_sig.S
  type bound = B.t

  (** The bounds of the variable must be retrieved with `DBM.project`. *)
  val dbm_to_lb : dbm_var -> bound -> bound
  val dbm_to_ub : dbm_var -> bound -> bound
  val dbm_to_itv: dbm_interval -> (bound * bound) -> (bound * bound)
  val lb_to_dbm : dbm_var -> bound -> bound
  val ub_to_dbm : dbm_var -> bound -> bound
end with module B=B

module Interval_view : Interval_view_sig
