(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Bounds
open Vardom
open Dbm

module type Interval_view_sig = functor (B: Bound_sig.S) ->
sig
  module B: Bound_sig.S
  type bound = B.t

  val dbm_to_lb : dbm_var -> bound -> bound
  val dbm_to_ub : dbm_var -> bound -> bound
  val dbm_to_itv: dbm_interval -> (bound * bound) -> (bound * bound)
  val lb_to_dbm : dbm_var -> bound -> bound
  val ub_to_dbm : dbm_var -> bound -> bound
end with module B=B

(* Rules for coping with rounding when transferring from DBM to BOX:
  * From BOX to DBM: every number is rounded UP because these numbers only decrease during the Floyd Warshall algorithm.
  * From DBM to BOX: the number is rounded DOWN for lower bound and UP for upper bound.

  To simplify the treatment (and improve soundness), we use interval arithmetic: (sqrt 2) is interpreted as the interval [sqrt_down 2, sqrt_up 2].
  Further operations are performed on this interval, and we chose the lower or upper bound at the end depending on what we need. *)
module Interval_view(B: Bound_sig.S) =
struct
  module B=B
  module R=Bound_rat
  type bound = B.t

  module I = Trigo.Make(Itv.Itv(R))

  let wrap of_rat f' v b = of_rat (f' v (B.to_rat b))

  let lb_rounding = if Types.is_continuous B.abstract_ty then B.of_rat_down else B.of_rat_up
  let ub_rounding = if Types.is_continuous B.abstract_ty then B.of_rat_up else B.of_rat_down

  let sqrt2_it = I.of_rats (R.sqrt_down R.two) (R.sqrt_up R.two)

  (* NOTE: We do not negate "2" in the division because the lower bound is already negated in DBM.project. *)
  let dbm_to_lb = wrap lb_rounding (fun v b ->
    if is_rotated v then
      I.lb (Bot.nobot (I.div (I.of_rat b) sqrt2_it))
    else
      R.div b R.two)

  let dbm_to_ub = wrap ub_rounding (fun v b ->
    if is_rotated v then
      I.ub (Bot.nobot (I.div (I.of_rat b) sqrt2_it))
    else
      R.div b R.two)

  let dbm_to_itv itv (lb,ub) = dbm_to_lb itv.lb lb, dbm_to_ub itv.ub ub

  let lb_to_dbm = wrap B.of_rat_up (fun v b ->
    if is_rotated v then
      I.ub (I.binop MUL (I.of_rat b) sqrt2_it)
    else
      R.mul b R.two)

  let ub_to_dbm = wrap B.of_rat_up (fun v b ->
    if is_rotated v then
      I.ub (I.binop MUL (I.of_rat b) sqrt2_it)
    else
      R.mul b R.two)
end
