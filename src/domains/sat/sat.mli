(* Copyright 2019 Pierre Talbot, Albin Coquereau

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module encapsulates a CDCL SAT solver based on MiniSat, and implemented by Albin Coquereau in OCaml in a library called Minisatml.
    WARNING: only one instance of this module can be used at once, this is because Minisatml relies on global variables. *)

open Bounds
open Core
open Sat_rep

module type Sat_sig =
sig
  type t

  module R = Sat_rep

  (** Boolean are representable on integers.
      NOTE: We use `Bound_int` instead of introducing a new `Bound_bool`. *)
  module B = Bound_int

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

module Sat: Sat_sig
