(* Copyright 2019 AbSolute Team

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

module type Vardom_of_bounds =
sig
  module B: Bound_sig.S
  type bound = B.t
  type t

  (** Same as `Vardom_sig.top` but with bounds. *)
  val of_bounds: ?ty:Types.var_ty -> (bound * bound) -> t
end

module type Vardom_factory_sig =
sig
  include Vardom_of_bounds

  val of_bound: bound -> t
  val zero : t
  val one : t
  val minus_one: t
  val zero_one: t
  val minus_one_zero: t
  val minus_one_one: t
  val positive: t
  val negative: t

  val of_ints: int -> int -> t
  val of_int: int -> t
  val of_rats: Bound_rat.t -> Bound_rat.t -> t
  val of_rat: Bound_rat.t -> t
  val of_floats: float -> float -> t
  val of_float: float -> t
end

(** This module aims at extending a vardom with conveniency function derived from `of_bounds`.
    The variable type (`var_ty`) passed to `of_bounds` depends on the constructor used.
    For example, `of_ints` calls `of_bounds` with the type `Concrete Int`.
    A rule of thumb is that we always try to build the vardom with the concrete type of the arguments. *)
module Make(V: Vardom_of_bounds) : Vardom_factory_sig
with
  type t = V.t and
  module B=V.B and
  type bound=V.bound
