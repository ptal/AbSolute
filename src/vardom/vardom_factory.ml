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

module Make(V: Vardom_of_bounds) =
struct
  include V

  let of_bound x = of_bounds (x,x)
  let zero = of_bound B.zero
  let one = of_bound B.one
  let minus_one = of_bound B.minus_one
  let zero_one = of_bounds (B.zero, B.one)
  let minus_one_zero = of_bounds (B.minus_one, B.zero)
  let minus_one_one = of_bounds (B.minus_one, B.one)
  let positive = of_bounds (B.zero, B.inf)
  let negative = of_bounds (B.minus_inf, B.zero)

  let of_ints l h = of_bounds ~ty:Types.(Concrete Int) ((B.of_int_down l),(B.of_int_up h))
  let of_int x = of_ints x x
  let of_rats l h = of_bounds ~ty:Types.(Concrete Real) ((B.of_rat_down l),(B.of_rat_up h))
  let of_rat x = of_rats x x
  let of_floats l h = of_bounds ~ty:Types.(Concrete Real) ((B.of_float_down l),(B.of_float_up h))
  let of_float x = of_floats x x
end
