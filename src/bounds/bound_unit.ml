(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** We view this bound as the single element `()`.
  In a sense, it is the worst over-approximation possible of anything.
  It maps to `0` when needed to convert to other numbers.
  This module is mainly useful for "meta abstract domain" that coordinates other abstract domains, but does not represent element or constraint.
  Rational: The signature of `Abstract_domain` requires a `Bound`. *)

type t = unit

let compare _ _ = 0
let equal _ _ = true
let leq _ _ = true
let geq _ _ = true
let lt _ _ = false
let gt _ _ = false
let neq _ _ = false

let odd _ = false
let even _ = true

let min x _ = x
let max x _ = x

let sign _ = 0

let is_continuous = false

let succ x = x
let prec x = x

let of_int_up _ = ()
let of_int_down _ = ()
let of_float_up _ = ()
let of_float_down _ = ()
let of_rat_up _ = ()
let of_rat_down _ = ()

let of_string_up _ = ()
let of_string_down _ = ()

(* printing *)
(* ******** *)

let to_string _ = "()"
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)

(* conversion *)
(* ********** *)

let to_float_up _ = 0.
let to_float_down _ = 0.
let to_rat _ = Bound_rat.zero
let to_int_up _ = 0
let to_int_down _ = 0

(* classification *)
(* ************** *)

type kind =
  | FINITE      (* finite number *)
  | MINF | INF  (* -oo or +oo *)
  | INVALID     (* for NaN and other inletid numbers *)

let classify _ = FINITE

(* useful constants *)
(* **************** *)

let zero = ()
let one = ()
let two = ()
let minus_one = ()
let inf = ()
let minus_inf = ()


(* operators *)
(* ********* *)

(* exact operators *)
let neg _ = ()
let abs _ = ()

(* operators with rounding *)
let add_up _ _ = ()
let sub_up _ _ = ()
let mul_up _ _ = ()
let div_up _ _ = ()
let add_down _ _ = ()
let sub_down _ _ = ()
let mul_down _ _ = ()
let div_down _ _ = ()

let bound_mul _ _ _ = ()
let bound_div _ _ _ = ()

let sqrt_up _ = ()
let sqrt_down _ = ()

let pow_up _ _ = ()
let pow_down _ _ = ()
let root_up _ _ = ()
let root_down _ _ = ()

let cos_up _ = ()
let cos_down _ = ()
let sin_up _ = ()
let sin_down _ = ()
let tan_up _ = ()
let tan_down _ = ()

let acos_up _ = ()
let acos_down _ = ()
let asin_up _ = ()
let asin_down _ = ()
let atan_up _ = ()
let atan_down _ = ()

let exp_up _ = ()
let exp_down _ = ()
let ln_up _ = ()
let ln_down _ = ()
let log_up _ = ()
let log_down _ = ()

(* integer rounding *)
let floor _ = ()
let ceil _ = ()
