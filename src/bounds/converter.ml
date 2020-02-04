(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core

module Converter(B1: Bound_sig.S)(B2: Bound_sig.S) =
struct
  let[@inline always] convert_up b =
    let open Types in
    match B1.type_of, B2.type_of with
    | F, F -> B2.of_float_up (B1.to_float_up b)
    | Z, Z -> B2.of_int_up (B1.to_int_up b)
    | Q, Q -> B2.of_rat_up (B1.to_rat b)
    | F, Z -> B2.of_float_up (B1.to_float_up b)
    | F, Q -> B2.of_float_up (B1.to_float_up b)
    | Z, F -> B2.of_int_up (B1.to_int_up b)
    | Z, Q -> B2.of_int_up (B1.to_int_up b)
    | Q, F -> B2.of_rat_up (B1.to_rat b)
    | Q, Z -> B2.of_rat_up (B1.to_rat b)

  let[@inline always] convert_down b =
    let open Types in
    match B1.type_of, B2.type_of with
    | F, F -> B2.of_float_up (B1.to_float_up b)
    | Z, Z -> B2.of_int_up (B1.to_int_up b)
    | Q, Q -> B2.of_rat_up (B1.to_rat b)
    | F, Z -> B2.of_float_down (B1.to_float_up b)
    | F, Q -> B2.of_float_down (B1.to_float_up b)
    | Z, F -> B2.of_int_down (B1.to_int_up b)
    | Z, Q -> B2.of_int_down (B1.to_int_up b)
    | Q, F -> B2.of_rat_down (B1.to_rat b)
    | Q, Z -> B2.of_rat_down (B1.to_rat b)

end [@@inline always]