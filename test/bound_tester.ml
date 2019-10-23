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

module Make(B:Bound_sig.S) =
struct
  let epsilon = B.of_float_down 0.000000001

  (* Note that for integer bounds, we expect the exact bound (epsilon = 0). *)
  let test_bound_delta name expected obtained =
    if B.neq expected obtained then
      let delta = B.abs (B.sub_up expected obtained) in
      let name = name ^ ".(epsilon: " ^ (B.to_string epsilon) ^ ", delta: " ^ (B.to_string delta) ^ ")" in
      Alcotest.(check bool) name true (B.leq delta epsilon)

  (* We test that the `obtained` bound is less or greater than (depending on `cmp`) the `expected` bound.
     We allow some rounding errors to occur, but they must be in the right direction (thus we do not lose potential solutions).
     In addition, we test that the delta between the bounds is not greater than `epsilon`. *)
  let expect_bound fun_name cmp expected obtained =
    let name = fun_name ^ " (expected: " ^ (B.to_string expected) ^ ", obtained: " ^ (B.to_string obtained) ^ ")" in
    Alcotest.(check bool) name true (cmp expected obtained);
    test_bound_delta name expected obtained

  let expect_ge fun_name = expect_bound (fun_name ^ ".expect_ge") (B.geq)
  let expect_le fun_name = expect_bound (fun_name ^ ".expect_le") (B.leq)
end