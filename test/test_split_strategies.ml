(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Test_octagon
open Bounds
open Octagon

module OctagonZ = Test_octagon.OctagonZ
module I = OctagonZ.I
module Middle = Octagon_split.Middle(OctagonZ.DBM)
module MinMax = Octagon_split.Min_max(OctagonZ.DBM)

let init_rewriter vars = List.fold_left I.extend (I.empty ()) vars

let test_middle () =
  let octagon = octagon_empty2D in
  let constraints = octagon_2D in
  let octagon = List.fold_left OctagonZ.weak_incremental_closure octagon constraints in
  let (octagon,_) = OctagonZ.closure octagon in
  let r = init_rewriter Test_rewriter.vars in
  begin
    (* `x` is between [1..5] ([2..10] as DBM.project). Middle should be `6`. *)
    let middle = Middle.select (OctagonZ.unwrap octagon) (Dbm.as_interval Test_rewriter.x_i) in
    Alcotest.(check bool) ("middle of [2..10] is " ^ (Z.to_string middle)) true (Z.equal middle (Z.of_int_up 6));
    let (octagon,_) = OctagonZ.incremental_closure octagon (List.hd (snd (I.interpret r Exact (Cmp (Var "x", LEQ, Cst (Bound_rat.of_int 2, Int)))))) in
    let middle = Middle.select (OctagonZ.unwrap octagon) (Dbm.as_interval Test_rewriter.x_i) in
    Alcotest.(check bool) ("middle of [2..4] is " ^ (Z.to_string middle)) true (Z.equal middle (Z.of_int_up 3));
    let (octagon,_) = OctagonZ.incremental_closure octagon (List.hd (snd (I.interpret r Exact (Cmp (Var "x", LEQ, Cst (Bound_rat.of_int 1, Int)))))) in
    let middle = Middle.select (OctagonZ.unwrap octagon) (Dbm.as_interval Test_rewriter.x_i) in
    Alcotest.(check bool) ("middle of [2..2] is " ^ (Z.to_string middle)) true (Z.equal middle (Z.of_int_up 2));
  end

module R = Bound_rat
let test_rat () =
  let r1 = R.inf in
  let r2 = R.of_int 0 in
  begin
    Alcotest.(check bool) "Rat.lt" true (R.lt r2 r1);
  end

let tests = [
  "middle", `Quick, test_middle;
  "rat", `Quick, test_rat;
]
