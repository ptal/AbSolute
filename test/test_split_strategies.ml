open Test_octagon
open Bounds
open Octagon

module OctagonZ = Test_octagon.OctagonZ
module Rewriter = OctagonZ.R
module Middle = Octagon_split.Middle(OctagonZ.DBM)
module MinMax = Octagon_split.Min_max(OctagonZ.DBM)

let init_rewriter vars = List.fold_left Rewriter.extend Rewriter.empty vars

let test_middle () =
  let octagon = octagon_empty2D in
  let constraints = octagon_2D in
  let octagon = List.fold_left OctagonZ.weak_incremental_closure octagon constraints in
  let octagon = OctagonZ.closure octagon in
  let r = init_rewriter Test_rewriter.vars in
  begin
    (* `x` is between [1..5] ([2..10] as DBM.project). Middle should be `6`. *)
    let middle = Middle.select (OctagonZ.unwrap octagon) Test_rewriter.x_i in
    Alcotest.(check bool) ("middle of [2..10] is " ^ (Z.to_string middle)) true (Z.equal middle (Z.of_int_up 6));
    let octagon = OctagonZ.incremental_closure octagon (List.hd (Rewriter.rewrite r (Var "x", LEQ, Cst (Bound_rat.of_int 2, Int)))) in
    let middle = Middle.select (OctagonZ.unwrap octagon) Test_rewriter.x_i in
    Alcotest.(check bool) ("middle of [2..4] is " ^ (Z.to_string middle)) true (Z.equal middle (Z.of_int_up 3));
    let octagon = OctagonZ.incremental_closure octagon (List.hd (Rewriter.rewrite r (Var "x", LEQ, Cst (Bound_rat.of_int 1, Int)))) in
    let middle = Middle.select (OctagonZ.unwrap octagon) Test_rewriter.x_i in
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
