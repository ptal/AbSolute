open Bounds
open Octagon
open Octagon.Dbm
open Lang.Ast
open Lang.Rewritting

module Rewriter = Octagon_representation.Octagon_rep(Bound_int)

let x_i = (as_interval {l=0;c=1})
let xy_i = (as_interval {l=2;c=0})

let vars = [("x", (as_interval {l=0;c=1}));
            ("y", (as_interval {l=2;c=3}));
            ("xy", (as_interval {l=2;c=0}))]

let ten = Cst (Bound_rat.of_int 10, Int)

let init_rewriter vars = List.fold_left Rewriter.extend Rewriter.empty vars

let test_init () =
  let r = init_rewriter vars in
  begin
    Alcotest.(check bool) "init" true ((compare x_i (Rewriter.to_abstract_var r "x")) = 0);
    Alcotest.(check bool) "init" true ((compare xy_i (Rewriter.to_abstract_var r "xy")) = 0);
    Alcotest.(check bool) "init" true ((compare "x" (Rewriter.to_logic_var r x_i)) = 0);
    Alcotest.(check bool) "init" true ((compare "xy" (Rewriter.to_logic_var r xy_i)) = 0);
  end

let check_dbm_constraint' name expected obtained =
begin
  Alcotest.(check int) (name ^ "-length") (List.length obtained) (List.length expected);
  List.iter2 (fun obtained expected ->
    Printf.printf "interval obtained=%s / expected=%s\n" (Test_octagon.string_of_constraint obtained Rewriter.B.to_string) (Test_octagon.string_of_constraint expected Rewriter.B.to_string);
    Alcotest.(check bool) (name ^ "-interval") true ((compare obtained.v expected.v) = 0);
    Alcotest.(check bool) (name ^ "-constant") true (Rewriter.B.equal obtained.d expected.d);
  ) obtained expected
end

let check_dbm_constraint name r c expected =
begin
  let obtained = Rewriter.rewrite r c in
  check_dbm_constraint' name expected obtained
end

let check_neg_dbm_constraint name r c expected =
begin
  check_dbm_constraint ("negate bconstraint " ^ name) r (neg_bconstraint c) expected;
  let obtained = Rewriter.rewrite r c in
  Alcotest.(check int) (name ^ "-singleton") (List.length obtained) 1;
  let negate_obtained = List.map Rewriter.negate obtained in
  check_dbm_constraint' ("negate dbm_constraint " ^ name) expected negate_obtained;
end

let test_rewrite () =
  let r = init_rewriter vars in
  begin
    check_dbm_constraint "rewrite x <= 10" r (Var "x", LEQ, ten) [{v={l=1;c=0}; d=(Rewriter.B.of_int_up 20)}];
    check_dbm_constraint "rewrite x >= 10" r (Var "x", GEQ, ten) [{v={l=0;c=1}; d=(Rewriter.B.of_int_up (-20))}];
    check_dbm_constraint "rewrite x < 10" r (Var "x", LT, ten) [{v={l=1;c=0}; d=(Rewriter.B.of_int_up 18)}];
    check_dbm_constraint "rewrite x > 10" r (Var "x", GT, ten) [{v={l=0;c=1}; d=(Rewriter.B.of_int_up (-22))}];
    check_dbm_constraint "rewrite x - y <= 10" r (Binary (Var "x", SUB, Var "y"), LEQ, ten) [{v={l=2;c=0}; d=(Rewriter.B.of_int_up 10)}];
    check_dbm_constraint "rewrite x - y > 10" r (Binary (Var "x", SUB, Var "y"), GT, ten) [{v={l=3;c=1}; d=Rewriter.B.of_int_up (-11)}];
    check_dbm_constraint "rewrite -x - y < 10" r (Binary (Unary (NEG, Var "x"), SUB, Var "y"), LT, ten) [{v={l=2;c=1}; d=Rewriter.B.of_int_up 9}];
    check_dbm_constraint "rewrite -x + y <= 10" r (Binary (Unary (NEG, Var "x"), SUB, Unary (NEG, Var "y")), LEQ, ten) [{v={l=3;c=1}; d=Rewriter.B.of_int_up 10}];
    check_dbm_constraint "rewrite x + y <= 10" r (Binary (Var "x", SUB, Unary (NEG, Var "y")), LEQ, ten) [{v={l=3;c=0}; d=Rewriter.B.of_int_up 10}];
  end

let test_negate () =
  let r = init_rewriter vars in
  begin
    check_neg_dbm_constraint "x <= 10" r (Var "x", LEQ, ten) [{v={l=0;c=1}; d=(Rewriter.B.of_int_up (-22))}];
    check_neg_dbm_constraint "x >= 10" r (Var "x", GEQ, ten) [{v={l=1;c=0}; d=(Rewriter.B.of_int_up 18)}];
    check_neg_dbm_constraint "x - y <= 10" r (Binary (Var "x", SUB, Var "y"), LEQ, ten) [{v={l=3;c=1}; d=(Rewriter.B.of_int_up (-11))}];
    check_neg_dbm_constraint "-x - y < 10" r (Binary (Unary (NEG, Var "x"), SUB, Var "y"), LT, ten) [{v={l=3;c=0}; d=Rewriter.B.of_int_up (-10)}];
    check_neg_dbm_constraint "-x + y <= 10" r (Binary (Unary (NEG, Var "x"), SUB, Unary (NEG, Var "y")), LEQ, ten) [{v={l=2;c=0}; d=Rewriter.B.of_int_up (-11)}];
    check_neg_dbm_constraint "x + y <= 10" r (Binary (Var "x", SUB, Unary (NEG, Var "y")), LEQ, ten) [{v={l=2;c=1}; d=Rewriter.B.of_int_up (-11)}];
  end

let tests = [
  "init", `Quick, test_init;
  "rewrite", `Quick, test_rewrite;
  "negate", `Quick, test_negate;
]
