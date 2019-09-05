open Core
open Lang
open Bounds
open Box
open Box.Box_dom

(* I. Data *)

let x = Ast.Var "x"
let y = Ast.Var "y"

let constraints_Z =
  let open Ast in
  let constraints =
    [(x, GEQ); (* x >= -1 *)
     (x, LEQ); (* x <= 5 *)
     (y, GEQ); (* y >= 0 *)
     (y, LEQ); (* y <= 5 *)
     (Binary (x, ADD, y), LEQ)] (* x + y <= 3 *)
  in
  List.map2 (fun (l,op) r -> (l, op, Cst (Bound_rat.of_int r, Int)))
    constraints
    [-1; 5; 0; 5; 3]

let x_eq_one = Ast.(x, EQ, Cst (Bound_rat.one, Int))

(* II. Tests utilities *)

module B = Bound_int
module BoxFFB = Box_base(Box_split.First_fail_bisect)(B)

module type Box_maker_sig =
sig
  module Box: Box_sig
  type t = {
    box: Box.t;
    repr: Box.R.t
  }
  val empty: t
  val init_vars: t -> Ast.var list -> t
  val init_constraints: t -> Ast.bconstraint list -> t
  val init: Ast.var list -> Ast.bconstraint list -> t
end

module Box_maker(Box: Box_sig) =
struct
  module Box = Box
  module R = Box.R
  type t = {
    box: Box.t;
    repr: R.t
  }

  let empty = { box=Box.empty; repr=R.empty }

  let init_vars br vars =
    let var_kinds = List.init (List.length vars) (fun _ -> ()) in
    let (box, vars_idx) = Tools.fold_map Box.extend br.box var_kinds in
    let repr = List.fold_left R.extend br.repr (List.combine vars vars_idx) in
    { box; repr }

  let init_constraints br constraints =
    let constraints =
        List.map (fun c -> Ast.Cmp c) constraints
     |> List.map (R.rewrite br.repr)
     |> List.flatten in
    let box = List.fold_left Box.weak_incremental_closure br.box constraints in
    { br with box }

  let init vars constraints =
    let br = init_vars empty vars in
    init_constraints br constraints
end

module type Bound_tester_sig =
sig
  module BR: Box_maker_sig
  val expect_bound_eq: string -> string -> int -> int -> unit
  val expect_domain_eq: BR.t -> (string * int * int) list -> unit
end

module Bound_tester(BR: Box_maker_sig) =
struct
  module BR = BR
  module Box = BR.Box
  module R = BR.Box.R

  let expect_bound_eq name var expected obtained =
    let name = name ^ " bound of `" ^ var ^ "`" in
    Alcotest.(check int) name expected obtained

  let expect_domain_eq br expected =
    List.iter (fun (var, lb, ub) ->
      let var_idx = R.to_abstract_var br.BR.repr var in
      let (lb', ub') = Box.project br.box var_idx in
      begin
        expect_bound_eq "lower" var lb (Box.Vardom.B.to_int_down lb');
        expect_bound_eq "upper" var ub (Box.Vardom.B.to_int_up ub')
      end)
    expected
end

(* III. Tests *)

let test_Z () =
  let (module BR : Box_maker_sig) = (module Box_maker(BoxFFB)) in
  let (module BT : Bound_tester_sig) = (module Bound_tester(BR)) in
  let br = BT.BR.init_vars BT.BR.empty ["x"; "y"] in
  begin
    BT.expect_domain_eq br [("x", B.minus_inf, B.inf); ("y", B.minus_inf, B.inf)];
    let br = BT.BR.init_constraints br constraints_Z in
    let br = {br with box=(BT.BR.Box.closure br.box)} in
    let box_expected = [("x",-1,3); ("y",0,4)] in
    BT.expect_domain_eq br box_expected;
    Printf.printf "first closure succeeded.\n";
    let br = BT.BR.init_constraints br [x_eq_one] in
    BT.expect_domain_eq br [("x",1,1); ("y",0,4)];
    let br = {br with box=(BT.BR.Box.closure br.box)} in
    BT.expect_domain_eq br [("x",1,1); ("y",0,2)];
    Printf.printf "second closure succeeded.\n";
  end

open Box_split
module Input_order_bisect = Make(Input_order)(Middle)(Bisect)
module Input_order_assign_lb = Make(Input_order)(Lower_bound)(Assign)

let test_split name (module Box: Box_sig) left_branch right_branch =
begin
  let (module BR : Box_maker_sig) = (module Box_maker(Box)) in
  let (module BT : Bound_tester_sig) = (module Bound_tester(BR)) in
  let br = BT.BR.init_vars BT.BR.empty ["x"; "y"] in
  let br = BT.BR.init_constraints br (x_eq_one::constraints_Z) in
  let br = {br with box=(BT.BR.Box.closure br.box)} in
  let boxes = BT.BR.Box.split br.box in
  Alcotest.(check int) name 2 (List.length boxes);
  let boxes = List.map BT.BR.Box.closure boxes in
  let brs = List.map (fun box -> {br with box}) boxes in
  BT.expect_domain_eq (List.nth brs 0) left_branch;
  Printf.printf "left branch succeeded.\n";
  BT.expect_domain_eq (List.nth brs 1) right_branch;
  Printf.printf "right branch succeeded.\n";
end

let test_split_input_order_bisect () =
  test_split "input order bisect" (module Box_base(Input_order_bisect)(Bound_int))
    [("x",1,1); ("y",0,1)]
    [("x",1,1); ("y",2,2)]

let test_split_input_order_assign_lb () =
  test_split "input order assign LB" (module Box_base(Input_order_assign_lb)(Bound_int))
    [("x",1,1); ("y",0,0)]
    [("x",1,1); ("y",1,2)]

let tests = [
  "init-closure(Z)", `Quick, test_Z;
  "split-input-order-bisect(Z)", `Quick, test_split_input_order_bisect;
  "split-input-order-assign-lb(Z)", `Quick, test_split_input_order_assign_lb;
]
