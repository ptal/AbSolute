(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core.Types
open Lang
open Lang.Ast
open Typing.Tast
open Domains.Interpretation
open Domains.Abstract_domain
open Event_loop
open Direct_product
open Bounds
open Box

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

module type Abstract_tester_sig =
sig
  module A: Abstract_domain
  val init_vars: vname list -> A.t
  val init_constraints: A.t -> bconstraint list -> A.t
  val expect_bound_eq: string -> string -> int -> int -> unit
  val expect_domain_eq: A.t -> (string * int * int) list -> unit
end

module Box_tester(Box: Box_sig) =
struct
  module Box = Box
  module E = Event_loop(Event_atom(Box))
  module A = Direct_product(
      Prod_cons(Box)(
      Prod_atom(E)))

  module I = A.I

  let box_uid = 1

  let init_vars vars =
    let box = ref (Box.empty box_uid) in
    let event = ref (E.init 2 box) in
    let a = A.init 0 (box, event) in
    let tf = List.fold_left
      (fun f name -> TExists(({name; ty=(Concrete Int); uid=box_uid}),f)) ttrue vars in
    fst (A.interpret a Exact tf)

  let init_constraints a cs =
    let cs = List.map (fun c -> TQFFormula ((box_uid, TCmp c))) cs in
    let cs = q_conjunction box_uid cs in
    let a, cs = A.interpret a Exact cs in
    List.fold_left A.weak_incremental_closure a cs

  let expect_bound_eq name var expected obtained =
    let name = name ^ " bound of `" ^ var ^ "`" in
    Alcotest.(check int) name expected obtained

  let expect_domain_eq a expected =
    List.iter (fun (var, lb, ub) ->
      let var_idx, _ = I.to_abstract_var (A.interpretation a) var in
      let (lb', ub') = A.project a var_idx in
      begin
        expect_bound_eq "lower" var lb (A.B.to_int_down lb');
        expect_bound_eq "upper" var ub (A.B.to_int_up ub')
      end)
    expected
end

(* III. Tests *)

let test_Z () =
  let (module BT : Abstract_tester_sig) = (module Box_tester(BoxFFB)) in
  let box = BT.init_vars ["x"; "y"] in
  begin
    BT.expect_domain_eq box [("x", B.minus_inf, B.inf); ("y", B.minus_inf, B.inf)];
    let box = BT.init_constraints box constraints_Z in
    let box, changed = BT.A.closure box in
    Alcotest.(check bool) "closure1 - changed" changed true;
    Printf.printf "closure changed succeeded.\n";
    let box_expected = [("x",-1,3); ("y",0,4)] in
    BT.expect_domain_eq box box_expected;
    Printf.printf "first closure succeeded.\n";
    let box = BT.init_constraints box [x_eq_one] in
    BT.expect_domain_eq box [("x",1,1); ("y",0,4)];
    let box, changed = BT.A.closure box in
    Alcotest.(check bool) "closure2 - changed" changed true;
    BT.expect_domain_eq box [("x",1,1); ("y",0,2)];
    Printf.printf "second closure succeeded.\n";
  end

open Box_split
module Input_order_bisect = Make(Input_order)(Middle)(Bisect)
module Input_order_assign_lb = Make(Input_order)(Lower_bound)(Assign)

let test_split name (module Box: Box_sig) left_branch right_branch =
begin
  let (module BT : Abstract_tester_sig) = (module Box_tester(Box)) in
  let box = BT.init_vars ["x"; "y"] in
  let box = BT.init_constraints box (x_eq_one::constraints_Z) in
  let box, _ = BT.A.closure box in
  let boxes = BT.A.split box in
  Alcotest.(check int) name 2 (List.length boxes);
  let box = BT.A.restore box (List.hd boxes) in
  let box, _ = BT.A.closure box in
  BT.expect_domain_eq box left_branch;
  Printf.printf "left branch succeeded.\n";
  let box = BT.A.restore box (List.nth boxes 1) in
  BT.expect_domain_eq box right_branch;
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
