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
open Typing.Tast
open Domains.Interpretation
open Event_loop
open Direct_product
open Logic_completion
open Vardom
open Propagator_completion
open Box

open Test_box

let box_uid = 1
let s_uid = 2
let lc_uid = 3

module LC_tester(Box: Box_sig) =
struct
  module PC = Propagator_completion(Itv.Itv(B))(Box)
  module LC = Logic_completion(PC)
  module E = Event_loop(Event_cons(PC)(Event_atom(LC)))
  module A = Direct_product(
      Prod_cons(Box)(
      Prod_cons(PC)(
      Prod_cons(LC)(
      Prod_atom(E)))))

  module I = A.I

  let init_vars vars =
    let box = ref (Box.empty box_uid) in
    let pc = ref (PC.init {a=box; uid=s_uid}) in
    let lc = ref (LC.init LC.I.{uid=lc_uid;a=pc}) in
    let event = ref (E.init 4 (pc,lc)) in
    let a = A.init 0 (box, (pc, (lc, event))) in
    let tf = List.fold_left
      (fun f name -> TExists(({name; ty=(Concrete Int); uid=box_uid}),f)) ttrue vars in
    fst (A.interpret a Exact tf)

  (** Type all c in cs with s_uid, and assemble them in a disjunction typed with the logic_completion. *)
  let init_constraints a cs =
    let cs = List.map (fun (_,c) -> TQFFormula ((s_uid, TCmp c))) cs in
    let cs = q_disjunction lc_uid cs in
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

let test_interpretation_lc () =
  let (module LCT : Abstract_tester_sig) = (module LC_tester(BoxFFB)) in
  let lc = LCT.init_vars ["x"; "y"] in
  begin
    LCT.expect_domain_eq lc [("x", B.minus_inf, B.inf); ("y", B.minus_inf, B.inf)];
    let lc = LCT.init_constraints lc constraints_Z in
    let lc, changed = LCT.A.closure lc in
    Alcotest.(check bool) "closure1 - changed" changed false;
    Printf.printf "closure changed succeeded.\n";
    LCT.expect_domain_eq lc [("x", B.minus_inf, B.inf); ("y", B.minus_inf, B.inf)];
    Printf.printf "first closure succeeded.\n";
    let lc = LCT.init_constraints lc [x_eq_one] in
    LCT.expect_domain_eq lc [("x",1,1); ("y", B.minus_inf, B.inf)];
    let lc, changed = LCT.A.closure lc in
    Alcotest.(check bool) "closure2 - changed" changed false;
    LCT.expect_domain_eq lc [("x",1,1); ("y", B.minus_inf, B.inf)];
    Printf.printf "second closure succeeded.\n";
  end

let tests = [
  "interpretation_lc", `Quick, test_interpretation_lc;
]
