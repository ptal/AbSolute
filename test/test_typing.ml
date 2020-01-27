(* Copyright 2020 Pierre Talbot

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
open Typing
open Typing.Ad_type
open Typing.Tast
open Lang.Ast

let string_of_uids uids =
  if List.length uids = 1 then string_of_int (List.hd uids)
  else
    let rec aux = function
      | [] -> ""
      | u::l -> (string_of_int u) ^ " " ^ (aux l) in
    "[" ^ (aux uids) ^ "]"

let check_uids u1 u2 printer f =
  if u1 = u2 then true
  else
   (Format.printf "Expected UID %d but obtained UID %d\n  Formula: %a"
     u1 u2 printer f;
    false)

let mismatch_formula_error () =
  Printf.printf "The typed and untyped formulas are not the same, which mean that `Infer.infer_type` did not preserve the shape of the formula.";
  false

let mtv name ty uid = Tast.{name;ty;uid}

let check_same_type_formula tf1 tf2 =
  let rec aux (u1,expected) (u2,obtained) =
    if check_uids u1 u2 Pretty_print.print_formula (tformula_to_formula (u2, obtained)) then
      match expected, obtained with
      | TFVar _, TFVar _ | TCmp _, TCmp _ -> true
      | TEquiv(tf1,tf2), TEquiv(tf1',tf2') -> aux tf1 tf1' && aux tf2 tf2'
      | TImply(tf1,tf2), TImply(tf1',tf2') -> aux tf1 tf1' && aux tf2 tf2'
      | TAnd(tf1,tf2), TAnd(tf1',tf2') -> aux tf1 tf1' && aux tf2 tf2'
      | TOr(tf1,tf2), TOr(tf1',tf2') -> aux tf1 tf1' && aux tf2 tf2'
      | TNot tf1, TNot tf1' -> aux tf1 tf1'
      | _ -> mismatch_formula_error ()
    else false in
  aux tf1 tf2

let rec check_same_type_qformula = function
  | (TExists(tv1,tf) as f), TExists(tv2,tf') ->
      if check_uids tv1.uid tv2.uid Pretty_print.print_qformula (tqformula_to_qformula f) then
        check_same_type_qformula (tf, tf')
      else false
  | TQFFormula tf, TQFFormula tf' -> check_same_type_formula tf tf'
  | _ -> mismatch_formula_error ()

let test_typing_exn ad_name dp f =
  Printexc.record_backtrace true;
  try
    Infer.infer_type dp f
  with
  | Wrong_modelling msg ->
      (Printf.printf "%s\n" msg;
       Printexc.print_backtrace stdout;
       Alcotest.(check bool) ("Could not type the formula in " ^ ad_name) true false; failwith "")
  | Not_found -> (Printexc.print_backtrace stdout; failwith "Not_found exception in infer.")

let expect_typing_exn ad_name dp f =
  try
    ignore(Infer.infer_type dp f);
    Alcotest.(check bool) ("Typing succeeded in " ^ ad_name ^ " but should have failed.") true false
  with Wrong_modelling _ -> ()

let test_simple_direct_product () =
  let x = Var "x" in
  let y = Var "y" in
  let c1 = (Binary(Unary(NEG,x), ADD, one), LEQ, y) in
  let conjunctive_f =
    Exists("x", Concrete Int,
    Exists("y", Concrete Int,
    Exists("b1", Abstract Bool,
    QFFormula(And(FVar "b1", Cmp c1))))) in
  let du, bu, ou = 0, 1, 2 in
  let box = (bu, Box (Interval Z)) in
  let oct = (ou, Octagon Z) in
  let dp = (du, Direct_product([box; oct])) in
  let dp_ob_expected_type =
    TExists((mtv "x" (Concrete Int) ou),
    TExists((mtv "y" (Concrete Int) ou),
    TExists((mtv "b1" (Abstract Bool) bu),
    TQFFormula(
      (du, TAnd((bu, TFVar "b1"), (ou, TCmp c1)))
    )))) in
  let dp_ob_obtained_type = test_typing_exn "Box X Oct" dp conjunctive_f in
  let res = check_same_type_qformula (dp_ob_expected_type, dp_ob_obtained_type) in
  Alcotest.(check bool) "Test direct product" true res

let test_typing_logic_completion () =
  let x = Var "x" in
  let y = Var "y" in
  let c1 = (one, LEQ, y) in
  let make_equiv_formula c =
    Exists("x", Concrete Int,
    Exists("y", Concrete Int,
    Exists("b1", Abstract Bool,
    QFFormula(Equiv(FVar "b1", Cmp c))))) in
  let equiv_formula = make_equiv_formula c1 in

  let du, bu, lu = 0, 1, 2 in
  let box = (bu, Box (Interval Z)) in
  let lcb = (lu, Logic_completion box) in
  let dp = (du, Direct_product([box; lcb])) in
  let lcb_expected_type =
    TExists((mtv "x" (Concrete Int) bu),
    TExists((mtv "y" (Concrete Int) bu),
    TExists((mtv "b1" (Abstract Bool) bu),
    TQFFormula(
      (lu, TEquiv((bu, TFVar "b1"), (bu, TCmp c1)))
    )))) in
  let lcb_obtained_type = test_typing_exn "LC(Box)" dp equiv_formula in
  let res = check_same_type_qformula (lcb_expected_type, lcb_obtained_type) in
  Alcotest.(check bool) "Test logic completion" true res;

  let c2 = (Binary(Unary(NEG,x), ADD, one), LEQ, y) in
  let equiv_formula = make_equiv_formula c2 in
  expect_typing_exn "LC(Box)" dp equiv_formula;

  let bu, ou, obu, lu, du = 1, 2, 3, 4, 0 in
  let box = (bu, Box (Interval Z)) in
  let oct = (ou, Octagon Z) in
  let ob = (obu, Direct_product([box;oct])) in
  let lc_ob = (lu, Logic_completion ob) in
  let dp = (du, Direct_product([box; oct; lc_ob])) in
  let lc_ob_expected_type =
    TExists((mtv "x" (Concrete Int) ou),
    TExists((mtv "y" (Concrete Int) ou),
    TExists((mtv "b1" (Abstract Bool) bu),
    TQFFormula(
      (lu, TEquiv((bu, TFVar "b1"), (ou, TCmp c2)))
    )))) in
  let lc_ob_obtained_type = test_typing_exn "LC(Box X Oct)" dp equiv_formula in
  let res = check_same_type_qformula (lc_ob_expected_type, lc_ob_obtained_type) in
  Alcotest.(check bool) "RCPSP-like typing with LC(Box X Oct)" true res

let test_typing_rcpsp_like () =
  (* Some reusable AST part between the untyped and typed formula. *)
  let x,y = Var "x", Var "y" in
  let b1, b2 = Var "b1", Var "b2" in
  let c1 = (Binary(x, ADD, one), LEQ, y) in
  let c2,c3 = (x,LEQ,y), (y,LT,Binary(x,SUB,two)) in
  let c4,c5 = (y,GEQ,x), (Unary(NEG,y),GT,Binary((Unary(NEG,x),SUB,two))) in
  let c6 = (Binary(Unary(NEG,x), ADD, one), LEQ, y) in
  let c7 = (Binary(Binary(one,MUL,b1),ADD,Binary(two,MUL,b2)),LEQ,two) in
  let rcpsp_like_formula =
    Exists("x", Concrete Int,
    Exists("y", Concrete Int,
    Exists("b1", Abstract Bool,
    Exists("b2", Abstract Bool,
    QFFormula(
      And(Cmp c1,  (* Octagon - precedence constraint *)
      And(Equiv(FVar "b1", And(Cmp c2, Cmp c3)), (* Logic_completion(Octagon) - bridge between resource and precedence constraints. *)
      And(Equiv(FVar "b2", And(Cmp c4, Cmp c5)),
      And(Cmp c6, (* Octagon - precedence constraint*)
      Cmp c7 (* Box - resource constraints*)
    ))))))))) in

  (* We first try to type this formula in the most basic abstract domain `Logic_completion(Propagator_completion(Box))`. *)
  let bu, pu, lu, du = 1, 2, 3, 0 in
  let box = (bu, Box (Interval Z)) in
  let pc_box = (pu, Propagator_completion box) in
  let lc_box = (lu, Logic_completion pc_box) in
  let dp = (du, Direct_product([box; pc_box; lc_box])) in
  let lc_box_expected_type =
    TExists((mtv "x" (Concrete Int) bu),
    TExists((mtv "y" (Concrete Int) bu),
    TExists((mtv "b1" (Abstract Bool) bu),
    TExists((mtv "b2" (Abstract Bool) bu),
    TQFFormula(
      (lu, TAnd((pu, TCmp c1),
      (lu, TAnd((lu, TEquiv((bu, TFVar "b1"), (pu, TAnd((pu, TCmp c2), (pu, TCmp c3))))),
      (lu, TAnd((lu, TEquiv((bu, TFVar "b2"), (pu, TAnd((pu, TCmp c4), (pu, TCmp c5))))),
      (pu, TAnd((pu, TCmp c6),
      (pu, TCmp c7)
    ))))))))))))) in
  let lc_box_obtained_type = test_typing_exn "LC(PC(Box))" dp rcpsp_like_formula in
  let res = check_same_type_qformula (lc_box_expected_type, lc_box_obtained_type) in
  Alcotest.(check bool) "RCPSP-like typing with LC(PC(Box))" true res;

  (* A more involved domain includes box, octagon and the logic completion of their product. *)
  let bu, pu, ou, obu, lu, du = 1, 2, 3, 4, 5, 0 in
  let box = (bu, Box (Interval Z)) in
  let pc_box = (pu, Propagator_completion box) in
  let oct = (ou, Octagon Z) in
  let ob = (obu, Direct_product([pc_box;oct])) in
  let lc_ob = (lu, Logic_completion ob) in
  let dp = (du, Direct_product([box; pc_box; oct; ob; lc_ob])) in
  let lc_ob_expected_type =
    TExists((mtv "x" (Concrete Int) ou),
    TExists((mtv "y" (Concrete Int) ou),
    TExists((mtv "b1" (Abstract Bool) bu),
    TExists((mtv "b2" (Abstract Bool) bu),
    TQFFormula(
      (lu, TAnd((ou, TCmp c1),
      (lu, TAnd((lu, TEquiv((bu, TFVar "b1"), (ou, TAnd((ou, TCmp c2), (ou, TCmp c3))))),
      (lu, TAnd((lu, TEquiv((bu, TFVar "b2"), (ou, TAnd((ou, TCmp c4), (ou, TCmp c5))))),
      (obu, TAnd((ou, TCmp c6),
      (pu, TCmp c7)
    ))))))))))))) in
  let lc_ob_obtained_type = test_typing_exn "LC(Box X Oct)" dp rcpsp_like_formula in
  let res = check_same_type_qformula (lc_ob_expected_type, lc_ob_obtained_type) in
  Alcotest.(check bool) "RCPSP-like typing with LC(Box X Oct)" true res

(* let test_typing_jfsp_like () = *)

let tests = [
  "simple_direct_product", `Quick, test_simple_direct_product;
  "logic_completion", `Quick, test_typing_logic_completion;
  "typing_rcpsp_like", `Quick, test_typing_rcpsp_like;
  (* "typing_fjsp_like", `Quick, test_typing_jfsp_like; *)
]

