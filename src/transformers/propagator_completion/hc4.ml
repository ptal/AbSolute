(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Bounds.Converter
open Lang.Ast
open Core.Bot
open Lang.Rewritting
open Pc_interpretation

module type PC_closure_sig = functor (I: PC_interpretation_sig) ->
sig
  module I: PC_interpretation_sig
  val incremental_closure: I.A.t -> I.rconstraint -> I.A.t * bool
  val entailment: I.A.t -> I.rconstraint -> bool
  val project: I.A.t -> I.var_id -> I.V.t
  val embed: I.A.t -> I.var_id -> I.V.bound * I.V.bound -> I.A.t
end with module I=I

module Make(I: PC_interpretation_sig) =
struct
  module I = I
  module A = I.A
  module V = I.V

  module FromA = Converter(A.B)(I.V.B)
  module ToA = Converter(I.V.B)(A.B)

  let expr_val expr = I.(expr.value)
  let exprs_val exprs = List.map expr_val exprs

  let vardom_of abs vid =
    let (l,u) = A.project abs vid in
    V.of_bounds' (FromA.convert_down l, FromA.convert_up u)

  let merge_view abs root vids =
    List.fold_left
      (fun v vid -> debot (V.meet v (vardom_of abs vid)))
      root vids

  let project abs vids =
    merge_view abs (vardom_of abs (List.hd vids)) (List.tl vids)

  let embed abs vids (l,u) =
    let (l,u) = ToA.convert_down l, ToA.convert_up u in
    List.fold_left (fun abs vid -> A.embed abs vid (l,u)) abs vids

  (* I. Evaluation part

     First step of the HC4-revise algorithm: it computes the intervals for each node of the expression.
     For example: given `x + 3` with `x in [1..3]`, it annotates `+` with `[4..6]`.
     It stores this new valued expression tree (`node`) into `rexpr.value`.

     This function is also useful for testing transfer functions errors (e.g. division by zero).
     - We raise Bot_found in case the expression only evaluates to error values.
     - Otherwise, we return only the non-error values.
   *)
  let rec eval abs expr =
    let open I in
    match expr.node with
    | BVar (vids, _) -> expr.value <- project abs vids
    | BCst (v,_) -> expr.value <- v
    | BUnary (o,e1) ->
      begin
        eval abs e1;
        match o with
        | NEG -> expr.value <- V.unop NEG (expr_val e1)
      end
    | BBinary (e1,o,e2) ->
      begin
        eval abs e1;
        eval abs e2;
        let v1 = expr_val e1 and v2 = expr_val e2 in
        let v = match o with
        | ADD -> V.binop ADD v1 v2
        | SUB -> V.binop SUB v1 v2
        | DIV -> debot (V.div v1 v2)
        | MUL ->
          let r = V.binop MUL v1 v2 in
          if V.equal v1 v2 then
            (* special case: squares are positive *)
            V.unop ABS r
          else r
        | POW -> V.binop POW v1 v2 in
        expr.value <- v
      end
    | BFuncall(name, args) ->
      begin
        List.iter (eval abs) args;
        let r = debot (V.eval_fun name (exprs_val args)) in
        expr.value <- r
      end

  (* II. Refine part

     Second step of the HC4-revise algorithm.
     It propagates the intervals from the root of the expression tree `e` to the leaves.
     For example: Given `y = x + 3`, `x in [1..3]`, `y in [1..5]`.
                  Then after `eval` we know that the node at `+` has the interval `[4..6]`.
                  Therefore we can intersect `y` with `[4..6]` due to the equality.
     Note that we can call again `eval` to restrain further `+`, and then another round of `refine` will restrain `x` as well.
     We raise `Bot_found` in case of unsatisfiability.

     NOTE: This step is functional: it does not modify the `rexpr.value` field.
   *)

  (* refines binary operator to handle constants *)
  let refine_bop f1 f2 e1 e2 x (b:bool) =
    let open I in
    match e1.node, e2.node, b with
    | BCst _, BCst _, _ -> Nb (e1.value, e2.value)
    | BCst _, _, true -> merge_bot2 (Nb e1.value) (f2 e2.value e1.value x)
    | BCst _, _, false -> merge_bot2 (Nb e1.value) (f2 e2.value x e1.value)
    | _, BCst _, _ -> merge_bot2 (f1 e1.value e2.value x) (Nb e2.value)
    | _, _, true -> merge_bot2 (f1 e1.value e2.value x) (f2 e2.value e1.value x)
    | _, _, false -> merge_bot2 (f1 e1.value e2.value x) (f2 e2.value x e1.value)

  (* u + v = r => u = r - v /\ v = r - u *)
  let refine_add u v r =
    refine_bop (V.filter_binop_f ADD) (V.filter_binop_f ADD) u v r true

  (* u - v = r => u = r + v /\ v = u - r *)
  let refine_sub u v r =
    refine_bop (V.filter_binop_f SUB) (V.filter_binop_f ADD) u v r false

  (* u * v = r => (u = r/v \/ v=r=0) /\ (v = r/u \/ u=r=0) *)
  let refine_mul u v r =
    refine_bop (V.filter_binop_f MUL) (V.filter_binop_f MUL) u v r true

  (* u / v = r => u = r * v /\ (v = u/r \/ u=r=0) *)
  let refine_div u v r =
    refine_bop V.filter_div_f (V.filter_binop_f MUL) u v r false

  let refine readonly abs root expr =
    let rec aux abs root expr =
      let open I in
      match expr with
      | BFuncall(name,args) ->
         let res = V.filter_fun name (List.map (fun e -> I.(e.value)) args) root in
         List.fold_left2 (fun acc res e -> aux acc res e.node) abs (debot res) args
      | BVar (vids, _) ->
          if readonly then
            let _ = ignore(merge_view abs root vids) in
            abs
          else
            let (l,u) = V.to_range root in
            (* Format.printf "Embed %a in %s for %d domains.\n" V.print root tv.name (List.length vids); *)
            embed abs vids (l,u)
      | BCst (i,_) -> ignore (debot (V.meet root i)); abs
      | BUnary (op,e) ->
         let j = match op with
           | NEG -> V.filter_unop NEG e.value root
         in aux abs (debot j) e.node
      | BBinary (e1,o,e2) ->
         let j = match o with
           | ADD -> refine_add e1 e2 root
           | SUB -> refine_sub e1 e2 root
           | MUL -> refine_mul e1 e2 root
           | DIV -> refine_div e1 e2 root
           | POW -> V.filter_binop POW e1.value e2.value root
         in
         let j1,j2 = debot j in
         aux (aux abs j1 e1.node) j2 e2.node in
    aux abs root expr

  (* III. HC4-revise algorithm (combining eval and refine).

     Apply the evaluation followed by the refine step of the HC4-revise algorithm.
     It prunes the domain of the variables in `abs` according to the constraint `e1 o e2`.
  *)
  let hc4_revise readonly abs (e1,op,e2) =
    let i1,i2 = expr_val e1, expr_val e2 in
    let j1,j2 = match op with
      | LT  -> debot (V.filter_lt i1 i2)
      | LEQ -> debot (V.filter_leq i1 i2)
      (* a > b <=> b < a*)
      | GEQ -> let j2,j1 = debot (V.filter_leq i2 i1) in (j1,j2)
      | GT  -> let j2,j1 = debot (V.filter_lt i2 i1) in (j1,j2)
      | NEQ -> debot (V.filter_neq i1 i2)
      | EQ  -> debot (V.filter_eq i1 i2)
    in
    let refined_store = if V.equal j1 i1 then abs else refine readonly abs j1 I.(e1.node) in
    if j2 = i2 then refined_store else refine readonly refined_store j2 I.(e2.node)

  let hc4_eval_revise abs (e1,op,e2) =
  begin
    eval abs e1;
    eval abs e2;
    let abs = hc4_revise false abs (e1,op,e2) in
    try
      ignore(hc4_revise true abs (e1,neg op,e2));
      abs, false
    with Bot_found -> abs, true
  end

  let incremental_closure abs c = hc4_eval_revise abs c

  let entailment abs (e1,op,e2) =
    try
      eval abs e1;
      eval abs e2;
      ignore(hc4_revise true abs (e1,neg op,e2));
      false
    with Bot_found -> true
end
