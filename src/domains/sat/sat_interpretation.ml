(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang
open Lang.Ast
open Typing.Tast
open Domains.Interpretation
open Minisatml
open Minisatml.Types

module type Sat_interpretation_sig =
sig
  type rconstraint = Lit.lit Vec.t
  include module type of (Interpretation_ground(
    struct type
      var_id=Solver.var
    end))

  (** Create a set of clauses from a formula.
      The formula is rewritten into CNF. *)
  val interpret: t -> approx_kind -> tformula -> t * rconstraint list

  (** See [Interpretation.to_qformula] *)
  val to_qformula: t -> rconstraint list -> tqformula
end

(* Replace `Equiv` and `Imply` with their logical equivalent using `And` and `Or`.
   NOTE: It duplicates formulas if they occur in `<=>`. *)
let eliminate_imply_and_equiv tf =
  let rec aux tf =
    let uid = fst tf in
    match snd tf with
    | TCmp _ -> tf
    | TFVar _ -> tf
    | TEquiv (tf1,tf2) ->
        let tf1 = aux tf1 in
        let tf2 = aux tf2 in
        (uid, TAnd ((uid, TOr (tf1, (uid, TNot tf2))), (uid, TOr ((uid, TNot tf1), tf2))))
    | TImply (tf1,tf2) -> (uid, TOr ((uid, TNot (aux tf1)), aux tf2))
    | TAnd (tf1,tf2) -> (uid, TAnd (aux tf1, aux tf2))
    | TOr (tf1,tf2) -> (uid, TOr (aux tf1, aux tf2))
    | TNot tf -> (uid, TNot (aux tf)) in
  aux tf

(* Move logical negation inwards the formula by De Morgan's Law, e.g. not (a \/ b) --> (not a /\ not b).
   It supposes `eliminate_imply_and_equiv` has already been applied. *)
let move_not_inwards tf =
  let rec aux neg tf =
    let uid = fst tf in
    if neg then
      match snd tf with
      | TCmp c -> uid, TCmp (Rewritting.neg_bconstraint c)
      | TFVar v -> uid, TNot (uid, TFVar v)
      | TAnd (tf1,tf2) -> uid, TOr (aux true tf1, aux true tf2)
      | TOr (tf1,tf2) -> uid, TAnd (aux true tf1, aux true tf2)
      | TNot tf -> aux false tf
      | TEquiv _ | TImply _ -> failwith "`move_not_inwards` must be called after `eliminate_imply_and_equiv`."
    else
      match snd tf with
      | TCmp _ -> tf
      | TFVar _ -> tf
      | TAnd (tf1,tf2) -> uid, TAnd (aux false tf1, aux false tf2)
      | TOr (tf1,tf2) -> uid, TOr (aux false tf1, aux false tf2)
      | TNot tf -> aux true tf
      | TEquiv _ | TImply _ -> failwith "`move_not_inwards` must be called after `eliminate_imply_and_equiv`." in
  aux false tf

(* Distribute `Or` over `And`: a \/ (b /\ c) --> (a \/ b) /\ (a \/ c). *)
let distribute_or tf =
  let rec aux tf =
    let uid = fst tf in
    match snd tf with
    | TCmp _ -> tf, false
    | TFVar _ -> tf, false
    | TOr (tf1, (_,TAnd (tf2, tf3))) -> (uid, TAnd ((uid, TOr(tf1,tf2)), (uid, TOr(tf1,tf3)))), true
    | TOr ((_, TAnd(tf1,tf2)), tf3) -> aux (uid, TOr(tf3, (uid, TAnd(tf1,tf2))))
    | TAnd (tf1,tf2) ->
        let (tf1, t1) = aux tf1 in
        let (tf2, t2) = aux tf2 in
        (uid, TAnd (tf1, tf2)), (t1 || t2)
    | TOr (tf1,tf2) ->
        let (tf1, t1) = aux tf1 in
        let (tf2, t2) = aux tf2 in
        (uid, TOr (tf1, tf2)), (t1 || t2)
    | TNot f -> let (tf1, t1) = aux f in (uid, TNot tf1), t1
    | TEquiv _ | TImply _ -> failwith "`distribute_or` must be called after `eliminate_imply_and_equiv`." in
  let rec iter_aux (tf, has_changed) =
    if has_changed then iter_aux (aux tf)
    else tf in
  iter_aux (tf, true)

(* Naive conversion of a Boolean formula to a conjunctive normal form (CNF). *)
let tformula_to_cnf tf =
  let tf = eliminate_imply_and_equiv tf in
  let tf = move_not_inwards tf in
  distribute_or tf

(* Given a formula in CNF, maps to each of its clauses.
   NOTE: this function assumes the formula is CNF. *)
let map_clauses f tf =
  let rec aux tf =
    match snd tf with
    | TAnd(tf1, tf2) -> (aux tf1)@(aux tf2)
    | TOr _
    | TFVar _
    | TNot (_,TFVar _) -> [f tf]
    | _ -> failwith "`map_clauses` assumes the formula is in CNF." in
  aux tf

module Sat_interpretation =
struct
  type rconstraint = Lit.lit Vec.t

  module IG = Interpretation_ground(struct type var_id=Solver.var end)
  include IG

  let rewrite_clause repr clause =
    let rec aux tf =
      match snd tf with
      | TCmp _ -> raise (Wrong_modelling "Constraints are not supported in Boolean domain, it only supports Boolean variables (`FVar`).")
      | TFVar v -> [Lit.lit (to_abstract_var' repr v) false]
      | TNot ((_,TFVar v)) -> [Lit.lit (to_abstract_var' repr v) true]
      | TOr (tf1, tf2) -> (aux tf1)@(aux tf2)
      | _ -> failwith "`rewrite_clause` is called on something else than a clause." in
    let clauses = aux clause in
    Vec.fromList clauses (List.length clauses)

  let interpret repr _ tf =
    IG.interpret_gen' repr "SAT" tf (fun repr tf ->
      let cnf = tformula_to_cnf tf in
      repr, map_clauses (rewrite_clause repr) cnf
    )

  let lit_to_formula (repr:t) lit =
    let var = (uid repr), TFVar (to_logic_var' repr (Lit.var lit)) in
    if Lit.sign lit then (uid repr), TNot var else var

  let to_disjunction repr clause =
    let literals = List.map (lit_to_formula repr) (Array.to_list (Vec.get_data clause)) in
    let literals = List.map (fun l -> TQFFormula l) literals in
    match q_disjunction (uid repr) literals with
    | TQFFormula tf -> tf
    | TExists _ -> failwith "unreachable because no variable has been added yet."

  let to_qformula repr cs = IG.to_qformula_gen repr cs to_disjunction
end
