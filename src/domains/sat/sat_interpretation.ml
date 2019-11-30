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
open Lang.Rewritting
open Domains.Interpretation
open Minisatml
open Minisatml.Types

module type Sat_interpretation_sig =
sig
  type rconstraint = Lit.lit Vec.t
  include module type of (Interpretation_base(
    struct type
      var_id=Solver.var
    end))

  (** Create a set of clauses from a formula.
      The formula is rewritten into CNF. *)
  val interpret: t -> approx_kind -> formula -> t * rconstraint list

  (** See [Interpretation.to_qformula] *)
  val to_qformula: t -> rconstraint list -> qformula
end

(* Replace `Equiv` and `Imply` with their logical equivalent using `And` and `Or`.
   NOTE: It duplicates formulas if they occur in `<=>`. *)
let eliminate_imply_and_equiv formula =
  let rec aux = function
    | Cmp _ as f -> f
    | FVar _ as f -> f
    | Equiv (f1,f2) ->
        let f1 = aux f1 in
        let f2 = aux f2 in
        And (Or (f1, Not f2), Or (Not f1, f2))
    | Imply (f1,f2) -> Or (Not (aux f1), aux f2)
    | And (f1,f2) -> And (aux f1, aux f2)
    | Or (f1,f2) -> Or (aux f1, aux f2)
    | Not f -> Not (aux f) in
  aux formula

(* Move logical negation inwards the formula by De Morgan's Law, e.g. not (a \/ b) --> (not a /\ not b).
   It supposes `eliminate_imply_and_equiv` has already been applied. *)
let move_not_inwards formula =
  let rec aux neg formula =
    if neg then
      match formula with
      | Cmp (e1, op, e2) -> Cmp (e1,Rewritting.neg op,e2)
      | FVar f -> Not (FVar f)
      | And (f1,f2) -> Or (aux true f1, aux true f2)
      | Or (f1,f2) -> And (aux true f1, aux true f2)
      | Not f -> aux false f
      | Equiv _ | Imply _ -> failwith "`move_not_inwards` must be called after `eliminate_imply_and_equiv`."
    else
      match formula with
      | Cmp _ as f -> f
      | FVar _ as f -> f
      | And (f1,f2) -> And (aux false f1, aux false f2)
      | Or (f1,f2) -> Or (aux false f1, aux false f2)
      | Not f -> aux true f
      | Equiv _ | Imply _ -> failwith "`move_not_inwards` must be called after `eliminate_imply_and_equiv`." in
  aux false formula

(* Distribute `Or` over `And`: a \/ (b /\ c) --> (a \/ b) /\ (a \/ c). *)
let distribute_or formula =
  let rec aux = function
    | Cmp _ as f -> f, false
    | FVar _ as f -> f, false
    | Or (f1, And (f2, f3)) -> And(Or(f1,f2), Or(f1,f3)), true
    | Or (And(f1,f2), f3) -> aux (Or(f3, And(f1,f2)))
    | And (f1,f2) ->
        let (f1, t1) = aux f1 in
        let (f2, t2) = aux f2 in
        And (f1, f2), (t1 || t2)
    | Or (f1,f2) ->
        let (f1, t1) = aux f1 in
        let (f2, t2) = aux f2 in
        Or (f1, f2), (t1 || t2)
    | Not f -> let (f1, t1) = aux f in Not f1, t1
    | Equiv _ | Imply _ -> failwith "`distribute_or` must be called after `eliminate_imply_and_equiv`." in
  let rec iter_aux (formula, has_changed) =
    if has_changed then iter_aux (aux formula)
    else formula in
  iter_aux (formula, true)

(* Naive conversion of a Boolean formula to a conjunctive normal form (CNF). *)
let formula_to_cnf formula =
  let formula = eliminate_imply_and_equiv formula in
  let formula = move_not_inwards formula in
  distribute_or formula

(* Given a formula in CNF, maps to each of its clauses.
   NOTE: this function assumes the formula is CNF. *)
let map_clauses f formula =
  let rec aux formula =
    match formula with
    | And(f1, f2) -> (aux f1)@(aux f2)
    | Or _
    | FVar _
    | Not FVar _ -> [f formula]
    | _ -> failwith "`map_clauses` assumes the formula is in CNF." in
  aux formula

module Sat_interpretation =
struct
  type rconstraint = Lit.lit Vec.t

  include Interpretation_base(struct type var_id=Solver.var end)

  let rewrite_clause repr clause =
    let rec aux = function
      | Cmp _ -> raise (Wrong_modelling "Constraints are not supported in Boolean domain, it only supports Boolean variables (`FVar`).")
      | FVar v -> [Lit.lit (to_abstract_var' repr v) false]
      | Not (FVar v) -> [Lit.lit (to_abstract_var' repr v) true]
      | Or (f1, f2) -> (aux f1)@(aux f2)
      | _ -> failwith "`rewrite_clause` is called on something else than a clause." in
    let clauses = aux clause in
    Vec.fromList clauses (List.length clauses)

  let interpret repr _ formula =
    let cnf = formula_to_cnf formula in
    repr, map_clauses (rewrite_clause repr) cnf

  let lit_to_formula repr lit =
    let var = FVar (to_logic_var' repr (Lit.var lit)) in
    if Lit.sign lit then Not var else var

  let to_disjunction repr clause =
    disjunction (List.map (lit_to_formula repr) (Array.to_list (Vec.get_data clause)))

  let to_formula repr cs = conjunction (List.map (to_disjunction repr) cs)

  let to_qformula repr cs = equantify repr (to_formula repr cs)
end
