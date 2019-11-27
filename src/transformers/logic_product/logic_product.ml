(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Core.Types
open Core.Kleene
open Bounds
open Lang
open Lang.Ast
open Domains.Interpretation
open Domains.Abstract_domain
open Ordered_product
open Event_loop.Event_abstract_domain

type qfp_formula =
  | Atom of gconstraint list
  | PNot of pn_formula
  | PAnd of pn_formula * pn_formula
  | POr of pn_formula * pn_formula
  | PImply of pn_formula * pn_formula
  | PEquiv of pn_formula * pn_formula
and approx_formula = {
  ask: qfp_formula;
  tell: qfp_formula;
}
and pn_formula = {
  positive: approx_formula;
  negative: approx_formula;
}

module type Logic_prod_interpretation_sig =
sig
  type t
  type init_t
  type var_id = unit
  type rconstraint = qfp_formula

  val init: init_t -> t
  val empty: unit -> t
  val extend: t -> (var * gvar * var_abstract_ty) -> t
  val exists: t -> var -> bool
  val to_logic_var: t -> gvar -> (var * var_abstract_ty)
  val to_abstract_var: t -> var -> (gvar * var_abstract_ty)
  val interpret: t -> approx_kind -> formula -> t * qfp_formula list
  val to_qformula: t -> qfp_formula list -> qformula
end

(* We do not support variable in this product to avoid overlap with `Ordered_product` in functionalities. *)
let no_variable_exn from = raise (Wrong_modelling
  (from ^ " abstract domain does not support variable. \
   Variables should be manipulated directly in the corresponding subdomain or through another product such as `Ordered_product`."))

module type LProd_combinator =
sig
  include Prod_combinator
  val drain_events: t -> (t * event list)
  val events_of: t -> rconstraint -> event list
end

module LProd_atom(A: Event_abstract_domain) =
struct
  include Prod_atom(A)

  let drain_events pa =
    let a, events = A.drain_events (unwrap pa) in
    wrap pa a, events

  let events_of pa c =
    A.events_of (unwrap pa) (get_constraint pa c)
end

module LProd_cons(A: Event_abstract_domain)(B: LProd_combinator) =
struct
  include Prod_cons(A)(B)
  module Atom = LProd_atom(A)

  let drain_events (a,b) =
    let a, events = Atom.drain_events a in
    let b, events' = B.drain_events b in
    (a,b), events@events'

  let events_of (a,b) c =
    if Atom.uid a != (fst c) then B.events_of b c
    else Atom.events_of a c
end

module Logic_prod_interpretation(P: LProd_combinator) =
struct
  type init_t = P.init_t
  type t = P.t
  type var_id = unit
  type rconstraint = qfp_formula

  let init = P.init
  let empty = P.empty
  let exists = P.exists
  let extend _ _ = no_variable_exn "Logic_prod_interpretation.extend"
  let to_logic_var _ _ = no_variable_exn "Logic_prod_interpretation.to_logic_var"
  let to_abstract_var _ _ = no_variable_exn "Logic_prod_interpretation.to_abstract_var"

  (** The formula is not rewritten and is represented as such. *)
  let interpret p approx f =
    let rec make_approx_formula p approx f =
      let p, ask = aux p UnderApprox f in
      let p, tell = aux p approx f in
      p, { ask; tell }
    and make_pn_formula p approx f =
      let p, positive = make_approx_formula p approx f in
      let p, negative = make_approx_formula p approx (Rewritting.neg_formula f) in
      p, { positive; negative }
    and aux p approx f =
      match f with
      (* Literals and constraints are the base cases. *)
      | Not(FVar _) | FVar _ | Cmp _ ->
          let p, fs = P.interpret p approx f in
          p, Atom fs
      | Not(f1) ->
          let p, f1 = make_pn_formula p approx f1 in
          p, PNot f1
      | And(f1, f2) -> aux_binary p f1 f2 (fun f1 f2 -> PAnd(f1,f2))
      | Equiv(f1, f2) -> aux_binary p f1 f2 (fun f1 f2 -> PEquiv(f1,f2))
      | Imply(f1, f2) -> aux_binary p f1 f2 (fun f1 f2 -> PImply(f1,f2))
      | Or(f1, f2) -> aux_binary p f1 f2 (fun f1 f2 -> POr(f1,f2))
    and aux_binary p f1 f2 make =
      let p, f1 = make_pn_formula p approx f1 in
      let p, f2 = make_pn_formula p approx f2 in
      p, make f1 f2 in
    let rec top_aux p approx f =
      match f with
      | And(f1, f2) ->
          let p, f1 = top_aux p approx f1 in
          let p, f2 = top_aux p approx f2 in
          p, f1@f2
      | _ -> let p, f = aux p approx f in p, [f]
    in
      top_aux p approx f

  let to_qformula p fs =
    let open Rewritting in
    let rec aux = function
      | Atom c -> P.to_qformula p c
      | PNot f1 -> map_formula (fun f -> Not f) (aux' f1)
      | PAnd(f1,f2) -> binary_aux f1 f2 (fun f1 f2 -> And(f1,f2))
      | POr(f1,f2) -> binary_aux f1 f2 (fun f1 f2 -> Or(f1,f2))
      | PImply(f1,f2) -> binary_aux f1 f2 (fun f1 f2 -> Imply(f1,f2))
      | PEquiv(f1,f2) -> binary_aux f1 f2 (fun f1 f2 -> Equiv(f1,f2))
    and aux' f1 = aux f1.positive.tell
    and binary_aux f1 f2 make =
      merge_formula make (aux' f1) (aux' f2)
    in q_conjunction (List.map aux fs)
end

module Logic_product(P: LProd_combinator) =
struct
  module B = Bound_unit
  module I = Logic_prod_interpretation(P)

  type t = {
    uid: ad_uid;
    prod: I.t;
    constraints: I.rconstraint Parray.t;
    (* Store the new constraint's indices since last call to `drain_tasks`. *)
    new_tasks: int list;
    num_active_tasks: int;
  }

  let empty _ = raise (Wrong_modelling ("Logic_product must be initialized with `init`."))

  let init uid prod = {
    uid;
    prod=(P.init prod);
    constraints = Tools.empty_parray ();
    new_tasks = [];
    num_active_tasks = 0;
  }

  let uid qp = qp.uid

  let name = "Logic_product(" ^ P.name ^ ")"

  (* Entailed constraints are automatically deactivated by `Event_loop`. *)
  let state qp =
    if qp.num_active_tasks = 0 then True
    else Unknown

  let extend ?ty:_ _ = no_variable_exn "Logic_product.extend"
  let project _ _ = no_variable_exn "Logic_product.project"

  (* This abstract domain is totally functional.
     The responsibility to copy subdomains is delegated to `Ordered_product`. *)
  type snapshot = t
  let lazy_copy p n = List.init n (fun _ -> p)
  let restore _ s = s

  let rec entailment qp = function
    | Atom cs -> List.for_all (P.entailment qp.prod) cs
    | PNot f1 -> entailment qp f1.negative.ask
    | PAnd(f1, f2) when entailment qp f1.positive.ask ->
        entailment qp f2.positive.ask
    | PAnd(f1, f2) when entailment qp f2.positive.ask ->
        entailment qp f1.positive.ask
    | POr(f1, _) when entailment qp f1.positive.ask -> true
    | POr(_, f2) when entailment qp f2.positive.ask -> true
    | PImply(f1, f2) when entailment qp f1.positive.ask ->
        entailment qp f2.positive.ask
    | PImply(f1, _) when entailment qp f1.negative.ask -> true
    | PEquiv(f1, f2) when entailment qp f1.positive.ask ->
        entailment qp f2.positive.ask
    | PEquiv(f1, f2) when entailment qp f1.negative.ask ->
        entailment qp f2.negative.ask
    | _ -> false

  type entailment_cases =
    | F1_entailed | F2_entailed
    | F1_disentailed | F2_disentailed
    | Unknown_entailment

  let binary_entailment qp f1 f2 =
    if entailment qp f1.positive.ask then F1_entailed
    else if entailment qp f2.positive.ask then F2_entailed
    else if entailment qp f1.negative.ask then F1_disentailed
    else if entailment qp f2.negative.ask then F2_disentailed
    else Unknown_entailment

  (* `None` if the constraint `f` is entailed.
     `Some c` if the constraint is not yet entailed, and has been rewritten to `c`. *)
  let rec incremental_closure qp f =
    let map_tell pn_f tell =
      {pn_f with positive={pn_f.positive with tell}} in
    match f with
    | Atom cs ->
        let prod = List.fold_left P.weak_incremental_closure qp.prod cs in
        {qp with prod}, None
    | PNot f1 -> incremental_closure qp f1.negative.tell
    | PAnd(f1,f2) ->
        let qp, qf1 = incremental_closure qp f1.positive.tell in
        let qp, qf2 = incremental_closure qp f2.positive.tell in
        begin match qf1, qf2 with
        | None, None -> qp, None
        | Some t1, None -> qp, Some t1
        | None, Some t2 -> qp, Some t2
        | Some t1, Some t2 -> qp, Some(PAnd(map_tell f1 t1, map_tell f2 t2))
        end
    | POr(f1, f2) ->
        begin match binary_entailment qp f1 f2 with
        | F1_entailed | F2_entailed -> qp, None
        | F1_disentailed -> incremental_closure qp f2.positive.tell
        | F2_disentailed -> incremental_closure qp f1.positive.tell
        | Unknown_entailment -> qp, Some f
        end
    | PImply(f1, f2) when entailment qp f1.positive.ask ->
        incremental_closure qp f2.positive.tell
    | PImply(f1, _) when entailment qp f1.negative.ask -> qp, None
    | PImply _ -> qp, Some f
    | PEquiv(f1, f2) ->
        begin match binary_entailment qp f1 f2 with
        | F1_entailed -> incremental_closure qp f2.positive.tell
        | F2_entailed -> incremental_closure qp f1.positive.tell
        | F1_disentailed -> incremental_closure qp f2.negative.tell
        | F2_disentailed -> incremental_closure qp f1.negative.tell
        | Unknown_entailment -> qp, Some f
        end

  (* Closure is performed by `Event_loop` calling `exec_task`. *)
  let closure qf = qf, false

  (* We propagate the constraint immediately.
     If the constraint is not entailed, it is added into the abstract element. *)
  let weak_incremental_closure qf c =
    let qp, c' = incremental_closure qf c in
    match c' with
    | None -> qp
    | Some c ->
      let c_idx = Parray.length qp.constraints in
      let constraints = Tools.extend_parray qp.constraints c in
      { qp with constraints;
          new_tasks=c_idx::qp.new_tasks;
          num_active_tasks=qp.num_active_tasks+1 }

  (* We could provide a split over the formula directly instead of the variables.
     For now, we rely on the split of the subdomains. *)
  let split _ = []

  (* This abstract domain has no variable, we symbolically attribute a volume corresponding to the number of active formulas. *)
  let volume qp =
    if qp.num_active_tasks = 0 then 1.
    else float_of_int qp.num_active_tasks

  let print fmt qp =
    let qf = I.to_qformula qp.prod (Parray.to_list qp.constraints) in
    Pretty_print.print_formula fmt (Rewritting.quantifier_free_of qf)

  let exec_task qp (_,c_idx) =
    (* let _ = Printf.printf "exec_task %d remaining\n" qp.num_active_tasks; flush_all () in *)
    let f = Parray.get qp.constraints c_idx in
    (* if qp.num_active_tasks = 1 then
      (Pretty_print.print_qformula Format.std_formatter (I.to_qformula qp.prod [f]); flush_all ()); *)
    let qp, f' = incremental_closure qp f in
    let constraints, entailed =
      match f' with
      | None -> qp.constraints, true
      | Some f -> Parray.set qp.constraints c_idx f, false in
    let num_active_tasks = qp.num_active_tasks - (if entailed then 1 else 0) in
    { qp with constraints; num_active_tasks; }, entailed

  let drain_events qp =
    let prod, events = P.drain_events qp.prod in
    {qp with prod}, events

  let events_of qp qf =
    let rec aux = function
      | Atom cs ->
          let concat_events acc c = acc@(P.events_of qp.prod c) in
          List.fold_left concat_events [] cs
      | PNot f -> aux f.positive.tell
      | PAnd(f1, f2) -> binary_aux f1 f2
      | POr(f1, f2) -> binary_aux f1 f2
      | PImply(f1, f2) -> binary_aux f1 f2
      | PEquiv(f1, f2) -> binary_aux f1 f2
    and binary_aux f1 f2 =
      (aux f1.positive.tell)@(aux f2.positive.tell)
    in List.sort_uniq compare (aux qf)

  let drain_tasks qp =
    let drain_one acc c_idx =
      let c = Parray.get qp.constraints c_idx in
      let events = events_of qp c in
      ((qp.uid, c_idx), events)::acc in
    let tasks_events = List.fold_left drain_one [] qp.new_tasks in
    ({ qp with new_tasks=[] }, tasks_events)

  let interpretation qp = qp.prod
  let map_interpretation qp f = {qp with prod=(f qp.prod)}

  let qinterpret qp approx qf =
    let f = Rewritting.quantifier_free_of qf in
    try
      let (prod, fs) = I.interpret qp.prod approx f in
      List.fold_left weak_incremental_closure {qp with prod} fs
    with Wrong_modelling msg ->
      raise (Wrong_modelling (
        "[" ^ name ^ "] None of the subdomains of this product could interpret the constraint:\n" ^ (Tools.indent msg)))
end
