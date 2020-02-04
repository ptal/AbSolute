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
open Core.Kleene
open Bounds
open Lang
open Lang.Ast
open Typing
open Typing.Tast
open Typing.Ad_type
open Domains.Interpretation
open Domains.Abstract_domain

module type Logic_completion_interpretation_sig =
sig
  module A: Abstract_domain
  type t = {
    a: A.t ref;
    uid: ad_uid;
  }
  type var_id = unit
  type rconstraint

  val exact_interpretation: bool
  val empty: ad_uid -> t
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> Ast.vname -> (var_id * Tast.tvariable)
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> Tast.tqformula
end

let no_variable_exn msg = no_variable_exn msg; failwith "unreachable"

module Logic_completion_interpretation(A: Abstract_domain) =
struct
  module A = A
  type t = {
    a: A.t ref;
    uid: ad_uid;
  }
  type var_id = unit

  (** For a single logic formula, we have 4 variants of the formula.
    We always keep the formula and its negation.
    Then, for both of these, we have a version for entailment querying (ask) and for joining the constraint in the abstract element (tell).
    Note that ask and tell formulas are tightly bound to under- and over-approximation.
    Alternatively, we could just compute these variants on the fly in `incremental_closure` and `entailment`.
    This version is more efficient because it precomputes these variants.

    This structure could be simplified by rewritting `PImply` and `PEquiv`.
    For now, we prefer to keep the structure of the formula as much as possible, but I am unsure that if it is more efficient or not. *)
  type rconstraint =
    | Atom of A.I.rconstraint list
    | PNot of pn_formula
    | PAnd of pn_formula * pn_formula
    | POr of pn_formula * pn_formula
    | PImply of pn_formula * pn_formula
    | PEquiv of pn_formula * pn_formula
  and approx_formula = {
    ask: rconstraint; (** Entailment query of a formula by under-approximation. *)
    tell: rconstraint; (** Join of a formula by over-approximation (or under-approximation depending on the user choice). *)
  }
  and pn_formula = {
    positive: approx_formula;
    negative: approx_formula;
  }

  let exact_interpretation = A.I.exact_interpretation

  let empty _ = raise (Wrong_modelling "`Logic_completion_interpretation.empty` is not supported, you should first create the abstract domains and then create the `Logic_completion`.")
  let to_logic_var _ _ = no_variable_exn "Logic_completion_interpretation.to_logic_var"
  let to_abstract_var _ _ = no_variable_exn "Logic_completion_interpretation.to_abstract_var"

  let wrap r a = r.a := a; r

  (* let c = ref 0 *)

  let interpret r approx tf =
    let forward_or r (uid, f) map_atom or_f =
      (* NOTE: It is important to check `r.uid <> uid` instead of `uid = A.uid !(r.a)`,
         because `a` might contain other sub-domains in which `f` is typed. *)
      if r.uid <> uid then
        let i, fs = A.I.interpret (A.interpretation !(r.a)) approx (uid, f) in
        let a = A.map_interpretation !(r.a) (fun _ -> i) in
        wrap r a, map_atom (Atom fs)
      else
        or_f ()
    in
    let rec make_approx_formula r approx tf =
      if exact_interpretation && approx = Exact then
        let r, c = aux r approx tf in
        r, {ask=c; tell=c}
      else
        let r, ask = aux r UnderApprox tf in
        let r, tell = aux r approx tf in
        r, { ask; tell }
    and make_pn_formula r approx tf =
      let r, positive = make_approx_formula r approx tf in
      let r, negative = make_approx_formula r approx (Tast.neg_formula r.uid tf) in
      r, { positive; negative }
    and aux r approx tf =
      forward_or r tf (fun x -> x) (fun () ->
        match snd tf with
        (* Literals and constraints are the base cases that should not have the logic completion UID. *)
        | TNot((_,TFVar _)) | TFVar _ | TCmp _ -> raise (Wrong_modelling
            "[Logic completion] Bad typing: Literals and constraints should not by typed as logic completion (this abstract domain only manage logic formulas.")
        | TNot(tf1) ->
            let r, f1 = make_pn_formula r approx tf1 in
            r, PNot f1
        | TAnd(tf1, tf2) -> aux_binary r tf1 tf2 (fun tf1 tf2 -> PAnd(tf1,tf2))
        | TEquiv(tf1, tf2) -> aux_binary r tf1 tf2 (fun tf1 tf2 -> PEquiv(tf1,tf2))
        | TImply(tf1, tf2) -> aux_binary r tf1 tf2 (fun tf1 tf2 -> PImply(tf1,tf2))
        | TOr(tf1, tf2) -> aux_binary r tf1 tf2 (fun tf1 tf2 -> POr(tf1,tf2)))
    and aux_binary r tf1 tf2 make =
      let r, f1 = make_pn_formula r approx tf1 in
      let r, f2 = make_pn_formula r approx tf2 in
      r, make f1 f2 in
    let rec top_aux r approx tf =
      forward_or r tf (fun x -> [x]) (fun () ->
        match snd tf with
        | TAnd(tf1, tf2) ->
            let r, f1 = top_aux r approx tf1 in
            let r, f2 = top_aux r approx tf2 in
            r, f1@f2
        | _ ->
            let r, f = aux r approx tf in r, [f])
    in
      (* let _ = Printf.printf "Interpreted %d constraints in logic completion: (%s) %s.\n" (c := !c + 1; !c) (string_of_approx approx) (Lang.Pretty_print.string_of_formula (tformula_to_formula tf)); flush_all () in *)
      top_aux r approx tf

  let to_qformula r fs =
    let rec aux = function
      | Atom cs -> A.I.to_qformula (A.interpretation !(r.a)) cs
      | PNot f1 -> map_tformula (fun f -> (r.uid, TNot f)) (aux' f1)
      | PAnd(f1,f2) -> binary_aux f1 f2 (fun f1 f2 -> (r.uid, TAnd(f1,f2)))
      | POr(f1,f2) -> binary_aux f1 f2 (fun f1 f2 -> (r.uid, TOr(f1,f2)))
      | PImply(f1,f2) -> binary_aux f1 f2 (fun f1 f2 -> (r.uid, TImply(f1,f2)))
      | PEquiv(f1,f2) -> binary_aux f1 f2 (fun f1 f2 -> (r.uid, TEquiv(f1,f2)))
    and aux' f1 = aux f1.positive.tell
    and binary_aux f1 f2 make =
      merge_formula make (aux' f1) (aux' f2)
    in q_conjunction r.uid (List.map aux fs)
end

module Logic_completion(A: Abstract_domain) =
struct
  module B = Bound_unit
  module I = Logic_completion_interpretation(A)

  open I

  type t = {
    repr: I.t;
    constraints: I.rconstraint Parray.t;
    (* Store the new constraint's indices since last call to `drain_tasks`. *)
    new_tasks: int list;
    num_active_tasks: int;
  }

  let wrap lc a = lc.repr.a := a; lc
  let unwrap lc = !(lc.repr.a)

  let empty _ = raise (Wrong_modelling ("Logic_completion must be initialized with `init`."))

  let init repr = {
    repr;
    constraints = Tools.empty_parray ();
    new_tasks = [];
    num_active_tasks = 0;
  }

  let uid lc = lc.repr.uid

  let name = "Logic_completion(" ^ A.name ^ ")"

  let type_of lc =
    match A.type_of (unwrap lc) with
    | Some t -> Some (uid lc, Logic_completion t)
    | None -> raise (Wrong_modelling
        "The domain underlying logic completion must not be a meta-domain (it must have a type).")

  (* Entailed constraints are automatically deactivated by `Event_loop`. *)
  let state lc =
    if lc.num_active_tasks = 0 then True
    else Unknown

  let project _ _ = no_variable_exn "Logic_completion.project"
  let embed _ _ _ = no_variable_exn "Logic_completion.embed"

  (* This abstract domain is totally functional. *)
  type snapshot = t
  let lazy_copy lc n = List.init n (fun _ -> lc)
  let restore _ s = s

  let rec entailment lc = function
    | Atom cs -> List.for_all (A.entailment (unwrap lc)) cs
    | PNot f1 -> entailment lc f1.negative.ask
    | PAnd(f1, f2) when entailment lc f1.positive.ask ->
        entailment lc f2.positive.ask
    | PAnd(f1, f2) when entailment lc f2.positive.ask ->
        entailment lc f1.positive.ask
    | POr(f1, _) when entailment lc f1.positive.ask -> true
    | POr(_, f2) when entailment lc f2.positive.ask -> true
    | PImply(f1, f2) when entailment lc f1.positive.ask ->
        entailment lc f2.positive.ask
    | PImply(f1, _) when entailment lc f1.negative.ask -> true
    | PEquiv(f1, f2) when entailment lc f1.positive.ask ->
        entailment lc f2.positive.ask
    | PEquiv(f1, f2) when entailment lc f1.negative.ask ->
        entailment lc f2.negative.ask
    | _ -> false

  type entailment_cases =
    | F1_entailed | F2_entailed
    | F1_disentailed | F2_disentailed
    | Unknown_entailment

  let binary_entailment lc f1 f2 =
    if entailment lc f1.positive.ask then F1_entailed
    else if entailment lc f2.positive.ask then F2_entailed
    else if entailment lc f1.negative.ask then F1_disentailed
    else if entailment lc f2.negative.ask then F2_disentailed
    else Unknown_entailment

  (* `None` if the constraint `f` is entailed.
     `Some c` if the constraint is not yet entailed, and has been rewritten to `c`. *)
  let rec incremental_closure lc f =
    let map_tell pn_f tell =
      {pn_f with positive={pn_f.positive with tell}} in
    match f with
    | Atom cs ->
        let a = List.fold_left A.weak_incremental_closure (unwrap lc) cs in
        wrap lc a, None
    | PNot f1 -> incremental_closure lc f1.negative.tell
    | PAnd(f1,f2) ->
        let lc, qf1 = incremental_closure lc f1.positive.tell in
        let lc, qf2 = incremental_closure lc f2.positive.tell in
        begin match qf1, qf2 with
        | None, None -> lc, None
        | Some t1, None -> lc, Some t1
        | None, Some t2 -> lc, Some t2
        | Some t1, Some t2 -> lc, Some(PAnd(map_tell f1 t1, map_tell f2 t2))
        end
    | POr(f1, f2) ->
        begin match binary_entailment lc f1 f2 with
        | F1_entailed | F2_entailed -> lc, None
        | F1_disentailed -> incremental_closure lc f2.positive.tell
        | F2_disentailed -> incremental_closure lc f1.positive.tell
        | Unknown_entailment -> lc, Some f
        end
    | PImply(f1, f2) when entailment lc f1.positive.ask ->
        incremental_closure lc f2.positive.tell
    | PImply(f1, _) when entailment lc f1.negative.ask -> lc, None
    | PImply _ -> lc, Some f
    | PEquiv(f1, f2) ->
        begin match binary_entailment lc f1 f2 with
        | F1_entailed -> incremental_closure lc f2.positive.tell
        | F2_entailed -> incremental_closure lc f1.positive.tell
        | F1_disentailed -> incremental_closure lc f2.negative.tell
        | F2_disentailed -> incremental_closure lc f1.negative.tell
        | Unknown_entailment -> lc, Some f
        end

  (* Closure is performed by `Event_loop` calling `exec_task`. *)
  let closure lc = lc, false

  (* We propagate the constraint immediately.
     If the constraint is not entailed, it is added into the abstract element. *)
  let weak_incremental_closure lc c =
    let lc, c' = incremental_closure lc c in
    match c' with
    | None -> lc
    | Some c ->
      let c_idx = Parray.length lc.constraints in
      let constraints = Tools.extend_parray lc.constraints c in
      { lc with constraints;
          new_tasks=c_idx::lc.new_tasks;
          num_active_tasks=lc.num_active_tasks+1 }

  (* We could provide a split over the formula directly instead of the variables.
     For now, we rely on the split of the subdomains. *)
  let split _ = []

  (* This abstract domain has no variable, we symbolically attribute a volume corresponding to the number of active formulas. *)
  let volume lc =
    if lc.num_active_tasks = 0 then 1.
    else float_of_int lc.num_active_tasks

  (* let print _ _ = () *)
  let print fmt lc =
    let tf = I.to_qformula lc.repr (Parray.to_list lc.constraints) in
    let f = Tast.tformula_to_formula (Tast.quantifier_free_of tf) in
    Pretty_print.print_formula fmt f

  let exec_task lc (_,c_idx) =
    (* let _ = Printf.printf "exec_task %d remaining\n" lc.num_active_tasks; flush_all () in *)
    let f = Parray.get lc.constraints c_idx in
    (* if lc.num_active_tasks = 1 then
      (Pretty_print.print_qformula Format.std_formatter (I.to_qformula lc.prod [f]); flush_all ()); *)
    let lc, f' = incremental_closure lc f in
    let constraints, entailed =
      match f' with
      | None -> lc.constraints, true
      | Some f -> Parray.set lc.constraints c_idx f, false in
    let num_active_tasks = lc.num_active_tasks - (if entailed then 1 else 0) in
    { lc with constraints; num_active_tasks; }, entailed

  let drain_events lc =
    let a, events = A.drain_events (unwrap lc) in
    wrap lc a, events

  let events_of lc qf =
    let rec aux = function
      | Atom cs ->
          let concat_events acc c = acc@(A.events_of (unwrap lc) c) in
          List.fold_left concat_events [] cs
      | PNot f -> aux f.positive.tell
      | PAnd(f1, f2) -> binary_aux f1 f2
      | POr(f1, f2) -> binary_aux f1 f2
      | PImply(f1, f2) -> binary_aux f1 f2
      | PEquiv(f1, f2) -> binary_aux f1 f2
    and binary_aux f1 f2 =
      (aux f1.positive.tell)@(aux f2.positive.tell)
    in List.sort_uniq compare (aux qf)

  let events_of_var _ _ = []

  let drain_tasks lc =
    let drain_one acc c_idx =
      let c = Parray.get lc.constraints c_idx in
      let events = events_of lc c in
      ((uid lc, c_idx), events)::acc in
    let tasks_events = List.fold_left drain_one [] lc.new_tasks in
    ({ lc with new_tasks=[] }, tasks_events)

  let interpretation lc = lc.repr
  let map_interpretation lc f = { lc with repr=(f lc.repr) }

  let interpret lc approx = function
  | TExists (_, _) -> no_variable_exn "Logic_completion.interpret"
  | TQFFormula tf ->
      let (repr, fs) = I.interpret lc.repr approx tf in
      { lc with repr }, fs
end
