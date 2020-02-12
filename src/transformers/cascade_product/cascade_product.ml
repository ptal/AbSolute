(* Copyright 2020 Pierre Talbot

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

module type Cascade_product_interpretation_sig =
sig
  module A: Abstract_domain
  module B: Abstract_domain
  type t = {
    uid: ad_uid;
    a: A.t ref;
    b: B.t ref;
  }
  type var_id = unit
  type rconstraint

  val exact_interpretation: bool
  val name: string
  val empty: ad_uid -> t
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> Ast.vname -> (var_id * Tast.tvariable)
  val local_vars: t -> vname -> var_id list
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> Tast.tqformula
end

let no_variable_exn msg = no_variable_exn msg; failwith "unreachable"

module Cascade_product_interpretation(A: Abstract_domain)(B: Abstract_domain) =
struct
  module A = A
  module B = B
  type t = {
    uid: ad_uid;
    a: A.t ref;
    b: B.t ref;
  }
  type var_id = unit

  type rconstraint = {
    ac: A.I.rconstraint;
    tf: tformula;
    (** A copy of the logical constraint that will be handled in B when instantiated enough. *)

    approx: approx_kind;
    avars_only: (A.I.var_id * tvariable) list;
    (** The variable names are the ones not in B.  *)

    bc: B.I.rconstraint list;
  }

  let exact_interpretation = A.I.exact_interpretation && B.I.exact_interpretation
  let name = "Cascade_product(" ^ A.name ^ ", " ^ B.name ^ ")"

  let empty _ = raise (Wrong_modelling "`Cascade_product_interpretation.empty` is not supported, you should first create the abstract domains and then create the `Cascade_product`.")
  let to_logic_var _ _ = no_variable_exn "Cascade_product_interpretation.to_logic_var"
  let to_abstract_var _ _ = no_variable_exn "Cascade_product_interpretation.to_abstract_var"
  let local_vars _ _ = no_variable_exn "Cascade_product_interpretation.local_vars"

  (** Prepare as much as possible to interpret the constraint in `B` when instantiated enough. *)
  let lazy_interpret_in_b approx r ac =
    let not_in_b r tv =
      try
        ignore(B.I.to_abstract_var (B.interpretation !(r.b)) tv.name);
        false
      with Not_found -> true in
    let tqf = A.I.to_qformula (A.interpretation !(r.a)) [ac] in
    let avars = quantifiers tqf in
    let avars_only = List.filter (not_in_b r) avars in
    let avars_only = List.map (
      fun tv -> A.I.to_abstract_var (A.interpretation !(r.a)) tv.name) avars_only in
    { ac; tf=(quantifier_free_of tqf); approx; avars_only; bc=[]}

  let interpret_in_a approx r tf =
    let tf_a = replace_uid (A.uid !(r.a)) tf in
    let (a, a_constraints) = A.map_interpretation !(r.a) (fun i ->
      A.I.interpret i approx tf_a) in
    r.a := a;
    r, a_constraints

  let interpret r approx tf =
    guarded_interpret r r.uid name tf (fun r tf ->
      let (r, a_constraints) = interpret_in_a approx r tf in
      r, List.map (lazy_interpret_in_b approx r) a_constraints
    )

  (* This function could be more precise if we verify if the constraint is in B instead of considering all constraints in A. *)
  let to_qformula r cs =
    A.I.to_qformula (A.interpretation !(r.a)) (List.map (fun c -> c.ac) cs)
end

module Cascade_product(A: Abstract_domain)(B: Abstract_domain) =
struct
  module I = Cascade_product_interpretation(A)(B)

  open I

  type t = {
    repr: I.t;
    constraints: I.rconstraint Parray.t;
    (* Store the new constraint's indexes since last call to `drain_tasks`. *)
    new_tasks: int list;
    num_active_tasks: int;
  }

  let empty _ = raise (Wrong_modelling ("Cascade_product must be initialized with `init`."))
  let name = I.name

  let init repr = {
    repr;
    constraints = Tools.empty_parray ();
    new_tasks = [];
    num_active_tasks = 0;
  }

  let uid cp = cp.repr.uid

  let unwrap_a cp = !(cp.repr.a)
  let unwrap_b cp = !(cp.repr.b)
  let wrap_a cp a = cp.repr.a := a; cp
  let wrap_b cp b = cp.repr.b := b; cp

  let type_of cp =
    match A.type_of (unwrap_a cp), B.type_of (unwrap_b cp) with
    | Some t1, Some t2 -> Some (uid cp, Cascade_product (t1,t2))
    | _ -> raise (Wrong_modelling
        "The two domains underlying the cascade product must not be meta-domains (they must have a type).")

  (* State is ultimately decided by the sub-domains as this domain does not owned variables. *)
  let state _ = True
  let split _ = []
  let volume _ = 1.
  let print _ _ = ()

  let project _ _ = no_variable_exn "Cascade_product.project"
  let embed _ _ _ = no_variable_exn "Cascade_product.embed"

  (* Closure is performed by `Event_loop` calling `exec_task`. *)
  let closure cp = cp, false

  let interpretation cp = cp.repr
  let map_interpretation cp f =
    let (repr, a) = f cp.repr in
    {cp with repr}, a

  (* This abstract domain is totally functional. *)
  type snapshot = t
  let lazy_copy cp n = List.init n (fun _ -> cp)
  let restore _ s = s

  (* Classify the variables of the constraint `c` according to their instantiation status in `A`. *)
  let classify_a_var cp c =
    let aux (uninstantiated, fixed) (vid, tv) =
      let (l,u) = A.project (unwrap_a cp) vid in
      if A.B.equal l u then
        (uninstantiated, (tv.name, (tv,A.B.to_rat l))::fixed)
      else
        ((vid,tv)::uninstantiated, fixed) in
    List.fold_left aux ([],[]) c.avars_only

  (* Given a constraint `c` and the set of instantiated variables, interpret the constraint `c` in `B`. *)
  let lazy_transfer_a_to_b cp c fixed =
    let c = { c with tf=(instantiate_vars fixed c.tf) } in
    let b, bc = B.map_interpretation (unwrap_b cp) (fun i -> B.I.interpret i c.approx c.tf) in
    wrap_b cp b, { c with avars_only=[]; bc}

  (* Try to interpret `c` in `B` if all the variables in `A` only are instantiated. *)
  let try_lazy_transfer cp c =
    (* If the constraint has not yet been transfered to `B`. *)
    if List.length c.bc = 0 then
      let uninstantiated, fixed = classify_a_var cp c in
      if List.length uninstantiated = 0 then
        lazy_transfer_a_to_b cp c fixed, true
      else
        (cp, { c with avars_only=uninstantiated }), false
    else
      (cp, c), true

  let entailment_a cp c =
    let a = unwrap_a cp in
    let a, ac, is_entailed = A.entailment a c.ac in
    wrap_a cp a, {c with ac}, is_entailed

  let entailment_b cp c =
    let aux (b, bc) c =
      let b, c, is_entailed = B.entailment b c in
      if is_entailed then (b, bc)
      else (b, c::bc) in
    let b = unwrap_b cp in
    let b, bc = List.fold_left aux (b,[]) c.bc in
    wrap_b cp b, { c with bc }, (List.length bc = 0)

  let entailment cp c =
    let (cp, c), is_transferred = try_lazy_transfer cp c in
    if is_transferred then
      entailment_b cp c
    else
      entailment_a cp c

  (* Try to transfer `c` in `B`, and upon success commit the transfer in `B` using `weak_incremental_closure`. *)
  let incremental_closure cp c =
    let (cp, c), is_transferred = try_lazy_transfer cp c in
    if is_transferred then
      let b = List.fold_left B.weak_incremental_closure (unwrap_b cp) c.bc in
      wrap_b cp b, c, true
    else
      cp, c, false

  let weak_incremental_closure cp c =
    let cp, c, is_transferred = incremental_closure cp c in
    if is_transferred then
      cp
    else
      let a = A.weak_incremental_closure (unwrap_a cp) c.ac in
      let cp = wrap_a cp a in
      let c_idx = Parray.length cp.constraints in
      let constraints = Tools.extend_parray cp.constraints c in
      { cp with constraints;
          new_tasks=c_idx::cp.new_tasks;
          num_active_tasks=cp.num_active_tasks+1 }

  let drain_events cp = cp, []

  let events_of cp c = List.flatten (
    List.map (fun (vid,_) -> A.events_of_var (unwrap_a cp) vid) c.avars_only)

  let events_of_var _ _ = []

  let exec_task cp (_,c_idx) =
    let c = Parray.get cp.constraints c_idx in
    let cp, c, entailed = incremental_closure cp c in
    let constraints =
      if entailed then cp.constraints
      else Parray.set cp.constraints c_idx c in
    let num_active_tasks = cp.num_active_tasks - (if entailed then 1 else 0) in
    { cp with constraints; num_active_tasks; }, entailed

  let drain_tasks cp =
    let drain_one acc c_idx =
      let c = Parray.get cp.constraints c_idx in
      let events = events_of cp c in
      ((uid cp, c_idx), events)::acc in
    let tasks_events = List.fold_left drain_one [] cp.new_tasks in
    ({ cp with new_tasks=[] }, tasks_events)

  let interpret cp approx = function
  | TExists (_, _) -> no_variable_exn "Cascade_product.interpret"
  | TQFFormula tf ->
      let (repr, cs) = I.interpret cp.repr approx tf in
      { cp with repr }, cs

  module B = Bound_unit
end
