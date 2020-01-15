(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

module Box_split = Box_split
module Box_interpretation = Box_interpretation
module Hc4 = Hc4
module Var_store = Var_store

open Core
open Core.Kleene
open Lang
open Typing
open Typing.Ad_type
open Typing.Tast
open Bounds
open Vardom
open Box_interpretation
open Event_loop.Event_abstract_domain

module type Box_sig =
sig
  module Vardom: Vardom_sig.Vardom_sig
  type vardom = Vardom.t
  include Schedulable_abstract_domain
  val project_vardom: t -> I.var_id -> vardom
end

module type Box_functor = functor (B: Bound_sig.S) -> Box_sig with module Vardom.B = B

module Make
  (B: Bound_sig.S)
  (VARDOM: Vardom_sig.Vardom_functor)
  (SPLIT: Box_split.Box_split_sig) =
struct
  module Vardom = VARDOM(B)
  module I = Box_interpretation(Vardom)
  module Store = I.Store
  module Closure = Hc4.Make(I)
  module Split = SPLIT(I)
  module V = Vardom
  module B = V.B
  type vardom = V.t

  type t = {
    uid: ad_uid;
    r: I.t;
    store: Store.t;
    constraints: I.rconstraint Parray.t;
    (* Store the new constraint's indices since last call to `drain_tasks`. *)
    new_tasks: int list;
    num_active_tasks: int;
  }

  let interpretation box = box.r
  let map_interpretation box f = {box with r=(f box.r)}

  (* Reexported functions from the parametrized modules. *)
  let entailment box = Closure.entailment box.store

  let empty uid = {
    uid;
    r = I.empty uid;
    store=Store.empty;
    constraints = Tools.empty_parray ();
    new_tasks = [];
    num_active_tasks = 0;
  }

  let uid box = box.uid

  let name = "Box(" ^ V.name ^ ")"

  let type_of box = Some (box.uid, Box (V.type_of ()))

  let interpret box approx tqf =
    let rec aux box = function
      | TQFFormula tf ->
          let r, cs = I.interpret box.r approx tf in
          {box with r}, cs
      | TExists(tv, tqf) ->
          let (store, idx, aty) = Store.extend ~ty:(tv.ty) box.store in
          let r = I.extend box.r (idx, {tv with ty = Abstract aty}) in
          aux {box with r; store} tqf
    in aux box tqf

  let project_vardom box v = Store.get box.store v

  let project box v = V.to_range (project_vardom box v)

  type snapshot = t
  let lazy_copy box n = List.map (fun s -> { box with store=s }) (Store.lazy_copy box.store n)
  let restore _ s = s

  let volume box =
    let range (l,h) =
      if B.equal l h then B.one
      else B.add_up (B.sub_up h l) B.one in
    let size vardom = range (V.to_range vardom) in
    let vol = B.to_float_up (Store.fold (fun acc _ vardom -> B.mul_up (size vardom) acc) B.one box.store) in
    if classify_float vol = FP_infinite || classify_float vol = FP_nan then
      infinity
    else
      vol

  let print_store fmt (repr,store) =
    let print_entry idx vardom =
      Format.fprintf fmt "%s=%a \n" (I.to_logic_var' repr idx) V.print vardom in
    Store.iter print_entry store

  let print_box_cons fmt f =
    let f = Tast.quantifier_free_of f in
    Format.fprintf fmt "%a\n" Pretty_print.print_formula
      (Tast.tformula_to_formula f)

  let print fmt box =
    Format.fprintf fmt "%a\n" print_store (box.r,box.store);
    Parray.iter (fun c -> print_box_cons fmt (I.to_qformula box.r [c])) box.constraints

  (** Closure is performed by `Event_loop` calling `exec_task`. *)
  let closure box = box, false

  (** We propagate the constraint immediately.
      If the constraint is not entailed, it is added into the box. *)
  let weak_incremental_closure box c =
    (* let _ = Format.printf "%a\n" print_box_cons (I.to_qformula box.r [c]); flush_all () in *)
    (* Format.fprintf Format.std_formatter "%a\n" print_store (box.r,box.store); *)
    let store, entailed = Closure.incremental_closure box.store c in
    let box = { box with store } in
    if entailed then box
    else
      let c_idx = Parray.length box.constraints in
      let constraints = Tools.extend_parray box.constraints c in
      { box with constraints;
          new_tasks=c_idx::box.new_tasks;
          num_active_tasks=box.num_active_tasks+1 }

  (* Entailed constraints are automatically deactivated by `Event_loop`. *)
  let state box =
    if box.num_active_tasks = 0 then True
    else Unknown

  let split box =
    let branches = Split.split box.store in
    (* Printf.printf "Box.Split %d\n" (List.length branches); flush_all (); *)
    let boxes = lazy_copy box (List.length branches) in
    (* We remove the branch that are unsatisfiable. *)
    List.flatten (List.map2 (fun box branch ->
      (* let _ = Format.printf "%a\n" print_box_cons (I.to_qformula box.r [branch]); flush_all () in *)
      try [weak_incremental_closure box branch]
      with Bot.Bot_found -> (* Printf.printf "unsat\n"; flush_all (); *) []) boxes branches)

  let exec_task box (_,c_idx) =
    let store, entailed = Closure.incremental_closure box.store
      (Parray.get box.constraints c_idx) in
    let num_active_tasks = box.num_active_tasks - (if entailed then 1 else 0) in
    { box with store; num_active_tasks }, entailed

  let make_events box vars : event list =
    List.map (fun v -> (box.uid, v)) vars

  let drain_events box =
    let store, deltas = Store.delta box.store in
    { box with store }, (make_events box deltas)

  let events_of box c =
    let vars = List.sort_uniq compare (I.vars_of_constraint c) in
    make_events box vars

  let drain_tasks box =
    let drain_one acc c_idx =
      let c = Parray.get box.constraints c_idx in
      let events = events_of box c in
      ((box.uid, c_idx), events)::acc in
    let tasks_events = List.fold_left drain_one [] box.new_tasks in
    ({ box with new_tasks=[] }, tasks_events)

  (* type t' = t *)
(*
  include QInterpreter_base(struct
    type t=t'
    module I=I
    let name=name
    let interpretation=interpretation
    let map_interpretation=map_interpretation
    let extend=extend
    let weak_incremental_closure=weak_incremental_closure end) *)
end

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor =
  functor (B: Bound_sig.S) -> Make(B)(Itv.Itv)(SPLIT)
