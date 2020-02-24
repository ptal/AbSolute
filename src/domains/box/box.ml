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
module Var_store = Var_store

open Core
open Core.Kleene
open Domains.Abstract_domain
open Domains.Interpretation
open Lang.Ast
open Typing.Ad_type
open Typing.Tast
open Bounds
open Vardom
open Box_interpretation

module type Box_sig =
sig
  module Vardom: Vardom_sig.S
  type vardom = Vardom.t
  include Abstract_domain

  (** `project_vardom box v` projects the domain of the variable `v`. *)
  val project_vardom: t -> I.var_id -> vardom
end

module type Box_functor = functor (B: Bound_sig.S) -> Box_sig
  with module B = B and module Vardom.B = B

module Make
  (B: Bound_sig.S)
  (VARDOM: Vardom_sig.Vardom_functor)
  (SPLIT: Box_split.Box_split_sig) =
struct
  module Vardom = VARDOM(B)
  module I = Box_interpretation(Vardom)
  module Store = I.Store
  module Split = SPLIT(I)
  module V = Vardom
  module B = V.B
  type vardom = V.t

  type t = {
    r: I.t;
    split_data: Split.t;
    strategy: search_strategy;
    init_strategy: bool;
    store: Store.t;
  }

  let interpretation box = box.r
  let map_interpretation box f =
    let (r, a) = f box.r in
    {box with r}, a

  let entailment box ((vid,v) as c) =
    try
      let store = Store.set box.store vid v in
      box, c, box.store == store
    with Bot.Bot_found -> box, c, false

  let empty uid = {
    r = I.empty uid;
    store=Store.empty;
    split_data=[];
    strategy=Simple;
    init_strategy=false;
  }

  let uid box = I.uid box.r

  let name = "Box(" ^ V.name ^ ")"

  let type_of box = Some (uid box, Box (V.type_of ()))

  let interpret box approx tqf =
    let rec aux box = function
      | TQFFormula tf ->
          let r, cs = I.interpret box.r approx tf in
          {box with r}, cs
      | TExists(tv, tqf) ->
          guarded_extend box (uid box) name tv (fun box tv ->
            let (store, idx, aty) = Store.extend ~ty:(tv.ty) box.store in
            let r = I.extend box.r (idx, {tv with ty = Abstract aty}) in
            aux {box with r; store} tqf
          )
    in aux box tqf

  let print_var fmt box idx vardom =
    let vname = I.to_logic_var' box.r idx in
    (* if vname.[0] = 's' then *)
    Format.fprintf fmt "%s=%a " vname V.print vardom

  let print fmt box =
    Store.iter (print_var fmt box) box.store;
    Format.fprintf fmt "\n"

  let project_vardom box v = Store.get box.store v

  let project box v = V.to_range (project_vardom box v)

  type snapshot = t
  let lazy_copy box n =
    List.map (fun s -> { box with store=s }) (Store.lazy_copy box.store n)
  let restore _ s =
    (* print Format.std_formatter s; *) s

  let volume box =
    let range (l,h) =
      if B.equal l h then B.one
      else B.add_up (B.sub_up h l) B.one in
    let size vardom = range (V.to_range vardom) in
    let vol = B.to_float_up (Store.fold
      (fun acc _ vardom -> B.mul_up (size vardom) acc) B.one box.store) in
    if classify_float vol = FP_infinite || classify_float vol = FP_nan then
      infinity
    else
      vol

  (** A box is always directly closed when adding the constraint. *)
  let closure box = box

  let weak_incremental_closure box (vid, v) =
    let store = Store.set box.store vid v in
    (* Printf.printf "inc(Box): "; print_var Format.std_formatter box vid v; Printf.printf "\n"; *)
    { box with store }

  let embed box v (l,u) =
    let vardom = V.of_bounds' (l,u) in
    (* Format.printf "Box.embed %a\n" V.print vardom; *)
    weak_incremental_closure box (v, vardom)

  let state _ = True

  let init_strategy box strategy =
    if box.strategy == strategy && box.init_strategy then box
    else
      let vids =
        match strategy with
        | Simple ->
            let vids = Store.fold (fun vids vid _ -> vid::vids) [] box.store in
            List.rev vids
        | VarView vars ->
            List.map (fun v -> fst (I.to_abstract_var box.r v)) vars
        | Sequence _ -> raise (Wrong_modelling "Box.split: The sequence strategy is not supported.") in
      { box with split_data=vids; strategy; init_strategy=true }

  let split ?strategy:(strat=Simple) box =
    let box = init_strategy box strat in
    let split_data, branches = Split.split box.split_data box.r box.store in
    (* print Format.std_formatter box; *)
    (* Printf.printf "Box.Split %d (split_data: %d): " (List.length branches) (List.length box.split_data); flush_all (); *)
    let boxes = lazy_copy { box with split_data } (List.length branches) in
    let branches = List.flatten (List.map2 (fun box branch ->
      (* let _ = Format.printf "%a \\/ " Lang.Pretty_print.print_formula (tformula_to_formula (quantifier_free_of (I.to_qformula box.r [branch]))); flush_all () in *)
      try [weak_incremental_closure box branch]
      with Bot.Bot_found -> []) boxes branches) in
    (* Printf.printf "\n"; *)
    branches

  let make_events box vars : event list =
    List.map (fun v -> (uid box, v)) vars

  let has_changed box =
    match Store.delta box.store with
    | _, [] -> false
    | _, _ -> true

  let drain_events box =
    let store, deltas = Store.delta box.store in
    (* Printf.printf "Box %d events drained (%d variables).\n" (List.length deltas) (Store.length store); *)
    { box with store }, (make_events box deltas)

  let events_of box (vid,_) = make_events box [vid]
  let events_of_var box vid = make_events box [vid]
end

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor =
  functor (B: Bound_sig.S) -> Make(B)(Itv.Itv)(SPLIT)
