(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(* Important note: This module is not functional due to the references to the abstract domains.
   It means that whenever an exception is raised, we must restore `pa`.
   Hence, for every function that modifies the abstract domain before an exception might occur, use `safe_wrap`. *)

open Domains.Abstract_domain
open Domains.Interpretation
open Lang.Ast
open Typing
open Typing.Ad_type
open Typing.Tast
open Core
open Bounds

type gvar = ad_uid * int
type gconstraint = ad_uid * int

module type Prod_combinator =
sig
  type init_t
  type t
  type var_id = gvar
  type rconstraint = gconstraint
  val count: int
  val name: string

  val init: init_t -> t
  val empty: unit -> t
  val exists: t -> vname -> bool
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> vname -> (var_id * Tast.tvariable)
  val to_qformula: t -> gconstraint list -> tqformula list
  val interpret_all: t -> approx_kind -> tformula -> t * gconstraint list
  val interpret_one: t -> approx_kind -> tformula -> t * gconstraint list
  val extend_var_all: t -> approx_kind -> tvariable -> t * gconstraint list
  val extend_var_one: t -> approx_kind -> tvariable -> t * gconstraint list

  val empty': ad_uid -> t
  val type_of: t -> ad_ty list
  val project: t -> gvar -> (Bound_rat.t * Bound_rat.t)
  type snapshot
  val restore: t -> snapshot -> t
  val lazy_copy: t -> int -> snapshot list
  val closure: t -> (t * bool)
  val weak_incremental_closure: t -> gconstraint -> t
  val entailment: t -> gconstraint -> bool
  val split: t -> snapshot list
  val volume: t -> float
  val state: t -> Kleene.t
  val print: Format.formatter -> t -> unit
  val drain_events: t -> (t * event list)
  val events_of: t -> rconstraint -> event list
end

module Prod_atom(A: Abstract_domain) =
struct
  module A = A

  type init_t = A.t ref
  type t = {
    a: init_t;
    var_map: A.I.var_id list;
    constraint_map: A.I.rconstraint list;
  }
  type var_id = gvar
  type rconstraint = gconstraint

  type snapshot = {
    a_bt: A.snapshot;
    var_map_bt: A.I.var_id list;
    constraint_map_bt: A.I.rconstraint list;
  }

  let count = 1
  let name = A.name

  let unwrap pa = !(pa.a)
  let wrap pa a = pa.a := a; pa

  let type_of pa =
    match A.type_of (unwrap pa) with
    | Some t -> [t]
    | None -> []

  let restore pa snapshot =
    pa.a := A.restore !(pa.a) snapshot.a_bt;
    {pa with
      var_map=snapshot.var_map_bt;
      constraint_map=snapshot.constraint_map_bt }

  let make_snapshot pa a_bt =
    {a_bt; var_map_bt=pa.var_map; constraint_map_bt=pa.constraint_map}

  let lazy_copy pa n =
    List.map (make_snapshot pa) (A.lazy_copy !(pa.a) n)

  let safe_wrap pa f =
    let snapshots = lazy_copy pa 2 in
    try
      let pa = restore pa (List.hd snapshots) in
      f pa
    with e ->
      (ignore(restore pa (List.nth snapshots 1)); (* To restore mutable effects. *)
      raise e)

  let init a = { a; var_map=[]; constraint_map=[] }
  let empty' uid = init (ref (A.empty uid))

  let uid pa = A.uid !(pa.a)
  let interpretation pa = A.interpretation !(pa.a)

  let empty () = raise (Wrong_modelling
    "`Direct_product.I.empty`: the interpretation must be created with `init`.")

  let get_constraint pa (uid', c_id) =
    if uid pa != uid' then raise
      (Wrong_modelling "This constraint does not belong to this ordered product.")
    else
      List.nth pa.constraint_map c_id

  let exists pa v = A.I.exists (interpretation pa) v

  let to_logic_var' pa var_id =
    A.I.to_logic_var (interpretation pa) (List.nth pa.var_map var_id)

  let to_logic_var pa (uid', var_id) =
    if uid pa != uid' then raise Not_found
    else to_logic_var' pa var_id

  let to_abstract_var pa var =
    let (var_a, tv) = A.I.to_abstract_var (interpretation pa) var in
    try
      let idx = Tools.find_index var_a pa.var_map in
      ((uid pa, idx), tv)
    with Not_found -> failwith (
      "[Direct_product.Prod_atom] The variable " ^ var ^
      " is registered in the underlying abstract domain but the mapping does not exist in Prod_atom.")

  let to_abstract_constraint pa c = List.nth pa.constraint_map (snd c)
  let to_generic_constraint pa c =
    let num_cons = List.length pa.constraint_map in
    let constraint_map = pa.constraint_map@[c] in
    {pa with constraint_map}, (uid pa, num_cons)

  let to_generic_constraints pa cs =
    List.fold_left
      (fun (pa, gcons) c -> let pa, c = to_generic_constraint pa c in pa, c::gcons)
      (pa, []) cs

  let to_qformula' pa constraints =
    let these, others = List.partition (fun c -> (uid pa) = (fst c)) constraints in
    (List.map (to_abstract_constraint pa) these), others

  let do_not_belong_exn s =
    raise (Wrong_modelling ("`" ^ s ^ "` over some constraints not belonging to any domain."))

  let to_qformula pa constraints =
    let constraints, others = to_qformula' pa constraints in
    match others with
    | [] -> [A.I.to_qformula (interpretation pa) constraints]
    | _ -> do_not_belong_exn "qformula"

  let closure pa =
    let (a, has_changed) = A.closure !(pa.a) in
    wrap pa a, has_changed

  let project pa (uid', v_id) =
    if uid pa != uid' then raise Not_found
    else
      let (lb, ub) = A.project !(pa.a) (List.nth pa.var_map v_id) in
      A.B.to_rat lb, A.B.to_rat ub

  let weak_incremental_closure pa (uid', c_id) =
    if uid pa != uid' then raise (Wrong_modelling "`Direct_product.weak_incremental_closure`: this constraint does not belong to this ordered product.")
    else
      let c_a = List.nth pa.constraint_map c_id in
      let a = A.weak_incremental_closure !(pa.a) c_a in
      wrap pa a

  let state pa = A.state !(pa.a)
  let entailment pa c = A.entailment !(pa.a) (get_constraint pa c)
  let split pa = List.map (make_snapshot pa) (A.split !(pa.a))
  let volume pa = A.volume !(pa.a)
  let print fmt pa = Format.fprintf fmt "%a" A.print !(pa.a)

  let interpret pa approx tf =
    safe_wrap pa (fun pa ->
      try
        let (i, constraints) = A.I.interpret (interpretation pa) approx tf in
        let pa = wrap pa (A.map_interpretation !(pa.a) (fun _ -> i)) in
        let pa, gconstraints = to_generic_constraints pa constraints in
        pa, gconstraints
      with Wrong_modelling msg ->
        raise (Wrong_modelling ("[" ^ A.name ^ "] " ^ msg)))

  let interpret_all = interpret
  let interpret_one = interpret

  let extend_var pa approx tv =
    let (a, cs) = A.interpret !(pa.a) approx (TExists(tv,ttrue)) in
    let (atom_id, _) = A.I.to_abstract_var (interpretation pa) tv.name in
    let pa = {pa with var_map=(atom_id::pa.var_map) } in
    let pa, gcons = to_generic_constraints pa cs in
    wrap pa a, gcons

  let extend_var_all = extend_var
  let extend_var_one = extend_var

  let drain_events pa =
    let a, events = A.drain_events (unwrap pa) in
    wrap pa a, events

  let events_of pa c =
    A.events_of (unwrap pa) (get_constraint pa c)
end

module Prod_cons(A: Abstract_domain)(B: Prod_combinator) =
struct
  module Atom = Prod_atom(A)
  type var_id = gvar
  type rconstraint = gconstraint

  type init_t = Atom.init_t * B.init_t
  type t = Atom.t * B.t

  let count = Atom.count + B.count
  let name = A.name ^ "," ^ B.name

  let type_of (a,b) = (Atom.type_of a)@(B.type_of b)

  let init (a,b) = (Atom.init a, B.init b)

  let empty () = raise (Wrong_modelling
    "`Direct_product.I.empty`: the interpretation must be created with `init`.")

  let empty' uid = Atom.empty' uid, B.empty' (uid+1)

  type snapshot = Atom.snapshot * B.snapshot

  let restore (a,b) (sa,sb) =
    (Atom.restore a sa, B.restore b sb)

  let lazy_copy (a,b) n =
    let a_snapshots = Atom.lazy_copy a n in
    let b_snapshots = B.lazy_copy b n in
    List.combine a_snapshots b_snapshots

  let exists (a,b) v =
    if Atom.exists a v then true
    else B.exists b v

  let to_logic_var (a,b) ((uid, var_id) as v) =
    if Atom.uid a != uid then B.to_logic_var b v
    else
      Atom.to_logic_var' a var_id

  let to_abstract_var (a,b) var =
    try
      Atom.to_abstract_var a var
    with Not_found ->
      B.to_abstract_var b var

  (* Whenever the list of constraints is empty means that it is a tautology, so we do not explicitly represent it.  *)
  let to_qformula (a,b) constraints =
    let a_constraints, b_constraints = Atom.to_qformula' a constraints in
    if a_constraints = [] then
      B.to_qformula b b_constraints
    else
      let a_formula = Atom.A.I.to_qformula (Atom.interpretation a) a_constraints in
      if b_constraints = [] then [a_formula]
      else a_formula::(B.to_qformula b b_constraints)

  let closure (a,b) =
    let a, has_changed = Atom.closure a in
    let b, has_changed' = B.closure b in
    (a,b), has_changed || has_changed'

  let interpret_one (a,b) approx tf =
    if (Atom.uid a) = (fst tf) then
      let a, cs = Atom.interpret_one a approx tf in
      (a,b), cs
    else
      let b, cs = B.interpret_one b approx tf in
      (a,b), cs

  let interpret_all (a,b) approx tf =
    let a, cs1 = Atom.interpret_one a approx tf in
    let b, cs2 = B.interpret_one b approx tf in
    (a,b), cs1@cs2

  let extend_var_one (a,b) approx tv =
    if (Atom.uid a) = tv.uid then
      let a, cs = Atom.extend_var_one a approx tv in
      (a,b), cs
    else
      let b, cs = B.extend_var_one b approx tv in
      (a,b), cs

  let extend_var_all (a,b) approx tv =
    let a, cs1 = Atom.extend_var_all a approx tv in
    let b, cs2 = B.extend_var_all b approx tv in
    (a,b), cs1@cs2

  let project (a,b) v_id =
    if (Atom.uid a) != (fst v_id) then B.project b v_id
    else Atom.project a v_id

  let weak_incremental_closure (a,b) c =
    if (Atom.uid a) != (fst c) then (a, B.weak_incremental_closure b c)
    else (Atom.weak_incremental_closure a c, b)

  let state (a,b) =
    Kleene.and_kleene (Atom.state a) (B.state b)

  let entailment (a,b) c =
    if (Atom.uid a) != (fst c) then B.entailment b c
    else Atom.entailment a c

  let split (a,b) =
    match Atom.split a with
    | [] ->
        let branches = B.split b in
        List.combine (Atom.lazy_copy a (List.length branches)) branches
    | branches ->
        List.combine branches (B.lazy_copy b (List.length branches))

  let volume (a,b) = (Atom.volume a) *. (B.volume b)

  let print fmt (a,b) =
    Format.fprintf fmt "%a\n%a" Atom.print a B.print b

  let drain_events (a,b) =
    let a, events = Atom.drain_events a in
    let b, events' = B.drain_events b in
    (a,b), events@events'

  let events_of (a,b) c =
    if Atom.uid a != (fst c) then B.events_of b c
    else Atom.events_of a c
end

module Direct_product(P: Prod_combinator) =
struct
  module B = Bound_rat

  module I =
  struct
    type t = {
      uid: ad_uid;
      prod: P.t
    }
    type var_id = P.var_id
    type rconstraint = P.rconstraint
    let wrap p prod = {p with prod}
    let init uid prod = { uid; prod=(P.init prod) }
    let empty uid = { uid=(uid + P.count); prod=(P.empty' uid)}
    let exists p name = P.exists p.prod name
    let to_logic_var p vid = P.to_logic_var p.prod vid
    let to_abstract_var p vname = P.to_abstract_var p.prod vname

    let to_qformula p cs =
      let cs = P.to_qformula p.prod cs in
      q_conjunction p.uid cs

    let interpret' puid prod approx tf =
      let rec aux prod (uid, f) =
        match f with
        | TAnd (tf1, tf2) when uid = puid ->
            let (prod, cs1) = aux prod tf1 in
            let (prod, cs2) = aux prod tf2 in
            (prod, cs1@cs2)
        | f when uid = puid -> P.interpret_all prod approx (uid,f)
        | f -> P.interpret_one prod approx (uid,f) in
      aux prod tf

    let interpret p approx tf =
      let prod, cs = interpret' p.uid p.prod approx tf in
      wrap p prod, cs
  end

  type t = I.t

  let name = "Direct_product(" ^ P.name ^ ")"

  let init = I.init
  (* The UIDs of the product components are generated as follows `uid,...,uid+(n-1)`.
     The UID of the product is `uid+n`. *)
  let empty = I.empty
  let uid (p:t) = p.uid

  let type_of (p:t) = Some (p.uid, Direct_product (P.type_of p.prod))

  let closure (p:t) =
    let prod, has_changed = P.closure p.prod in
    I.wrap p prod, has_changed

  let state (p:t) = P.state p.prod

  type snapshot = P.snapshot

  let restore p s = I.wrap p (P.restore p.prod s)
  let lazy_copy (p:t) n = P.lazy_copy p.prod n

  let project (p:t) v_id = P.project p.prod v_id
  let weak_incremental_closure p c = I.wrap p (P.weak_incremental_closure p.prod c)
  let entailment (p:t) c = P.entailment p.prod c
  let split (p:t) = P.split p.prod
  let volume (p:t) = P.volume p.prod
  let print fmt (p:t) = P.print fmt p.prod

  let map_interpretation p f = f p
  let interpretation (p:t) = p

  let interpret (p:t) approx tqf =
    let rec aux prod = function
    | TQFFormula tf -> I.interpret' p.uid prod approx tf
    | TExists (tv, tqf) ->
        let extend =
          if p.uid = tv.uid then P.extend_var_all
          else P.extend_var_one in
        let prod, cs1 = extend prod approx tv in
        let prod, cs2 = aux prod tqf in
        prod, cs1@cs2
    in
      let prod, cs = aux p.prod tqf in
      I.wrap p prod, cs

  let drain_events (p:t) =
    let prod, events = P.drain_events p.prod in
    I.wrap p prod, events

  let events_of (p:t) c = P.events_of p.prod c
end
