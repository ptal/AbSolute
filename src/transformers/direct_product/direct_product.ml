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
open Bounds.Converter

type gvar = ad_uid * int
type gconstraint = ad_uid * int

module type Prod_combinator =
sig
  type init_t
  type t
  type var_id = gvar
  type rconstraint = gconstraint
  val exact_interpretation: bool
  val count: int
  val name: string

  val init: init_t -> t
  val empty: unit -> t
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> vname -> (var_id * Tast.tvariable)
  val local_vars: t -> vname -> var_id list
  val to_qformula: t -> gconstraint list -> tqformula list
  val interpret_all: t -> approx_kind -> tformula -> t * gconstraint list
  val interpret_one: t -> approx_kind -> tformula -> t * gconstraint list
  val extend_var_all: t -> approx_kind -> tvariable -> t * gconstraint list
  val extend_var_one: t -> approx_kind -> tvariable -> t * gconstraint list

  val empty': ad_uid -> t
  val type_of: t -> ad_ty list
  val project: t -> gvar -> (Bound_rat.t * Bound_rat.t)
  val embed: t -> gvar -> (Bound_rat.t * Bound_rat.t) -> t
  type snapshot
  val restore: t -> snapshot -> t
  val lazy_copy: t -> int -> snapshot list
  val closure: t -> t
  val weak_incremental_closure: t -> gconstraint -> t
  val entailment: t -> gconstraint -> t * gconstraint * bool
  val split: (ad_uid * search_strategy) -> t -> snapshot list
  val volume: t -> float
  val state: t -> Kleene.t
  val print: Format.formatter -> t -> unit
  val has_changed: t -> bool
  val drain_events: t -> (t * event list)
  val events_of: t -> rconstraint -> event list
  val events_of_var: t -> var_id -> event list
end

type 'a owned_or_shared =
  Owned of 'a
| Shared of 'a

module Prod_atom(A: Abstract_domain) =
struct
  module A = A

  type init_t = (A.t ref) owned_or_shared

  type t = {
    a: A.t ref;
    owned: bool;
    var_map: A.I.var_id list;
    constraint_map: A.I.rconstraint Parray.t;
  }
  type var_id = gvar
  type rconstraint = gconstraint

  (* Depending if the underlying domain is owned or shared, this product takes care of the backtracking or not.
     This is to ensure that only one product takes care of the memory management of a domain. *)
  type a_snapshot =
    Backtrack of A.snapshot
  | RefOnly

  type snapshot = {
    a_bt: a_snapshot;
    var_map_bt: A.I.var_id list;
    constraint_map_bt: A.I.rconstraint Parray.t;
  }
  let exact_interpretation = A.I.exact_interpretation
  let count = 1
  let name = A.name

  let unwrap pa = !(pa.a)
  let wrap pa a = pa.a := a; pa

  let type_of pa =
    match A.type_of (unwrap pa) with
    | Some t -> [t]
    | None -> []

  let restore pa snapshot =
    (match snapshot.a_bt with
    | Backtrack a_bt ->
        pa.a := A.restore !(pa.a) a_bt
    | RefOnly -> ());
    {pa with
      var_map=snapshot.var_map_bt;
      constraint_map=snapshot.constraint_map_bt }

  let make_snapshots pa n make_children =
    let children =
      if pa.owned then
        List.map (fun a_bt -> Backtrack a_bt) (make_children !(pa.a))
      else
        List.map (fun _ -> RefOnly) (Tools.range 1 n) in
    List.map
      (fun a_bt -> {a_bt; var_map_bt=pa.var_map; constraint_map_bt=pa.constraint_map})
      children

  let lazy_copy pa n = make_snapshots pa n (fun a -> A.lazy_copy a n)

  (* For the time of the execution of `f`, we own `a` and restore it if an exception occurs. *)
  let safe_wrap pa f =
    let pa' = {pa with owned=true} in
    let snapshots = lazy_copy pa' 2 in
    try
      let pa' = restore pa' (List.hd snapshots) in
      f {pa' with owned=pa.owned}
    with e ->
      (ignore(restore pa' (List.nth snapshots 1)); (* To restore mutable effects. *)
      raise e)

  let init a =
    let (a, owned) =
      match a with
      | Owned a -> (a, true)
      | Shared a -> (a, false) in
    { a; owned; var_map=[]; constraint_map=(Tools.empty_parray ()) }

  let empty' uid = init (Owned (ref (A.empty uid)))

  let uid pa = A.uid !(pa.a)
  let interpretation pa = A.interpretation !(pa.a)

  let empty () = raise (Wrong_modelling
    "`Direct_product.I.empty`: the interpretation must be created with `init`.")

  let get_constraint pa (uid', c_id) =
    if uid pa != uid' then raise
      (Wrong_modelling "This constraint does not belong to this direct product.")
    else
      Parray.get pa.constraint_map c_id

  let set_constraint pa (_, c_id) ac =
    let constraint_map = Parray.set pa.constraint_map c_id ac in
    {pa with constraint_map}

  let to_logic_var' pa var_id =
    A.I.to_logic_var (interpretation pa) (List.nth pa.var_map var_id)

  let to_logic_var pa (uid', var_id) =
    if uid pa != uid' then raise Not_found
    else to_logic_var' pa var_id

  let to_abstract_var pa var =
    let (var_a, tv) = A.I.to_abstract_var (interpretation pa) var in
    let idx = Tools.find_index var_a pa.var_map in
    ((uid pa, idx), tv)

  let local_vars pa var =
    try [fst (to_abstract_var pa var)]
    with Not_found -> []

  let to_abstract_constraint pa c = Parray.get pa.constraint_map (snd c)
  let to_generic_constraint pa c =
    let num_cons = Parray.length pa.constraint_map in
    let constraint_map = Tools.extend_parray pa.constraint_map c in
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

  let closure pa = wrap pa (A.closure !(pa.a))

  let project pa (uid', v_id) =
    if uid pa != uid' then raise Not_found
    else
      let (lb, ub) = A.project !(pa.a) (List.nth pa.var_map v_id) in
      A.B.to_rat lb, A.B.to_rat ub

  module ToA = Converter(Bound_rat)(A.B)
  let embed pa (uid', v_id) (l,u) =
    if uid pa != uid' then raise Not_found
    else
      let l,u = ToA.convert_down l, ToA.convert_up u in
      let a = A.embed !(pa.a) (List.nth pa.var_map v_id) (l,u) in
      wrap pa a

  let weak_incremental_closure pa (uid', c_id) =
    if uid pa != uid' then raise (Wrong_modelling "`Direct_product.weak_incremental_closure`: this constraint does not belong to this direct product.")
    else
      let c_a = Parray.get pa.constraint_map c_id in
      let a = A.weak_incremental_closure !(pa.a) c_a in
      wrap pa a

  let state pa = A.state !(pa.a)

  let entailment pa c =
    let (a, ac, b) = A.entailment !(pa.a) (get_constraint pa c) in
    set_constraint (wrap pa a) c ac, c, b

  let split (u, strategy) pa =
    if uid pa != u then
      raise (Wrong_modelling "`Direct_product.split`: the strategy is defined on a domain not in this product.")
    else
      make_snapshots pa 0 (A.split ~strategy)

  let volume pa = if pa.owned then A.volume !(pa.a) else 1.
  let print fmt pa = if pa.owned then Format.fprintf fmt "%a" A.print !(pa.a) else ()

  let interpret pa approx tf =
    if snd tf = Tast.ctrue then pa, []
    else
    begin
      safe_wrap pa (fun pa ->
        try
          let (a, constraints) = A.map_interpretation !(pa.a)
            (fun i -> A.I.interpret i approx tf) in
          let pa = wrap pa a in
          let pa, gconstraints = to_generic_constraints pa constraints in
          pa, gconstraints
        with Wrong_modelling msg ->
          raise (Wrong_modelling ("[" ^ A.name ^ "] " ^ msg)))
    end

  let interpret_all pa approx tf =
    let tf = Tast.replace_uid (A.uid (unwrap pa)) tf in
    interpret pa approx tf

  let interpret_one = interpret

  let extend_var pa approx tv =
    safe_wrap pa (fun pa ->
      let (a, cs) = A.interpret !(pa.a) approx (TExists(tv,ttrue)) in
      let pa = wrap pa a in
      let (atom_id, _) = A.I.to_abstract_var (interpretation pa) tv.name in
      let pa = {pa with var_map=(pa.var_map@[atom_id]) } in
      let pa, gcons = to_generic_constraints pa cs in
      pa, gcons)

  let extend_var_all pa approx tv =
    extend_var pa approx {tv with uid=(uid pa)}

  let extend_var_one = extend_var

  let has_changed pa = A.has_changed (unwrap pa)

  let drain_events pa =
    let a, events = A.drain_events (unwrap pa) in
    wrap pa a, events

  let events_of pa c =
    A.events_of (unwrap pa) (get_constraint pa c)

  let events_of_var pa v_id =
    A.events_of_var (unwrap pa) (List.nth pa.var_map (snd v_id))
end

module Prod_cons(A: Abstract_domain)(B: Prod_combinator) =
struct
  module Atom = Prod_atom(A)
  type var_id = gvar
  type rconstraint = gconstraint

  type init_t = Atom.init_t * B.init_t
  type t = Atom.t * B.t
  let exact_interpretation = Atom.exact_interpretation && B.exact_interpretation
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

  let to_logic_var (a,b) ((uid, var_id) as v) =
    if Atom.uid a != uid then B.to_logic_var b v
    else
      Atom.to_logic_var' a var_id

  let to_abstract_var (a,b) var =
    try
      Atom.to_abstract_var a var
    with Not_found ->
      B.to_abstract_var b var

  let local_vars (a,b) var =
    (Atom.local_vars a var)@(B.local_vars b var)

  (* Whenever the list of constraints is empty means that it is a tautology, so we do not explicitly represent it.  *)
  let to_qformula (a,b) constraints =
    let a_constraints, b_constraints = Atom.to_qformula' a constraints in
    if a_constraints = [] then
      B.to_qformula b b_constraints
    else
      let a_formula = Atom.A.I.to_qformula (Atom.interpretation a) a_constraints in
      if b_constraints = [] then [a_formula]
      else a_formula::(B.to_qformula b b_constraints)

  let closure (a,b) = (Atom.closure a, B.closure b)

 let wrap_wrong_modelling f1 f2 =
    try f1 ()
    with Wrong_modelling msg1 ->
      begin try f2 ()
      with Wrong_modelling msg2 ->
        let newline =
          if msg1.[(String.length msg1) - 1] = '\n' then "" else "\n" in
        raise (Wrong_modelling (msg1 ^ newline ^ msg2))
      end

  let interpret_one (a,b) approx tf =
    wrap_wrong_modelling
      (fun () ->
        let a, cs = Atom.interpret_one a approx tf in
        (a,b), cs)
      (fun () ->
        let b, cs = B.interpret_one b approx tf in
        (a,b), cs)

  let interpret_all (a,b) approx tf =
    let a, cs1 = Atom.interpret_all a approx tf in
    let b, cs2 = B.interpret_all b approx tf in
    (a,b), cs1@cs2

  let extend_var_one (a,b) approx tv =
    wrap_wrong_modelling
      (fun () ->
        let a, cs = Atom.extend_var_one a approx tv in
        (a,b), cs)
      (fun () ->
        let b, cs = B.extend_var_one b approx tv in
        (a,b), cs)

  let extend_var_all (a,b) approx tv =
    let a, cs1 = Atom.extend_var_all a approx tv in
    let b, cs2 = B.extend_var_all b approx tv in
    (a,b), cs1@cs2

  let project (a,b) v_id =
    if (Atom.uid a) != (fst v_id) then B.project b v_id
    else Atom.project a v_id

  let embed (a,b) v_id (l,u) =
    if (Atom.uid a) != (fst v_id) then a, B.embed b v_id (l,u)
    else Atom.embed a v_id (l,u), b

  let weak_incremental_closure (a,b) c =
    if (Atom.uid a) != (fst c) then (a, B.weak_incremental_closure b c)
    else (Atom.weak_incremental_closure a c, b)

  let state (a,b) =
    Kleene.and_kleene (Atom.state a) (B.state b)

  let entailment (a,b) c =
    if (Atom.uid a) != (fst c) then
      let (b, c, is_entailed) = B.entailment b c in
      (a,b), c, is_entailed
    else
      let (a, c, is_entailed) = Atom.entailment a c in
      (a,b), c, is_entailed

  let split (uid, strategy) (a,b) =
    if (Atom.uid a) != uid then
      let branches = B.split (uid, strategy) b in
      List.combine (Atom.lazy_copy a (List.length branches)) branches
    else
      let branches = Atom.split (uid, strategy) a in
      List.combine branches (B.lazy_copy b (List.length branches))

  let volume (a,b) = (Atom.volume a) *. (B.volume b)

  let print fmt (a,b) =
    Format.fprintf fmt "%a\n%a" Atom.print a B.print b

  let has_changed (a,b) = Atom.has_changed a || B.has_changed b

  let drain_events (a,b) =
    let a, events = Atom.drain_events a in
    let b, events' = B.drain_events b in
    (a,b), events@events'

  let events_of (a,b) c =
    if Atom.uid a != (fst c) then B.events_of b c
    else Atom.events_of a c

  let events_of_var (a,b) v_id =
    if (Atom.uid a) != (fst v_id) then B.events_of_var b v_id
    else Atom.events_of_var a v_id
end

module Direct_product(P: Prod_combinator) =
struct
  module B = Bound_rat
  let name = "Direct_product(" ^ P.name ^ ")"

  module I =
  struct
    type t = {
      uid: ad_uid;
      prod: P.t;
      (* See `split` for the usage of these strategies. *)
      initial_strategy: search_strategy;
      strategy: search_strategy;
    }
    type var_id = P.var_id
    type rconstraint = P.rconstraint
    let exact_interpretation = P.exact_interpretation
    let wrap p prod = {p with prod}
    let init uid prod = { uid; prod=(P.init prod);
      initial_strategy=Simple; strategy=Simple }
    let empty uid = { uid=(uid + P.count); prod=(P.empty' uid);
      initial_strategy=Simple; strategy=Simple }
    let to_logic_var p vid = P.to_logic_var p.prod vid
    let to_abstract_var p vname = P.to_abstract_var p.prod vname
    let local_vars p vname =
      match P.local_vars p.prod vname with
      | [] -> raise Not_found
      | l -> l

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

  let init = I.init
  (* The UIDs of the product components are generated as follows `uid,...,uid+(n-1)`.
     The UID of the product is `uid+n`. *)
  let empty = I.empty
  let uid (p:t) = p.uid

  let type_of (p:t) = Some (p.uid, Direct_product (P.type_of p.prod))

  let closure (p:t) = I.wrap p (P.closure p.prod)

  let state (p:t) = P.state p.prod

  type snapshot = P.snapshot * search_strategy * search_strategy

  let restore p (s,s1,s2) =
    let p = I.wrap p (P.restore p.prod s) in
    { p with initial_strategy=s1; strategy=s2 }

  let make_snapshot (p:t) s = (s,p.initial_strategy, p.strategy)

  let lazy_copy (p:t) n =
    List.map (make_snapshot p) (P.lazy_copy p.prod n)

  let project (p:t) v_id = P.project p.prod v_id
  let embed (p:t) v_id (l,u) = I.wrap p (P.embed p.prod v_id (l,u))
  let weak_incremental_closure p c = I.wrap p (P.weak_incremental_closure p.prod c)
  let entailment (p:t) c =
    let (p', c, b) = P.entailment p.prod c in
    (I.wrap p p', c, b)

  let split ?strategy:(strat=Simple) (p:t) =
    let p =
      if strat == p.initial_strategy then p
      else { p with initial_strategy=strat; strategy=strat } in
    let sequence_of s =
      Sequence(List.map (fun (uid,_) -> (uid, s)) (P.type_of p.prod)) in
    let rec aux s =
      match s with
      | Simple | VarView _ -> aux (sequence_of s)
      | Sequence [] -> s, []
      | Sequence (strat::l) ->
          let branches = P.split strat p.prod in
          if branches = [] then
            aux (Sequence l)
          else
            s, branches in
    (* let strategy, branches = aux p.strategy in
    List.map (make_snapshot { p with strategy }) branches *)
    let _, branches = aux p.strategy in
    List.map (make_snapshot p) branches

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

  let has_changed (p:t) = P.has_changed p.prod

  let drain_events (p:t) =
    let prod, events = P.drain_events p.prod in
    I.wrap p prod, events

  let events_of (p:t) c = P.events_of p.prod c
  let events_of_var (p:t) v_id = P.events_of_var p.prod v_id
end
