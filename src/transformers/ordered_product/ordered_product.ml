(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Domains.Abstract_domain
open Domains.Interpretation
open Lang.Ast
open Lang.Rewritting
open Core
open Core.Types
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

  val init: init_t -> t
  val empty: unit -> t
  val extend: t -> (var * gvar * var_abstract_ty) -> t
  val to_logic_var: t -> gvar -> (var * var_abstract_ty)
  val to_abstract_var: t -> var -> (gvar * var_abstract_ty)
  val interpret: t -> approx_kind -> formula -> (t * gconstraint list) option
  val to_qformula: t -> gconstraint list -> qformula
  val qinterpret: t -> approx_kind -> qformula -> t option

  val empty': ad_uid -> t
  val extend': ?ty:var_ty -> t -> (t * gvar * var_abstract_ty)
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
end

module Prod_atom(A: Abstract_domain) =
struct
  module A = A

  type init_t = A.t ref
  type t = {
    a: init_t;
    var_map: (A.I.var_id * int) list;
    constraint_map: A.I.rconstraint list;
  }
  type var_id = gvar
  type rconstraint = gconstraint

  let count = 1

  let wrap pa a = pa.a := a; pa

  let init a = { a; var_map=[]; constraint_map=[] }
  let empty' uid = init (ref (A.empty uid))

  let uid pa = A.uid !(pa.a)
  let interpretation pa = A.interpretation !(pa.a)

  let empty () = raise (Wrong_modelling
    "`Ordered_product.I.empty`: the interpretation must be created with `init`.")

  let get_constraint pa (uid', c_id) =
    if uid pa != uid' then raise
      (Wrong_modelling "This constraint does not belong to this ordered product.")
    else
      List.nth pa.constraint_map c_id

  type snapshot = {
    a_bt: A.snapshot;
    var_map_bt: (A.I.var_id * int) list;
    constraint_map_bt: A.I.rconstraint list;
  }

  let restore pa snapshot =
    pa.a := A.restore !(pa.a) snapshot.a_bt;
    {pa with
      var_map=snapshot.var_map_bt;
      constraint_map=snapshot.constraint_map_bt }

  let make_snapshot pa a_bt =
    {a_bt; var_map_bt=pa.var_map; constraint_map_bt=pa.constraint_map}

  let lazy_copy pa n =
    List.map (make_snapshot pa) (A.lazy_copy !(pa.a) n)

  let extend pa (v, (_, var_id), ty) =
    let v_a = fst (List.nth pa.var_map var_id) in
    wrap pa (A.map_interpretation !(pa.a) (fun i -> A.I.extend i (v, v_a, ty)))

  let to_logic_var' pa var_id =
      A.I.to_logic_var (interpretation pa) (fst (List.nth pa.var_map var_id))

  let to_logic_var pa (uid', var_id) =
    if uid pa != uid' then raise Not_found
    else to_logic_var' pa var_id

  let to_abstract_var pa var =
    let (var_a, ty) = A.I.to_abstract_var (interpretation pa) var in
    let idx = List.assoc var_a pa.var_map in
    ((uid pa, idx), ty)

  let to_abstract_constraint pa c = List.nth pa.constraint_map (snd c)
  let to_generic_constraint pa c =
    let num_cons = List.length pa.constraint_map in
    let constraint_map = pa.constraint_map@[c] in
    {pa with constraint_map}, (uid pa, num_cons)

  let interpret pa approx f =
    let interpreted_f = A.I.interpret (interpretation pa) approx f in
    match interpreted_f with
    | None -> None
    | Some (i, constraints) ->
        let pa = wrap pa (A.map_interpretation !(pa.a) (fun _ -> i)) in
        let pa, gconstraints = List.fold_left
          (fun (pa, gcons) c -> let pa, c = to_generic_constraint pa c in pa, c::gcons)
          (pa, []) constraints in
        Some (pa, gconstraints)

  let to_qformula' pa constraints =
    let these, others = List.partition (fun c -> (uid pa) = (fst c)) constraints in
    (List.map (to_abstract_constraint pa) these), others

  let do_not_belong_exn s =
    raise (Wrong_modelling ("`" ^ s ^ "` over some constraints not belonging to any domain."))

  let to_qformula pa constraints =
    let constraints, others = to_qformula' pa constraints in
    match others with
    | [] -> A.I.to_qformula (interpretation pa) constraints
    | _ -> do_not_belong_exn "qformula"

  let closure pa =
    let (a, has_changed) = A.closure !(pa.a) in
    wrap pa a, has_changed

  let extend' ?ty pa =
    let (a, v_a, aty) = A.extend ?ty !(pa.a) in
    let v_id = List.length pa.var_map in
    let pa = {pa with var_map=(pa.var_map@[(v_a, v_id)]) } in
    wrap pa a, (uid pa, v_id), aty

  let project pa (uid', v_id) =
    if uid pa != uid' then raise Not_found
    else
      let (lb, ub) = A.project !(pa.a) (fst (List.nth pa.var_map v_id)) in
      A.B.to_rat lb, A.B.to_rat ub

  let weak_incremental_closure pa (uid', c_id) =
    if uid pa != uid' then raise (Wrong_modelling "`Ordered_product.weak_incremental_closure`: this constraint does not belong to this ordered product.")
    else
      let c_a = List.nth pa.constraint_map c_id in
      let a = A.weak_incremental_closure !(pa.a) c_a in
      wrap pa a

  let state pa = A.state !(pa.a)
  let entailment pa c = A.entailment !(pa.a) (get_constraint pa c)
  let split pa = List.map (make_snapshot pa) (A.split !(pa.a))
  let volume pa = A.volume !(pa.a)
  let print fmt pa = Format.fprintf fmt "%a" A.print !(pa.a)

  let qinterpret pa approx formula =
    match A.qinterpret !(pa.a) approx formula with
    | Some a -> Some (wrap pa a)
    | None -> None
end

module Prod_cons(A: Abstract_domain)(B: Prod_combinator) =
struct
  module Atom = Prod_atom(A)
  type var_id = gvar
  type rconstraint = gconstraint

  type init_t = Atom.init_t * B.init_t
  type t = Atom.t * B.t

  let count = Atom.count + B.count

  let init (a,b) = (Atom.init a, B.init b)

  let empty () = raise (Wrong_modelling
    "`Ordered_product.I.empty`: the interpretation must be created with `init`.")

  let empty' uid = Atom.empty' uid, B.empty' (uid+1)

  type snapshot = Atom.snapshot * B.snapshot

  let restore (a,b) (sa,sb) =
    (Atom.restore a sa, B.restore b sb)

  let lazy_copy (a,b) n =
    let a_snapshots = Atom.lazy_copy a n in
    let b_snapshots = B.lazy_copy b n in
    List.combine a_snapshots b_snapshots

  let extend (a,b) ((_, (uid, _), _) as vmap) =
    if Atom.uid a != uid then a, B.extend b vmap
    else Atom.extend a vmap, b

  let to_logic_var (a,b) ((uid, var_id) as v) =
    if Atom.uid a != uid then B.to_logic_var b v
    else
      Atom.to_logic_var' a var_id

  let to_abstract_var (a,b) var =
    try
      Atom.to_abstract_var a var
    with Not_found ->
      B.to_abstract_var b var

  let option1 x = function
  | None -> None
  | Some (y,c) -> Some ((x,y), c)

  let option2 y = function
  | None -> None
  | Some (x,c) -> Some ((x,y), c)

  let interpret (a,b) approx f =
    match option2 b (Atom.interpret a approx f) with
    | None -> option1 a (B.interpret b approx f)
    | x -> x

  let to_qformula (a,b) constraints =
    let constraints, others = Atom.to_qformula' a constraints in
    let qformula = Atom.A.I.to_qformula (Atom.interpretation a) constraints in
    q_conjunction [qformula; (B.to_qformula b others)]

  let closure (a,b) =
    let a, has_changed = Atom.closure a in
    let b, has_changed' = B.closure b in
    (a,b), has_changed || has_changed'

  let extend' ?ty (a,b) =
    try
      let (a, v, ty) = Atom.extend' ?ty a in
      ((a,b), v, ty)
    with Wrong_modelling _ ->
      let (b, v, ty) = B.extend' ?ty b in
      ((a,b), v, ty)

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

  let qinterpret (a,b) approx formula =
    match Atom.qinterpret a approx formula with
    | Some a -> Some (a,b)
    | None ->
        match B.qinterpret b approx formula with
        | Some b -> Some(a,b)
        | None -> None
end

module Ordered_product(P: Prod_combinator) =
struct
  module B = Bound_rat
  module I = P

  type t = {
    uid: ad_uid;
    prod: P.t
  }

  let init uid prod = { uid; prod=(P.init prod) }
  (* The UIDs of the product components are generated as follows `uid,...,uid+(n-1)`.
     The UID of the product is `uid+n`. *)
  let empty uid = { uid=(uid + P.count); prod=(P.empty' uid)}
  let uid p = p.uid

  let wrap p prod = {p with prod}

  let closure p =
    let prod, has_changed = P.closure p.prod in
    wrap p prod, has_changed

  let state p = P.state p.prod

  type snapshot = P.snapshot

  let restore p s = wrap p (P.restore p.prod s)
  let lazy_copy p n = P.lazy_copy p.prod n

  let extend ?ty p =
    let (prod, v, ty) = P.extend' ?ty p.prod in
    wrap p prod, v, ty
  let project p v_id = P.project p.prod v_id
  let weak_incremental_closure p c = wrap p (P.weak_incremental_closure p.prod c)
  let entailment p c = P.entailment p.prod c
  let split p = P.split p.prod
  let volume p = P.volume p.prod
  let print fmt p = P.print fmt p.prod

  let map_interpretation p f = wrap p (f p.prod)
  let interpretation p = p.prod

  (** The formula is supposely of the form `∃(v1..vN).f1 /\ ... /\ fN`.
      Each sub-formula `fi` is then added into an abstract domain supporting this formula.
      The abstract domains are tried in sequential order until one supporting the formula is found. *)
  let qinterpret p approx formula =
    let rec aux p vars = function
    | Exists (v, ty, qf) -> aux p ((v,ty)::vars) qf
    | QFFormula (And (f1, f2)) ->
      begin
        match aux p vars (QFFormula f1) with
        | Some p -> aux p vars (QFFormula f2)
        | None -> None
      end
    | QFFormula formula ->
        let qf = quantify vars formula in
        match P.qinterpret p.prod approx qf with
        | None -> None
        | Some prod -> Some (wrap p prod)
    in aux p [] formula
end
