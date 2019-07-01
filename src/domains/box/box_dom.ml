open Box_representation
open Pengine
open Kleene

module type Box_sig =
sig
  type t
  module Vardom : Vardom_sig.Vardom_sig
  module R : Box_rep_sig with module Vardom = Vardom
  type vardom = Vardom.t
  type bound = Vardom.B.t

  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (Vardom.B.t * Vardom.B.t)
  val project_vardom: t -> R.var_id -> vardom
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> Kleene.t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> Kleene.t
  val print: R.t -> Format.formatter -> t -> unit
  val delta: t -> R.var_id list
end

module type Box_functor = functor (B: Bound_sig.BOUND) -> Box_sig with module Vardom.B = B

module Make
  (B: Bound_sig.BOUND)
  (VARDOM: Vardom_sig.Vardom_functor)
  (SPLIT: Box_split.Box_split_sig) =
struct
  module Vardom = VARDOM(B)
  module R = Box_rep(Vardom)
  module Store = R.Store
  module Closure = Hc4.Make(R)
  module Split = SPLIT(R)
  module V = Vardom
  module B = V.B
  type vardom = V.t
  type bound = V.B.t

  (* We use `Parray` for most arrays because the structures must be backtrackable.
     Note that for `constraints` and `reactor` these structures should be static during the resolution.
     (We rarely add a new constraint inside the problem during resolution, but with Parray, it is possible to do so). *)
  type t = {
    store: Store.t;
    constraints: R.rconstraint Parray.t;

    (* Propagation engine of the constraints in `constraints`. *)
    engine: Pengine.t;
  }

  (* Reexported functions from the parametrized modules. *)
  let entailment box = Closure.entailment box.store

  (* This function helps to deal with unreachable state: normally a disentailed constraint triggered a `Bot_found` exception. *)
  let failure_disentailment () =
    failwith "Found a constraint that is disentailed and Bot_found has not been raised."

  let empty = {
    store=Store.empty;
    constraints = Tools.empty_parray ();
    engine = Pengine.empty ();
  }

  let extend box () =
    let (store, idx) = Store.extend box.store in
    let engine = Pengine.extend_event box.engine in
    ({ box with store; engine }, idx)

  let project_vardom box v = Store.get box.store v

  let project box v = V.to_range (project_vardom box v)

  let lazy_copy box n = List.map (fun s -> { box with store=s }) (Store.lazy_copy box.store n)
  let copy box = { box with store=Store.copy box.store }

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

  let closure_one box c_idx =
    let store, entailed = Closure.incremental_closure box.store (Parray.get box.constraints c_idx) in
    let store, deltas = Store.delta store in
    { box with store }, entailed, deltas

  let closure box =
    let store, deltas = Store.delta box.store in
    let box = { box with store } in
    if List.length deltas = 0 then box
    else
    begin
      Pengine.react box.engine deltas;
      let engine, box = Pengine.fixpoint box.engine closure_one box in
      { box with engine }
    end

  (* We propagate the constraint immediately.
     If the constraint is not entailed, we register it into the event system (in the actives, reactor and constraints structures). *)
  let weak_incremental_closure box c =
    let store, entailed = Closure.incremental_closure box.store c in
    let box = { box with store } in
    if entailed then box
    else
      let c_idx = Parray.length box.constraints in
      let constraints = Tools.extend_parray box.constraints c in
      let engine = Pengine.extend_task box.engine in
      let vars = List.sort_uniq compare (R.vars_of_constraint c) in
      let engine = Pengine.subscribe engine c_idx vars in
      { box with constraints; engine; }

  (* Entailed constraints are automatically deactivated by Pengine.fixpoint. *)
  let state_decomposition box =
    if Pengine.num_active_tasks box.engine = 0 then True
    else Unknown

  let print_store fmt repr store =
    let print_entry idx vardom =
      Format.fprintf fmt "%s=%a \n" (R.to_logic_var repr idx) V.print vardom in
    Store.iter print_entry store

  let print repr fmt box =
  let print_constraint c =
    let logic_constraint = R.to_logic_constraint repr c in
    Format.fprintf fmt "%a\n" Csp.print_constraint logic_constraint in
  begin
    print_store fmt repr box.store;
    Format.fprintf fmt "\n";
    Parray.iter print_constraint box.constraints;
  end

  let split box =
    let branches = Split.split box.store in
    let boxes = lazy_copy box (List.length branches) in
    (* We remove the branch that are unsatisfiable. *)
    List.flatten (List.map2 (fun box branch ->
      try [weak_incremental_closure box branch]
      with Bot.Bot_found -> []) boxes branches)

  let delta box = Pengine.delta box.engine
end

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor = functor (B: Bound_sig.BOUND) ->
  Make(B)(Itv.Itv)(SPLIT)
