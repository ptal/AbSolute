open Var_store
open Box_representation
open Pengine
open Kleene

module type Box_sig =
sig
  type t
  module I: Vardom_sig.Vardom_sig
  module R = Box_rep
  type itv = I.t
  module B = I.B

  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (I.B.t * I.B.t)
  val project_itv: t -> R.var_id -> itv
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

module Make
  (VARDOM: Vardom_sig.Vardom_sig)
  (STORE: Var_store_functor)
  (CLOSURE: Hc4.Box_closure_sig)
  (SPLIT: Box_split.Box_split_sig) =
struct
  module Vardom = VARDOM
  module Store = STORE(Vardom)
  module Closure = CLOSURE(Store)
  module Split = SPLIT(Store)
  module I = Store.I
  module B = I.B
  module R = Box_rep
  type itv = I.t

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

  let project_itv box v = Store.get box.store v

  let project box v = I.to_range (project_itv box v)

  let lazy_copy box n = List.map (fun s -> { box with store=s }) (Store.lazy_copy box.store n)
  let copy box = { box with store=Store.copy box.store }

  let volume box =
    let range (l,h) =
      if B.equal l h then B.one
      else B.add_up (B.sub_up h l) B.one in
    let size itv = range (I.to_range itv) in
    let vol = B.to_float_up (Store.fold (fun acc _ itv -> B.mul_up (size itv) acc) B.one box.store) in
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
      let vars = List.sort_uniq compare (Csp.vars_of_bconstraint c) in
      let engine = Pengine.subscribe engine c_idx vars in
      { box with constraints; engine; }

  (* Entailed constraints are automatically deactivated by Pengine.fixpoint. *)
  let state_decomposition box =
    if Pengine.num_active_tasks box.engine = 0 then
      True
    else
      Unknown

  let print repr fmt box =
  begin
    Store.print fmt repr box.store;
    Format.fprintf fmt "\n";
    let print_var fmt v = Csp.print_var fmt (R.to_logic_var repr v) in
    Parray.iter (Format.fprintf fmt "%a\n" (Csp.print_gconstraint print_var)) box.constraints;
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

module Box_base(V: Vardom_sig.Vardom_sig)(SPLIT: Box_split.Box_split_sig) =
  Make(V)(Var_store.Make)(Hc4.Make)(SPLIT)

