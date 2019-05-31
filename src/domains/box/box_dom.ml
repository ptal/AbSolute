open Var_store
open Abstract_domain
open Box_representation

module type Box_sig =
sig
  type t
  module I: Itv_sig.ITV
  module R = Box_rep
  type itv = I.t
  type bound = I.B.t

  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (I.B.t * I.B.t)
  val project_itv: t -> R.var_id -> itv
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> kleene
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> kleene
  val print: R.t -> Format.formatter -> t -> unit
end

module type Box_functor = functor (B: Bound_sig.BOUND) -> Box_sig with module I.B = B

module Make
  (B: Bound_sig.BOUND)
  (INTERVAL: Itv_sig.Itv_functor)
  (STORE: Var_store_functor)
  (CLOSURE: Hc4.Box_closure_sig)
  (SPLIT: Box_split.Box_split_sig) =
struct
  module Interval = INTERVAL(B)
  module Store = STORE(Interval)
  module Closure = CLOSURE(Store)
  module Split = SPLIT(Store)
  module I = Store.I
  module B = I.B
  module R = Box_rep
  type itv = I.t
  type bound = I.B.t
  type t = {
    store: Store.t;
    constraints: R.rconstraint list;
  }

  (* Reexported functions from the parametrized modules. *)
  let entailment box = Closure.entailment box.store

  (* This function helps to deal with unreachable state: normally a disentailed constraint triggered a `Bot_found` exception. *)
  let failure_disentailment () =
    failwith "Found a constraint that is disentailed and Bot_found has not been raised."

  let empty = {
    store=Store.empty;
    constraints=[];
  }

  let extend box () =
    let (store, idx) = Store.extend box.store in
    ({ box with store = store }, idx)

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

  (* We propagate all the constraints in box.
     The volume is used to detect when the store changed. *)
  let rec propagate vol box =
    let store = List.fold_left Closure.incremental_closure box.store box.constraints in
    let box = { box with store=store } in
    let vol' = volume box in
    if vol <> vol' then
      propagate vol' box
    else
      (vol, box)

  (* We remove the constraints entailed in the store.
     It is useful to avoid propagating entailed constraints. *)
  let remove_entailed_constraints box =
    let is_unknown c =
      match entailment box c with
      | Unknown -> true
      | True -> false
      | False -> failure_disentailment () in
    { box with constraints=List.filter is_unknown box.constraints }

  let closure box =
    let vol = volume box in
    let (vol', box) = propagate vol box in
    if vol <> vol' then
      remove_entailed_constraints box
    else
      box

  let weak_incremental_closure box c = { box with constraints=c::box.constraints }
  let incremental_closure box c = closure (weak_incremental_closure box c)

  (* `closure` and `incremental_closure` automatically remove entailed constraints. *)
  let state_decomposition box =
    if (List.length box.constraints) = 0 then
      True
    else
      Unknown

  let print repr fmt box =
  begin
    Store.print fmt repr box.store;
    Format.fprintf fmt "\n";
    let print_var fmt v = Csp.print_var fmt (R.to_logic_var repr v) in
    List.iter (Format.fprintf fmt "%a\n" (Csp.print_gconstraint print_var)) box.constraints;
  end

  let split box =
    let branches = Split.split box.store in
    let boxes = lazy_copy box (List.length branches) in
    List.map2 weak_incremental_closure boxes branches
end

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor = functor (B: Bound_sig.BOUND) ->
  Make(B)(Itv.Itv)(Var_store.Make)(Hc4.Make)(SPLIT)
