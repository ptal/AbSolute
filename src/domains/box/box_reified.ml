open Csp
open Box_dom
open Box_representation
open Pengine

type rbox_constraint =
  | BoxConstraint of box_constraint
  | ReifiedConstraint of box_reified_constraint
and box_reified_constraint = box_var * rbox_constraint list

let rec vars_of_reified (b, conj) =
  let vars_of_rbox_cons acc = function
  | BoxConstraint c -> (Csp.vars_of_bconstraint c)@acc
  | ReifiedConstraint c -> (vars_of_reified c)@acc in
  let vars = List.fold_left vars_of_rbox_cons [] conj in
  List.sort_uniq compare (b::vars)

let recursive_reified () = raise (Wrong_modelling "Reified constraint inside reified constraint is not implemented.")

module type Reified_box_rep_sig =
sig
  type t
  type var_kind = unit
  type var_id = box_var
  type rconstraint = rbox_constraint

  val empty: t
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> bformula -> rconstraint list
  val rewrite_reified: t -> var -> bconstraint list -> rconstraint list
  val relax: t -> bformula -> rconstraint list
  val negate: rconstraint -> rconstraint
end

module Reified_box_rep =
struct
  type var_kind = unit
  type var_id = box_var
  type rconstraint = rbox_constraint
  module R = Box_rep
  type t = R.t
  let empty = R.empty
  let extend = R.extend
  let to_logic_var = R.to_logic_var
  let to_abstract_var = R.to_abstract_var
  let rewrite repr c = List.map (fun c -> BoxConstraint c) (R.rewrite repr c)
  let rewrite_reified repr b constraints =
    let bconstraints = List.map (fun (e1,op,e2) -> Cmp (op, e1, e2)) constraints in
    let constraints = List.flatten (List.map (R.rewrite repr) bconstraints) in
    let b = R.to_abstract_var repr b in
    [ReifiedConstraint (b, List.map (fun c -> BoxConstraint c) constraints)]
  let relax = rewrite
  let negate = function
  | BoxConstraint c -> BoxConstraint (R.negate c)
  | ReifiedConstraint (_, _) -> recursive_reified ()
end

module type Box_reified_sig =
sig
  type t
  module I: Vardom_sig.Vardom_sig
  module B = I.B
  module R = Reified_box_rep
  type bound = B.t
  type itv = I.t

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
end

module Make(Box: Box_sig) =
struct
  module I = Box.I
  module B = I.B
  module R = Reified_box_rep
  type bound = B.t
  type itv = I.t
  type t = {
    inner: Box.t;
    reified_constraints: box_reified_constraint Parray.t;
    engine: Pengine.t;
  }

  let empty = {
    inner=Box.empty;
    reified_constraints=Tools.empty_parray ();
    engine=Pengine.empty ();
  }

  let extend box () =
    let (inner, idx) = Box.extend box.inner () in
    let engine = Pengine.extend_event box.engine in
    ({ box with inner; engine }, idx)

  (* The following functions just forward the call to `Box`. *)
  let entailment box = function
    | BoxConstraint c -> Box.entailment box.inner c
    | ReifiedConstraint _ -> recursive_reified ()
  let project_itv box v = Box.project_itv box.inner v
  let project box v = Box.project box.inner v
  let lazy_copy box n = List.map (fun i -> { box with inner=i }) (Box.lazy_copy box.inner n)
  let copy box = { box with inner=Box.copy box.inner }
  let volume box = Box.volume box.inner

  let weak_incremental_closure box = function
    | BoxConstraint c -> {box with inner=Box.weak_incremental_closure box.inner c}
    | ReifiedConstraint (b, conj) ->
        let c_idx = Parray.length box.reified_constraints in
        let reified_constraints = Tools.extend_parray box.reified_constraints (b, conj) in
        let engine = Pengine.extend_task box.engine in
        let vars = vars_of_reified (b, conj) in
        let engine = Pengine.subscribe engine c_idx vars in
        { box with reified_constraints; engine; }

  let propagate_negation_conjunction box (b, conjunction) =
    let open Kleene in
    match Kleene.and_reified (List.map (entailment box) conjunction) with
    | False,_ -> box, true
    | True,_ -> raise Bot.Bot_found
    | Unknown, Some(u) ->
        let c = (List.nth conjunction u) in
        weak_incremental_closure box (R.negate c), true
    | Unknown, None -> box, false

  (* Propagate the reified constraints.
     Entailed reified constraints are removed from `box`. *)
  let propagate_reified box (b, conjunction) =
    let itv = Box.project_itv box.inner b in
    if Box.I.is_singleton itv then
      let (value,_) = Box.I.to_range itv in
      if B.equal B.one value then
        List.fold_left weak_incremental_closure box conjunction, true
      else if B.equal B.zero value then
        propagate_negation_conjunction box (b, conjunction)
      else failwith "Reified boolean should be equal to 0 or 1."
    else
      let open Kleene in
      match fst (and_reified (List.map (entailment box) conjunction)) with
      | False -> weak_incremental_closure box (BoxConstraint (Var b, EQ, constant_zero)), true
      | True -> weak_incremental_closure box (BoxConstraint (Var b, EQ, constant_one)), true
      | Unknown -> box, false

  let closure_one box c_idx =
    let box, entailed = propagate_reified box (Parray.get box.reified_constraints c_idx) in
    let deltas = Box.delta box.inner in
    box, entailed, deltas

  let rec closure box =
    let inner = Box.closure box.inner in
    let box = { box with inner } in
    let deltas = Box.delta box.inner in
    if List.length deltas = 0 then box
    else
    begin
      Pengine.react box.engine deltas;
      let engine, box = Pengine.fixpoint box.engine closure_one box in
      closure { box with engine }
    end

  let state_decomposition box =
    let open Kleene in
    let state = if Pengine.num_active_tasks box.engine = 0 then True else Unknown in
    and_kleene state (Box.state_decomposition box.inner)

  let rec print_reified_constraint repr fmt (b, conjunction) =
  begin
    let print_var fmt v = Csp.print_var fmt (R.to_logic_var repr v) in
    Format.fprintf fmt "%a <=> " print_var b;
    let rec aux = function
    | [] -> ()
    | (BoxConstraint c)::l ->
        Format.fprintf fmt "%a " (Csp.print_gconstraint print_var) c;
        aux l
    | (ReifiedConstraint c)::l ->
        print_reified_constraint repr fmt c;
        aux l in
    aux conjunction;
    Format.fprintf fmt "\n";
  end

  let print repr fmt box =
  begin
    Box.print repr fmt box.inner;
    Format.fprintf fmt "\n";
    Parray.iter (print_reified_constraint repr fmt) box.reified_constraints;
  end

  let split box = List.map (fun branch -> { box with inner=branch}) (Box.split box.inner)
end

module BoxReifiedZ(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_int))
module BoxReifiedQ(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_rat))
module BoxReifiedF(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_float))
