open Csp
open Abstract_domain
open Box_dom
open Box_representation

type rbox_constraint =
  | BoxConstraint of box_constraint
  | ReifiedConstraint of box_reified_constraint
and box_reified_constraint = box_var * rbox_constraint list

let recursive_reified () = failwith "Reified constraint inside reified constraint is not implemented."

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
  val rewrite: t -> bconstraint -> rconstraint list
  val rewrite_reified: t -> var -> bconstraint list -> rconstraint list
  val relax: t -> bconstraint -> rconstraint list
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
    let constraints = List.flatten (List.map (R.rewrite repr) constraints) in
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
  val incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> kleene
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> kleene
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
    reified_constraints: box_reified_constraint list;
  }

  let empty = {
    inner=Box.empty;
    reified_constraints=[]
  }

  let extend box () =
    let (inner, idx) = Box.extend box.inner () in
    ({ box with inner=inner; }, idx)

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
    | ReifiedConstraint c -> {box with reified_constraints=c::box.reified_constraints}

  let propagate_negation_conjunction box (b, conjunction) =
    match and_reified (List.map (entailment box) conjunction) with
    | False,_ -> box
    | True,_ -> raise Bot.Bot_found
    | Unknown, Some(u) ->
        let c = (List.nth conjunction u) in
        weak_incremental_closure box (R.negate c)
    | Unknown, None ->
        { box with reified_constraints=(b, conjunction)::box.reified_constraints}

  (* Propagate the reified constraints.
     Entailed reified constraints are removed from `box`. *)
  let propagate_reified box (b, conjunction) =
    let itv = Box.project_itv box.inner b in
    if Box.I.is_singleton itv then
      let (value,_) = Box.I.to_range itv in
      if B.equal B.one value then
        List.fold_left weak_incremental_closure box conjunction
      else if B.equal B.zero value then
        propagate_negation_conjunction box (b, conjunction)
      else failwith "Reified boolean should be equal to 0 or 1."
    else
      match fst (and_reified (List.map (entailment box) conjunction)) with
      | False -> weak_incremental_closure box (BoxConstraint (Var b, EQ, constant_zero))
      | True -> weak_incremental_closure box (BoxConstraint (Var b, EQ, constant_one))
      | Unknown -> { box with reified_constraints=(b, conjunction)::box.reified_constraints }

  let propagate_all_reified box =
    List.fold_left propagate_reified { box with reified_constraints=[] } box.reified_constraints

  let rec propagate box =
    let box = { box with inner=Box.closure box.inner } in
    let count_reified = List.length box.reified_constraints in
    let box = propagate_all_reified box in
    let count_reified' = List.length box.reified_constraints in
    if count_reified <> count_reified' then
      propagate box
    else
      box

  let closure box = propagate box

  let incremental_closure box c = closure (weak_incremental_closure box c)

  let state_decomposition box =
    let state = if (List.length box.reified_constraints) = 0 then True else Unknown in
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
    List.iter (print_reified_constraint repr fmt) box.reified_constraints;
  end

  let split box = List.map (fun branch -> { box with inner=branch}) (Box.split box.inner)
end

module BoxReifiedZ(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_int))
module BoxReifiedQ(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_rat))
module BoxReifiedF(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_float))
