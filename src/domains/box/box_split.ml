open Box_representation

module type Variable_order = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  val select: R.Store.t -> (R.var_id * R.var_dom) option
end with module R=R

module type Value_order = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  val select: R.var_dom -> R.Vardom.B.t
end with module R=R

module type Distributor = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  val distribute: R.var_id -> R.Vardom.B.t -> R.rconstraint list
end with module R=R

module type Box_split_sig = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  val split: R.Store.t -> R.rconstraint list
end with module R=R

module Input_order(R: Box_rep_sig) =
struct
  module R=R
  module Store = R.Store
  module V = R.Vardom

  exception Found_var of R.var_id * R.var_dom
  let select store =
    try
      Store.iter (fun v d -> if not (V.is_singleton d) then raise (Found_var (v,d))) store;
      None
    with Found_var (v,d) -> Some (v,d)
end

(* This module factorizes `First_fail` and `Anti_first_fail`. *)
module Width_order(R: Box_rep_sig) =
struct
  module R=R
  module V = R.Vardom
  module B = V.B
  let select store width_cmp =
    let size (l,h) = B.sub_up h l in
    let var =
      R.Store.fold (fun a v d ->
        if V.is_singleton d then a
        else
          let width = size (V.to_range d) in
          match a with
          | Some (best,_,_) when width_cmp width best -> Some (width,v,d)
          | Some _ -> a
          | None -> Some (width,v,d))
      None store in
    match var with
    | Some (_, v,d) -> Some (v,d)
    | None -> None
end

module First_fail(R: Box_rep_sig) =
struct
  module R=R
  module W = Width_order(R)
  let select store = W.select store R.Vardom.B.lt
end

module Anti_first_fail(R: Box_rep_sig) =
struct
  module R=R
  module W = Width_order(R)
  let select store = W.select store R.Vardom.B.gt
end

module Middle (R: Box_rep_sig) =
struct
  module R=R
  module V = R.Vardom
  module B = V.B
  let select vardom =
    let (l,u) = V.to_range vardom in
    B.div_down (B.add_up l u) B.two
end

module Lower_bound (R: Box_rep_sig) =
struct
  module R=R
  module V = R.Vardom
  let select vardom = fst (V.to_range vardom)
end

module Upper_bound (R: Box_rep_sig) =
struct
  module R=R
  module V = R.Vardom
  let select vardom = snd (V.to_range vardom)
end

module Assign (R: Box_rep_sig) =
struct
  module R=R
  module V=R.Vardom
  let make_cst v = R.(make_expr (BCst (V.create (V.OF_BOUNDS(v, v)))))
  let distribute var_idx value =
    R.[(make_expr (BVar var_idx), Csp.EQ, make_cst value);
       (make_expr (BVar var_idx), Csp.NEQ, make_cst value)]
end

module Bisect (R: Box_rep_sig) =
struct
  module R=R
  module V=R.Vardom
  let make_cst v = R.(make_expr (BCst (V.create (V.OF_BOUNDS(v, v)))))
  let distribute var_idx value =
    R.[(make_expr (BVar var_idx), Csp.LEQ, make_cst value);
       (make_expr (BVar var_idx), Csp.GT, make_cst value)]
end

module Make
  (VARIABLE: Variable_order)
  (VALUE: Value_order)
  (DISTRIB: Distributor)
  (R: Box_rep_sig) =
struct
  module R = R
  module Variable = VARIABLE(R)
  module Value = VALUE(R)
  module Distrib = DISTRIB(R)

  let split store =
    match Variable.select store with
    | None -> []
    | Some (var_idx, vardom) ->
        Distrib.distribute var_idx (Value.select vardom)
end

module First_fail_bisect = Make(First_fail)(Middle)(Bisect)
module First_fail_LB = Make(First_fail)(Lower_bound)(Assign)
module Anti_first_fail_LB = Make(Anti_first_fail)(Lower_bound)(Assign)
module Anti_first_fail_UB = Make(Anti_first_fail)(Upper_bound)(Assign)
module MSLF_simple = First_fail_LB
