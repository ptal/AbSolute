open Csp
open Dbm

let rec normalize_expr e =
  let neg e = Unary (NEG, e) in
  match e with
  | Unary (NEG, Cst(c,a)) -> Cst (Bound_rat.neg c, a)
  | Unary (NEG, Unary (NEG, e)) -> normalize_expr e
  | Unary (NEG, Binary (SUB, x, y)) -> normalize_expr (Binary (ADD, neg x, y))
  | Unary (NEG, Binary (ADD, x, y)) -> normalize_expr (Binary (SUB, neg x, y))
  | Unary (op, x) -> Unary(op, normalize_expr x)
  | Binary (SUB, x, Unary (NEG, y)) -> normalize_expr (Binary (ADD, x, y))
  | Binary (ADD, x, Unary (NEG, y)) -> normalize_expr (Binary (SUB, x, y))
  | Binary (op, x, y) -> Binary (op, normalize_expr x, normalize_expr y)
  | e -> e

let normalize (e1, op, e2) = ((normalize_expr e1), op, (normalize_expr e2))

let rec generic_rewrite c =
  match normalize c with
  | e1, GEQ, e2 -> generic_rewrite (Unary (NEG, e1), LEQ, Unary (NEG, e2))
  | e1, GT, e2 -> generic_rewrite (Unary (NEG, e1), LT, Unary (NEG, e2))
  | Var x, op, Var y -> generic_rewrite (Binary (SUB, Var x, Var y), op, Cst (Bound_rat.zero, Int))
  | e1, EQ, e2 -> (generic_rewrite (e1, LEQ, e2))@(generic_rewrite (e1, GEQ, e2))
  | c -> [c]

module type Octagon_rep_sig =
sig
  module B: Bound_sig.BOUND
  type t
  type var_kind = unit
  type var_id = dbm_interval
  type rconstraint = B.t dbm_constraint
  val empty: t
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> bformula -> rconstraint list
  val relax: t -> bformula -> rconstraint list
  val negate: rconstraint -> rconstraint
end

module Octagon_rep(B: Bound_sig.BOUND) =
struct
  type var_id = dbm_interval
  type var_kind = unit
  type rconstraint = B.t dbm_constraint

  module B = B
  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=var_id
    let compare = compare end)

  type t = {
    (* maps each variable name to its DBM interval. *)
    env: var_id Env.t;
    (* reversed mapping of `env`. *)
    renv: var REnv.t;
  }

  let empty = {env=Env.empty; renv=REnv.empty}
  let extend repr (v,itv) = {
    env=(Env.add v itv repr.env);
    renv=(REnv.add itv v repr.renv);
  }

  let to_logic_var repr itv = REnv.find itv repr.renv
  let to_abstract_var repr v = Env.find v repr.env

  let dim_of_var repr v =
    let itv = to_abstract_var repr v in
    let k1, k2 = (itv.lb.l / 2), (itv.lb.c / 2) in
    if k1 <> k2 then failwith "Octagon_rep.dim_of_var: only variable with a canonical plane are defined on a single dimension."
    else k1

  (** If the bound is discrete, it reformulates strict inequalities `<` into the inequality `<=` (dual `>` is handled by `generic_rewrite`).
      Contrarily to `relax`, the rewritten constraint is logically equivalent to the initial constraint.
      For example, it rewrites `x - y < d` into `x - y <= d - 1`. *)
  let reformulate c =
    if B.is_continuous then c
    else
      match c with
      | e1, LT, Cst (d, a) -> normalize (e1, LEQ, Cst (Bound_rat.sub_up d Bound_rat.one, a))
      | c -> c

  (** `x <= d` ~> `x + x <= 2d` *)
  let x_leq_d x c = {v=make_var (x+1) x; d=(B.mul_up c B.two)}

  (** `-x <= d` ~> `-x - x <= 2d` *)
  let minus_x_leq_d x c = {v=make_var x (x+1); d=(B.mul_up c B.two)}

  (* NOTE: we invert `x` and `y` because dbm_var is defined as {x; y} where `x` is the line and `y` the column.
     In an expression such as `X - Y <= 10`, `X` is the column and `Y` the line. *)

  (** `x + y <= d` *)
  let x_plus_y_leq_d x y d = {v=make_var (y+1) x; d=d}

  (** `x - y <= d` *)
  let x_minus_y_leq_d x y d = {v=make_var y x; d=d}

  (** `-x + y <= d` *)
  let minus_x_plus_y_leq_d x y d = {v=make_var (y+1) (x+1); d=d}

  (** `-x - y <= d` *)
  let minus_x_minus_y_leq_d x y d = {v=make_var y (x+1); d=d}

  let map_to_dim r f x d = f ((dim_of_var r x)*2) (B.of_rat_up d)
  let map2_to_dim r f x y d = f ((dim_of_var r x)*2) ((dim_of_var r y)*2) (B.of_rat_up d)

  (* Try creating an octagonal constraint from a normalized form.
     The constraint should be processed by `generic_rewrite` first. *)
  let try_create r = function
    | Var x, LEQ, Cst (d, _) -> Some (map_to_dim r x_leq_d x d)
    | Unary (NEG, Var x), LEQ, Cst (d, _) -> Some (map_to_dim r minus_x_leq_d x d)
    | Binary (ADD, Var x, Var y), LEQ, Cst (d, _) ->  Some (map2_to_dim r x_plus_y_leq_d x y d)
    | Binary (SUB, Var x, Var y), LEQ, Cst (d, _) ->  Some (map2_to_dim r x_minus_y_leq_d x y d)
    | Binary (ADD, Unary (NEG, Var x), Var y), LEQ, Cst (d, _) ->  Some (map2_to_dim r minus_x_plus_y_leq_d x y d)
    | Binary (SUB, Unary (NEG, Var x), Var y), LEQ, Cst (d, _) ->  Some (map2_to_dim r minus_x_minus_y_leq_d x y d)
    | _ -> None

  let unwrap_all constraints =
    if List.for_all Tools.is_some constraints then
      List.map Tools.unwrap constraints
    else []

  let rewrite_atom repr c =
    generic_rewrite c |>
    List.map (fun c -> try_create repr (reformulate c)) |>
    unwrap_all

  let rewrite repr formula =
    try mapfold_conjunction (rewrite_atom repr) formula
    with Wrong_modelling _ -> []

  let relax_atom repr c =
    List.flatten (List.map (fun c ->
      match c with
      | e1, LT, e2 -> rewrite_atom repr (e1, LEQ, e2)
      | c -> []
    ) (generic_rewrite c))

  let relax repr formula =
    try mapfold_conjunction (relax_atom repr) formula
    with Wrong_modelling _ -> []

  let negate c =
    if is_rotated c.v then
      { v=(Dbm.inv c.v); d=B.neg (B.succ c.d) }
    else
      { v=(Dbm.inv c.v); d=B.mul_up (B.neg (B.succ (B.div_down c.d B.two))) B.two }
end
