open Ast

(** Every atoms `a` in `expr` is transformed to `a binop expr'`. *)
val distribute: (binop * expr) -> expr -> expr

(** Propagate constants in expression. *)
val expand: expr -> expr

(** Simplify elementary functions. *)
val simplify_fp: expr -> expr

val simplify_bexpr: bexpr -> bexpr

val left_hand_side: (expr * cmpop * expr) -> (cmpop * expr)
val left_hand: bexpr -> (cmpop * expr)
val is_arith: bexpr -> bool

(** Iterate on expression. *)
val iter_expr: (expr -> unit) -> expr -> unit

(** Iterate on expressions and all subformulas. *)
val iter_subformula: (expr -> unit) -> (bexpr -> unit) -> bexpr -> unit

(** Map constraints of a Boolean formula. *)
val map_constraint: (bconstraint -> bconstraint) -> bexpr -> bexpr

(** Inversion of the comparison operator.
    For example, <= becomes >=.
    See also `neg`. *)
val inv: cmpop -> cmpop

(** Negate the comparison operator. *)
val neg: cmpop -> cmpop

(** Negate the constraint. *)
val neg_bexpr: bexpr -> bexpr
val neg_bconstraint: bconstraint -> bconstraint

val replace_cst_expr: (var * i) -> expr -> expr
val replace_cst_bexpr: (var * i) -> bexpr -> bexpr

module Variables: Set.S

val get_vars_expr: expr -> var list
val get_vars_set_expr: expr -> Variables.t
val get_vars_bexpr: bexpr -> var list
val get_vars_set_bexpr: bexpr -> Variables.t
val vars_of_bconstraint: bconstraint -> var list

(* True if the constraint is fully defined over the set of variables `vars`. *)
val is_defined_over: var list -> bconstraint -> bool

val from_cst_to_expr: (var * (i * i)) -> bconstraint list
val csts_to_expr: (var * (i * i)) list -> bconstraint list

val replace_var_in_expr: (var -> expr) -> expr -> expr
