open Format
open Ast

val print_unop: formatter -> unop -> unit
val print_binop: formatter -> binop -> unit
val print_cmpop: formatter -> cmpop -> unit
val print_var: formatter -> var -> unit
val prior_level: (expr * expr) -> int

val must_parenthesis_right: expr -> expr -> bool

(** This function prints an expression without redundant parenthesis
   and using continuation passing style to avoid a stack overflow
   error over very large constraints. *)
val print_expr: formatter -> expr -> unit
val print_formula: formatter -> formula -> unit
val print_constraint: formatter -> bconstraint -> unit

val output_constraints: bconstraint list -> unit
val print_constraints: formatter -> formula list -> unit

val string_of_constraint: bconstraint -> string
