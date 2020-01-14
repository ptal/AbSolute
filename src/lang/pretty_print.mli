(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Format
open Ast

val print_unop: formatter -> unop -> unit
val print_binop: formatter -> binop -> unit
val print_cmpop: formatter -> cmpop -> unit
val print_var: formatter -> vname -> unit
val prior_level: (expr * expr) -> int

val must_parenthesis_right: expr -> expr -> bool

(** This function prints an expression without redundant parenthesis
   and using continuation passing style to avoid a stack overflow
   error over very large constraints. *)
val print_expr: formatter -> expr -> unit
val print_formula: formatter -> formula -> unit
val print_qformula: formatter -> qformula -> unit
val print_constraint: formatter -> bconstraint -> unit

val output_constraints: bconstraint list -> unit
val print_constraints: formatter -> formula list -> unit

val string_of_constraint: bconstraint -> string
