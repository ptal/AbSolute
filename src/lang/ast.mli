(** This file represents the AST of a constraint problem.
    This syntactic representation can be transformed by the `Representation_sig` of an abstract domain. *)

open Bounds

(* variables are identified by a string *)
type var = string

(* constants are rationals (the domain of the variable *)
type i = Bound_rat.t

type annot = Int | Real

(* unary arithmetic operators *)
type unop = NEG

(* binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW

(* arithmetic comparison operators *)
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT

(* Expressions. *)
type expr =
  | Funcall of string * expr list
  | Unary   of unop * expr
  | Binary  of expr * binop * expr
  | Var     of var
  | Cst     of i * annot

(** A binary constraint. *)
type bconstraint = (expr * cmpop * expr)

(* boolean expressions *)
type bexpr =
  | Cmp of bconstraint
  | And of bexpr * bexpr
  | Or  of bexpr * bexpr
  | Not of bexpr

(** Checks if an expression contains a variable. *)
val has_variable: expr -> bool

(** Checks if an expression is linear *)
val is_linear: expr -> bool

(* checks if a constraints is linear *)
val is_cons_linear: bexpr -> bool

val zero: expr
val one: expr
val two: expr

val is_cst: expr -> bool

val join_annot: annot -> annot -> annot
