(** This file represents the AST of a constraint problem.
    This syntactic representation can be transformed by the `Representation_sig` of an abstract domain. *)

open Bounds

(** This exception is raised when a constraint is passed to an abstract
   domain that cannot represent this constraint. *)
exception Wrong_modelling of string

(** Variables are identified by a string. *)
type var = string

(** Constants are defined over rational. *)
type i = Bound_rat.t

type annot = Int | Real

(** Unary arithmetic operators. *)
type unop = NEG

(** Binary arithmetic operators. *)
type binop = ADD | SUB | MUL | DIV | POW

(** Arithmetic comparison operators. *)
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT

type expr =
  | Funcall of string * expr list
  | Unary   of unop * expr
  | Binary  of expr * binop * expr
  | Var     of var
  | Cst     of i * annot

(** A binary constraint. *)
type bconstraint = (expr * cmpop * expr)

(** Propositional logic formula. *)
type formula =
  | FVar of var
  | Cmp of bconstraint
  | Equiv of formula * formula
  | Imply of formula * formula
  | And of formula * formula
  | Or  of formula * formula
  | Not of formula

val zero: expr
val one: expr
val two: expr

(** Encoding of true and false as formula. *)
val truef: formula
val falsef: formula

val conjunction: formula list -> formula

(** Checks if an expression contains a variable. *)
val has_variable: expr -> bool

(** Checks if an expression is linear. *)
val is_linear: expr -> bool

(** Checks if a constraints is linear. *)
val is_cons_linear: formula -> bool

val is_cst: expr -> bool

val join_annot: annot -> annot -> annot
