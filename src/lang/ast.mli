(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This file represents the AST of a constraint problem.
    This syntactic representation can be transformed by the `Representation_sig` of an abstract domain. *)

open Bounds
open Core.Types

(** This exception is raised when a constraint is passed to an abstract
   domain that cannot represent this constraint. *)
exception Wrong_modelling of string

(** Variables are identified by a string. *)
type var = string

(** Constants are defined over rational because they can represent exactly integers or floating point numbers. *)
type i = Bound_rat.t

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
  | Cst     of i * var_concrete_ty

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

(** Formula with existential quantifier.
    It is mainly useful for declaring the concrete type of a variable's value. *)
type qformula =
  | QFFormula of formula
  | Exists of var * var_ty * qformula

val zero: expr
val one: expr
val two: expr

(** Encoding of true and false as formula. *)
val truef: formula
val falsef: formula

(** Checks if an expression contains a variable. *)
val has_variable: expr -> bool

(** Checks if an expression is linear. *)
val is_linear: expr -> bool

(** Checks if a constraints is linear. *)
val is_cons_linear: formula -> bool

val is_cst: expr -> bool