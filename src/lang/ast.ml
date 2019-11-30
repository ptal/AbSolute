(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Bounds
open Core
open Core.Types

exception Wrong_modelling of string
type var = string
type i = Bound_rat.t
type unop = NEG
type binop = ADD | SUB | MUL | DIV | POW
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT
type expr =
  | Funcall of string * expr list
  | Unary   of unop * expr
  | Binary  of expr * binop * expr
  | Var     of var
  | Cst     of i * var_concrete_ty

type bconstraint = (expr * cmpop * expr)
type formula =
  | FVar of var
  | Cmp of bconstraint
  | Equiv of formula * formula
  | Imply of formula * formula
  | And of formula * formula
  | Or  of formula * formula
  | Not of formula

type qformula =
  | QFFormula of formula
  | Exists of var * var_ty * qformula

type optimization_kind =
  | Minimize of var
  | Maximize of var
  | Satisfy

type bab_qformula = {
  qf: qformula;
  optimise: optimization_kind
}

let one = Cst (Bound_rat.one, Int)
let zero = Cst (Bound_rat.zero, Int)
let two  = Cst (Bound_rat.two, Int)

let truef = Cmp (zero, LEQ, zero)
let falsef = Cmp (one, LEQ, zero)

let rec has_variable = function
  | Funcall(_, args) -> List.exists has_variable args
  | Unary (_, e) -> has_variable e
  | Binary (e1, _, e2) -> has_variable e1 || has_variable e2
  | Var _ -> true
  | Cst _ -> false

let rec is_linear = function
  | Unary (NEG,e) -> is_linear e
  | Binary(e1, MUL, e2) | Binary(e1, DIV, e2)
    -> not (has_variable e1 && has_variable e2) && is_linear e1 && is_linear e2
  | Binary(e1, POW, e2) -> not (has_variable e1 || has_variable e2)
  | Binary(e1, _, e2) -> is_linear e1 && is_linear e2
  | Var _ | Cst _ -> true
  | _ -> false

let rec is_cons_linear = function
  | Cmp (e1,_,e2) -> is_linear e1 && is_linear e2
  | FVar _ -> true
  | Equiv (b1,b2)
  | Imply (b1,b2)
  | And (b1,b2)
  | Or (b1,b2) -> is_cons_linear b1 && is_cons_linear b2
  | Not b -> is_cons_linear b

let is_cst = function
  | Cst _ -> true
  | _ -> false

let type_dispatch (module B: Bound_sig.S) from ty f =
  match ty with
  | (Concrete ty) when ty=B.concrete_ty -> f ()
  | (Abstract ty) when
      (less_precise_than ty B.abstract_ty) = Kleene.True -> f ()
  | ty -> raise (Wrong_modelling (
      from ^ "(" ^ (string_of_aty B.abstract_ty) ^ ") does not support " ^
      (string_of_ty ty)))
