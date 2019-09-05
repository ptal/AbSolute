open Bounds

exception Wrong_modelling of string
type var = string
type i = Bound_rat.t
type annot = Int | Real
type unop = NEG
type binop = ADD | SUB | MUL | DIV | POW
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT
type expr =
  | Funcall of string * expr list
  | Unary   of unop * expr
  | Binary  of expr * binop * expr
  | Var     of var
  | Cst     of i * annot

type bconstraint = (expr * cmpop * expr)
type formula =
  | FVar of var
  | Cmp of bconstraint
  | Equiv of formula * formula
  | Imply of formula * formula
  | And of formula * formula
  | Or  of formula * formula
  | Not of formula

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

let one = Cst (Bound_rat.one, Int)
let zero = Cst (Bound_rat.zero, Int)
let two  = Cst (Bound_rat.two, Int)

let join_annot a b =
  match a,b with
  | Int,Int -> Int
  | _ -> Real

let is_cst = function
  | Cst _ -> true
  | _ -> false
