(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

type value_ty = Z | Q | F

type var_abstract_ty =
  | VUnit
  | Bool
  | Machine of value_ty
  | BDD of int

type var_concrete_ty = Int | Real

type var_ty =
  | Concrete of var_concrete_ty
  | Abstract of var_abstract_ty

let string_of_cty = function
  | Int -> "Int"
  | Real -> "Real"

let string_of_value_ty_short = function
  | Z -> "Z"
  | Q -> "Q"
  | F -> "F"

let string_of_value_ty = function
  | Z -> "Integer"
  | Q -> "Rational"
  | F -> "Float"

let string_of_aty = function
  | VUnit -> "Unit"
  | Bool -> "Bool"
  | Machine ty -> string_of_value_ty ty
  | BDD x -> "BDD(" ^ (string_of_int x) ^ ")"

let string_of_ty = function
  | Concrete x -> string_of_cty x
  | Abstract x -> string_of_aty x

let abstract_to_concrete_ty = function
  | VUnit | Bool | Machine Z | BDD _ -> Int
  | Machine Q | Machine F -> Real

let to_concrete_ty = function
  | Abstract ty -> abstract_to_concrete_ty ty
  | Concrete ty -> ty

let is_continuous = function
  | VUnit | Bool | Machine Z | BDD _ -> false
  | Machine Q | Machine F -> true

let join_concrete_ty a b =
  match a,b with
  | Int,Int -> Int
  | _ -> Real

let bits_of = function
  | VUnit -> 0
  | Bool -> 1
  | Machine Z -> 32
  | Machine Q -> max_int
  | Machine F -> 32
  | BDD n -> n

let less_precise_than less more =
  let cless, cmore =
    abstract_to_concrete_ty less, abstract_to_concrete_ty more in
  if cless != cmore then Kleene.Unknown
  else
    (* When considering the abstract types (for a same concrete domain),
       we can simply compare their numbers of bits. *)
    let bless, bmore = bits_of less, bits_of more in
    if bless <= bmore then Kleene.True
    else Kleene.False
