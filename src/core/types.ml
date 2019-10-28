(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

type var_abstract_ty =
  | VUnit
  | Bool
  | Integer
  | Rational
  | Float
  | BDD of int

type var_concrete_ty = Int | Real

type var_ty =
  | Concrete of var_concrete_ty
  | Abstract of var_abstract_ty

let abstract_to_concrete_ty = function
  | VUnit | Bool | Integer | BDD _ -> Int
  | Rational | Float -> Real

let is_continuous = function
  | VUnit | Bool | Integer | BDD _ -> false
  | Rational | Float -> true

let join_concrete_ty a b =
  match a,b with
  | Int,Int -> Int
  | _ -> Real
