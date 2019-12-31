(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** We represent the abstract and concrete types of variables.
    Design rational: We need to distinguish between concrete and abstract types because `Real` can be "instantiated" to a float or rational number.
    Rational are more precise than float but in essence they aim to represent the same solutions space.
    Note: It is not in `Lang` to avoid circular dependencies between `Bounds` and `Lang`.
    Indeed, we need to associate an abstract type to each bound. *)

(** [value_ty] symbolically represents integers (Z), rational (Q), and floating point numbers (F).
    It is also used in [Lang.Typing.Tast]. *)
type value_ty = Z | Q | F

(** `Integer` is considered to be over 32 bits. *)
type var_abstract_ty =
  | VUnit
  | Bool
  | Machine of value_ty (** Machine representation of [value_ty]. *)
  | BDD of int

type var_concrete_ty = Int | Real

type var_ty =
  | Concrete of var_concrete_ty
  | Abstract of var_abstract_ty

val string_of_value_ty: value_ty -> string
val string_of_cty: var_concrete_ty -> string
val string_of_aty: var_abstract_ty -> string
val string_of_ty: var_ty -> string

val abstract_to_concrete_ty: var_abstract_ty -> var_concrete_ty

(** True if the abstract element do not have machine-representable successors or predecessors. *)
val is_continuous: var_abstract_ty -> bool

val join_concrete_ty: var_concrete_ty -> var_concrete_ty -> var_concrete_ty

(** We compare the abstract type with respect to their precision.
    Abstract types that do not have the same concrete type are unordered (Kleene.Unknown). *)
val less_precise_than: var_abstract_ty -> var_abstract_ty -> Kleene.t
