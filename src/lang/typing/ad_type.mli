(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Symbolic representation of the abstract domain by their "types".
    For now, only the most interesting combination are represented, for instance I left out BDD because it is not complete enough to be useful.
    These types should be extended whenever a new domain or combination is added.

    See also the [Typing] module. *)

open Core.Types
open Core

type ad_uid = int

type vardom_ty =
  | Interval of value_ty
  | Interval_oc of value_ty
  | Interval_mix

type ad_ty_ =
  | Box of vardom_ty
  | Octagon of value_ty
  | SAT
  | Logic_completion of ad_ty
  | Propagator_completion of ad_ty
  | Cascade_product of ad_ty * ad_ty
  | Direct_product of ad_ty list
and ad_ty = ad_uid * ad_ty_

val string_of_vardom_ty: vardom_ty -> string
val string_of_adty: ad_ty -> string

(** Rational is more specialized than float (because more precise).
    Integers and rational/float are unordered. *)
val is_more_specialized_value: value_ty -> value_ty -> Kleene.t

val is_more_specialized_vardom: vardom_ty -> vardom_ty -> Kleene.t

(** [subtype a b] is true if a is included in b.
    Example: Box is included in PC(Box X Oct). *)
val subtype: ad_ty -> ad_ty -> bool

(** Give an order over the abstract domains.
    An abstract domain A1 is more specialized than A2 if:
     (1) The constraint language of A1 is included in A2, and
     (2) A1 can infer more information than A2 (regardless of efficiency).
    We have: SAT > Octagon > Box.
    Inductive call on Logic_transformer and Direct_product. *)
val is_more_specialized: ad_ty -> ad_ty -> Kleene.t

module UID2Adty: Map.S with type key=ad_uid

val build_adenv: ad_ty -> ad_ty UID2Adty.t

(** Filter the abstract domains in the list which subsume all the others.
    The list is empty if none exists. *)
val select_mgad: ad_ty list -> ad_ty option

(** True if `adty` supports all the domains `uids` *)
val is_mgad: ad_ty -> ad_uid list -> bool
