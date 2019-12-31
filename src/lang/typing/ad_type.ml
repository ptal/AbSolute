(* Copyright 2019 Pierre Talbot

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

open Domains.Abstract_domain

type value_ty = Z | Q | F

type vardom_ty =
  | Interval of value_ty
  | Interval_mix

type ad_ty_ =
  | Box of vardom_ty
  | Octagon of value_ty
  | SAT
  | Logic_product of ad_ty list
  | Product of ad_ty list
and ad_ty = ad_uid * ad_ty_

(** Associate the UID of an abstract element to its type.
    This environment is mainly useful in the typing function converting a `qformula` into a `tformula` in order to know what abstract domains are available.
    This environment is built by calling `type_of a` over an abstract element `a`. *)
type typing_env = ad_ty list
