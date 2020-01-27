(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Typing
open Vardom
open Var_store
open Domains.Interpretation

module type Box_interpretation_sig =
sig
  module Vardom: Vardom_sig.S

  (** We depend on the store to represent the `var_id`.
      However, the store is contained in `Box` itself and not here.
      NOTE: We could parametrize Box_interpretation_sig with a `Store` if we have different kind of stores in the future. *)
  module Store: Var_store_sig with module V=Vardom

  include module type of (Interpretation_ground(struct type var_id=Store.key end))

  type var_dom = Store.cell
  type rconstraint = var_id * var_dom

  val exact_interpretation: bool
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> Tast.tqformula
end

module type Box_interpretation_functor = functor (Vardom: Vardom_sig.S) -> Box_interpretation_sig
  with module Vardom=Vardom

module Box_interpretation: Box_interpretation_functor
