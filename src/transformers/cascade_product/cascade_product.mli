(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Bounds
open Lang.Ast
open Typing
open Typing.Ad_type
open Domains.Abstract_domain
open Domains.Interpretation
open Event_loop.Schedulable_abstract_domain

module type Cascade_product_interpretation_sig =
sig
  module A: Abstract_domain
  module B: Abstract_domain
  type t = {
    uid: ad_uid;
    a: A.t ref;
    b: B.t ref;
  }
  type var_id = unit
  type rconstraint

  val exact_interpretation: bool
  val name: string
  val empty: ad_uid -> t
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> vname -> (var_id * Tast.tvariable)
  val local_vars: t -> vname -> var_id list
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> Tast.tqformula
end

module Cascade_product(A: Abstract_domain)(B: Abstract_domain):
sig
  module I : Cascade_product_interpretation_sig
  include Schedulable_abstract_domain with module I := I
  val init: I.t -> t
end with module B = Bound_unit and module I.A = A
