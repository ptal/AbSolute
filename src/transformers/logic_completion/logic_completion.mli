(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** `Logic_completion` equips an abstract domain with support for logical formula.
    The main operation required on the underlying abstract domain is `entailment`.
     It generalizes to any propositional logical formula the results presented in the paper:
      "Combining Constraint Languages via Abstract Interpretation" (Talbot and al., 2019).
    It is also related to `Disjunctive completion` in abstract interpretation. *)

open Bounds
open Lang.Ast
open Typing
open Typing.Ad_type
open Domains.Abstract_domain
open Domains.Interpretation
open Event_loop.Schedulable_abstract_domain

(** Variables cannot be represented in this abstract domain.
    They must be interpreted through the corresponding sub-domain or in another reduced product. *)
module type Logic_completion_interpretation_sig =
sig
  module A: Abstract_domain
  type t = {
    a: A.t ref;
    uid: ad_uid;
  }
  type var_id = unit
  type rconstraint

  val empty: ad_uid -> t
  val exists: t -> vname -> bool
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> vname -> (var_id * Tast.tvariable)

  (** The formula is not rewritten and is represented as such. *)
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> Tast.tqformula
end

module Logic_completion(A: Abstract_domain):
sig
  include Schedulable_abstract_domain
  val init: I.t -> t
end with module B = Bound_unit
