(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** `Logic_product` is a reduced product combining heterogeneous constraints languages through a logic formula.
     It generalizes to any propositional logical formula the results presented in the paper:
      "Combining Constraint Languages via Abstract Interpretation" (Talbot and al., 2019). *)

open Core.Types
open Bounds
open Lang.Ast
open Domains.Interpretation
open Domains.Abstract_domain
open Ordered_product
open Event_loop.Event_abstract_domain

(** For a single logic formula, we have 4 variants of the formula.
    We always keep the formula and its negation.
    Then, for both of these, we have a version for entailment querying (ask) and for joining the constraint in the abstract element (tell).
    Note that ask and tell formulas are tightly bound to under- and over-approximation.
    Alternatively, we could just compute these variants on the fly in `incremental_closure` and `entailment`.
    This version is more efficient because it precomputes these variants.

    This structure could be simplified by rewritting `PImply` and `PEquiv`.
    For now, we prefer to keep the structure of the formula as much as possible, but I am unsure that if it is more efficient or not. *)
type qfp_formula =
  | Atom of gconstraint list
  | PNot of pn_formula
  | PAnd of pn_formula * pn_formula
  | POr of pn_formula * pn_formula
  | PImply of pn_formula * pn_formula
  | PEquiv of pn_formula * pn_formula
and approx_formula = {
  ask: qfp_formula; (** Entailment query of a formula by under-approximation. *)
  tell: qfp_formula; (** Join of a formula by over-approximation (or under-approximation depending on the user choice). *)
}
and pn_formula = {
  positive: approx_formula;
  negative: approx_formula;
}

(** Variables cannot be represented in this abstract domain.
    They must be interpreted through the corresponding sub-domain or in another reduced product. *)
module type Logic_prod_interpretation_sig =
sig
  type t
  type init_t
  type var_id = ()
  type rconstraint = qfp_formula

  val init: init_t -> t
  val empty: unit -> t
  val extend: t -> (var * gvar * var_abstract_ty) -> t
  val to_logic_var: t -> gvar -> (var * var_abstract_ty)
  val to_abstract_var: t -> var -> (gvar * var_abstract_ty)
  val interpret: t -> approx_kind -> formula -> (t * qfp_formula list) option
  val to_qformula: t -> qfp_formula list -> qformula
end

module type LProd_combinator =
sig
  include Prod_combinator
  val drain_events: t -> (t * event list)
  val events_of: t -> rconstraint -> event list
end

module Logic_prod_interpretation(P: LProd_combinator):
  Logic_prod_interpretation_sig with
    type t = P.t and
    type init_t = P.init_t

module LProd_atom(A: Event_abstract_domain) : LProd_combinator

module LProd_cons(A: Event_abstract_domain)(B: LProd_combinator) : LProd_combinator

module Logic_product(P: LProd_combinator):
sig
  include Schedulable_abstract_domain
  val init: ad_uid -> P.init_t -> t
end with module B = Bound_unit
