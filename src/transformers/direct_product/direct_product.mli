(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This is a direct product combining N abstract domains A1...An.
    Whenever a variable or a constraint is typed with the UID of this product, it is added in ALL sub-domains.
    However, for functions operating on a single variable such as `to_abstract_var`, `project` or `embed`, only the first sub-domain is considered.
    To operate over a particular sub-domain, retrieve the variable ID with `local_vars`. *)

open Domains.Abstract_domain
open Domains.Interpretation
open Lang.Ast
open Core
open Bounds
open Typing
open Typing.Ad_type
open Typing.Tast

(** Generic variables and constraints. *)
type gvar = ad_uid * int
type gconstraint = ad_uid * int

module type Prod_combinator =
sig
  type init_t
  type t
  type var_id = gvar
  type rconstraint = gconstraint
  val exact_interpretation: bool
  val count: int
  val name: string

  val init: init_t -> t
  val empty: unit -> t
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> vname -> (var_id * Tast.tvariable)
  val local_vars: t -> vname -> var_id list
  val to_qformula: t -> gconstraint list -> tqformula list

  (** Interpret a constraint in the product.
      Either we interpret this constraint in all components of the product (the type of the constraint is the UID of the product), or in only one.
      Similarly for variables. *)
  val interpret_all: t -> approx_kind -> tformula -> t * gconstraint list
  val interpret_one: t -> approx_kind -> tformula -> t * gconstraint list
  val extend_var_all: t -> approx_kind -> tvariable -> t * gconstraint list
  val extend_var_one: t -> approx_kind -> tvariable -> t * gconstraint list

  val empty': ad_uid -> t
  val type_of: t -> ad_ty list
  val project: t -> var_id -> (Bound_rat.t * Bound_rat.t)
  val embed: t -> gvar -> (Bound_rat.t * Bound_rat.t) -> t
  type snapshot
  val restore: t -> snapshot -> t
  val lazy_copy: t -> int -> snapshot list
  val closure: t -> (t * bool)
  val weak_incremental_closure: t -> gconstraint -> t
  val entailment: t -> gconstraint -> t * gconstraint * bool
  val split: (ad_uid * search_strategy) -> t -> snapshot list
  val volume: t -> float
  val state: t -> Kleene.t
  val print: Format.formatter -> t -> unit
  val drain_events: t -> (t * event list)
  val events_of: t -> rconstraint -> event list
  val events_of_var: t -> var_id -> event list
end

type 'a owned_or_shared =
  Owned of 'a
| Shared of 'a

module Prod_atom(A: Abstract_domain) :
sig
  include Prod_combinator
  val unwrap: t -> A.t
  val wrap: t -> A.t -> t
  val get_constraint: t -> rconstraint -> A.I.rconstraint
  val uid: t -> ad_uid
end with type init_t = (A.t ref) owned_or_shared

module Prod_cons(A: Abstract_domain)(B: Prod_combinator) :
  Prod_combinator with
    type t = Prod_atom(A).t * B.t and
    type init_t = Prod_atom(A).init_t * B.init_t

module Direct_product(P: Prod_combinator):
sig
  include Abstract_domain
  val init: ad_uid -> P.init_t -> t
end with module B=Bound_rat
