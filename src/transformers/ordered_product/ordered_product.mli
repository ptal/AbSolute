(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This is a direct product combining N abstract domains A1...An in an ordered fashion.
    In particular, when adding a constraint, we try to add it into Ai and if it fails into A(i+1).
    This direct product slightly generalizes the "specialization product" presented in the dissertation of Ghiles Ziat (2019).  *)

open Domains.Abstract_domain
open Domains.Interpretation
open Lang.Ast
open Core
open Core.Types
open Bounds

(** Generic variables and constraints. *)
type gvar = ad_uid * int
type gconstraint = ad_uid * int

module type Prod_combinator =
sig
  type init_t
  type t
  type var_id = gvar
  type rconstraint = gconstraint
  val count: int

  val init: init_t -> t
  val empty: unit -> t
  val extend: t -> (var * gvar * var_abstract_ty) -> t
  val to_logic_var: t -> gvar -> (var * var_abstract_ty)
  val to_abstract_var: t -> var -> (gvar * var_abstract_ty)
  val interpret: t -> approx_kind -> formula -> (t * gconstraint list) option
  val to_qformula: t -> gconstraint list -> qformula
  val qinterpret: t -> approx_kind -> qformula -> t option

  val empty': ad_uid -> t
  val extend': ?ty:var_ty -> t -> (t * gvar * var_abstract_ty)
  val project: t -> gvar -> (Bound_rat.t * Bound_rat.t)
  type snapshot
  val restore: t -> snapshot -> t
  val lazy_copy: t -> int -> snapshot list
  val closure: t -> (t * bool)
  val weak_incremental_closure: t -> gconstraint -> t
  val entailment: t -> gconstraint -> bool
  val split: t -> snapshot list
  val volume: t -> float
  val state: t -> Kleene.t
  val print: Format.formatter -> t -> unit
end

module Prod_atom(A: Abstract_domain) :
  Prod_combinator with type init_t = A.t ref

module Prod_cons(A: Abstract_domain)(B: Prod_combinator) :
  Prod_combinator with
    type t = Prod_atom(A).t * B.t and
    type init_t = A.t ref * B.init_t

module Ordered_product(P: Prod_combinator) :
sig
  include Abstract_domain
  val init: ad_uid -> P.init_t -> t
end with module B=Bound_rat
