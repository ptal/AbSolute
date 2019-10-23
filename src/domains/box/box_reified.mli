(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)


(** This module equips `Box_dom` with reified constraints. *)
open Core
open Box_dom
open Box_representation
open Lang.Ast
open Vardom
open Domains.Abstract_domain

module type Reified_box_rep_sig =
sig
  module BaseRep : Box_rep_sig
  type t
  type var_kind = BaseRep.var_kind
  type var_id = BaseRep.var_id
  type rconstraint =
  | BaseConstraint of BaseRep.rconstraint
  | ReifiedConstraint of reified_constraint
  and reified_constraint = BaseRep.var_id * rconstraint list

  val empty: t
  val extend: t -> (var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> formula -> rconstraint list
  (* This is a temporary function. We should generalized Representation_sig to formula rather than only constraint. *)
  val rewrite_reified: t -> var -> bconstraint list -> rconstraint list
  val relax: t -> formula -> rconstraint list
  val negate: rconstraint -> rconstraint
end

module Reified_box_rep(BaseRep: Box_rep_sig) : Reified_box_rep_sig
  with module BaseRep=BaseRep

module type Box_reified_sig =
sig
  type t
  module R: Reified_box_rep_sig
  module Vardom: Vardom_sig.Vardom_sig
  module B = Vardom.B
  type bound = B.t
  type vardom = Vardom.t

  val empty: ad_uid -> t
  val uid: t -> ad_uid
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (Vardom.B.t * Vardom.B.t)
  val project_vardom: t -> R.var_id -> vardom
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> Kleene.t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> Kleene.t
  val print: R.t -> Format.formatter -> t -> unit
end

module Make(Box: Box_sig) : Box_reified_sig
module BoxReifiedZ(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
module BoxReifiedQ(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
module BoxReifiedF(SPLIT: Box_split.Box_split_sig) : Box_reified_sig
