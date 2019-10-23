(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Bounds
open Lang.Ast
open Octagon
open Box
open Box.Box_dom
open Domains.Abstract_domain

module type Box_oct_rep_sig =
sig
  module Box_rep: Box_representation.Box_rep_sig
  module Oct_rep: Octagon_representation.Octagon_rep_sig
  type t = {
    box_rep: Box_rep.t;
    oct_rep: Oct_rep.t;
  }
  type var_kind = BoxKind of Box_rep.var_kind | OctKind of Oct_rep.var_kind
  type var_id = BoxVar of Box_rep.var_id | OctVar of Oct_rep.var_id
  type reified_octagonal = Box_rep.var_id * Oct_rep.rconstraint list
  type rconstraint =
    BoxConstraint of Box_rep.rconstraint
  | OctConstraint of Oct_rep.rconstraint
  | ReifiedConstraint of reified_octagonal

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

module type Box_oct_rep_functor =
  functor(Box_rep: Box_representation.Box_rep_sig)(Oct_rep: Octagon_representation.Octagon_rep_sig) -> Box_oct_rep_sig
  with module Box_rep=Box_rep and module Oct_rep=Oct_rep

module Box_oct_rep: Box_oct_rep_functor

module type Box_octagon_disjoint_sig =
sig
  module B : Bound_sig.S
  module R : Box_oct_rep_sig
  type t
  type bound = B.t
  val empty: ad_uid -> t
  val uid: t -> ad_uid
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (B.t * B.t)
  val lazy_copy: t -> int -> t list
  val copy: t -> t

  (** This closure filters the box and octagon with regards to the (reified) constraints in `box_oct`.
      Besides reducing the domain of the variables, the entailed constraints are removed from `box_oct`. *)
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> Kleene.t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> Kleene.t
  val print: R.t -> Format.formatter -> t -> unit
end

module Make
  (BOX: Box_functor)
  (Octagon: Octagon_sig) : Box_octagon_disjoint_sig
