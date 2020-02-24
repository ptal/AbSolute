(* Copyright 2019 AbSolute Team

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
open Interpretation
open Lang
open Typing
open Typing.Ad_type

exception Conflict of int
type task = ad_uid * int
type event = ad_uid * int
type search_strategy =
  | Simple
  | VarView of Ast.vname list
  | Sequence of (ad_uid * search_strategy) list
module type Abstract_domain =
sig
  module I: Interpretation_sig
  type t
  module B: Bound_sig.S
  val empty: ad_uid -> t
  val uid: t -> ad_uid
  val name: string
  val type_of: t -> ad_ty option
  val interpretation: t -> I.t
  val map_interpretation: t -> (I.t -> I.t * 'a) -> t * 'a
  val interpret: t -> approx_kind -> Tast.tqformula -> (t * I.rconstraint list)
  val project: t -> I.var_id -> (B.t * B.t)
  val embed: t -> I.var_id -> (B.t * B.t) -> t
  type snapshot
  val lazy_copy: t -> int -> snapshot list
  val restore: t -> snapshot -> t
  val closure: t -> t
  val weak_incremental_closure: t -> I.rconstraint -> t
  val entailment: t -> I.rconstraint -> t * I.rconstraint * bool
  val split: ?strategy:search_strategy -> t -> snapshot list
  val volume: t -> float
  val state: t -> Kleene.t
  val print: Format.formatter -> t -> unit
  val has_changed: t -> bool
  val drain_events: t -> (t * event list)
  val events_of: t -> I.rconstraint -> event list
  val events_of_var: t -> I.var_id -> event list
end
