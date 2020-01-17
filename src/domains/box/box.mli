(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

module Box_split = Box_split
module Box_interpretation = Box_interpretation
module Hc4 = Hc4
module Var_store = Var_store

open Bounds
open Vardom
open Event_loop.Schedulable_abstract_domain

(** Box are an event-based abstract domain and must be encapsulated in an `Event_loop`.
    The `closure` operator is the identity function since it is decomposed in many tasks
    handled by `Event_loop`. *)
module type Box_sig =
sig
  module Vardom: Vardom_sig.Vardom_sig
  type vardom = Vardom.t

  include Schedulable_abstract_domain

  (** `project_vardom box v` projects the interval of the variable `v`. *)
  val project_vardom: t -> I.var_id -> vardom
end

module type Box_functor = functor (B: Bound_sig.S) -> Box_sig with module Vardom.B = B

module Make
  (B: Bound_sig.S)
  (VARDOM: Vardom_sig.Vardom_functor)
  (SPLIT: Box_split.Box_split_sig) : Box_sig

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor
