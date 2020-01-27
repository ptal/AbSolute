(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

module PC_interpretation = Pc_interpretation
module Hc4 = Hc4

open Bounds
open Vardom
open Domains.Abstract_domain
open Event_loop.Schedulable_abstract_domain
open Pc_interpretation

(** Propagator_completion is an event-based abstract domain and must be encapsulated in an `Event_loop`.
    The `closure` operator is the identity function since it is decomposed in many tasks
    handled by `Event_loop`. *)
module type Propagator_completion_sig =
sig
  module B: Bound_sig.S
  module V: Vardom_sig.S
  module I: PC_interpretation_sig
  type vardom = V.t
  include Schedulable_abstract_domain with
    module B := B and module I := I
  val init: I.t -> t
end

module Propagator_completion
  (V: Vardom_sig.S)
  (A: Abstract_domain) : Propagator_completion_sig
with module V = V and module I.A = A