(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

module Event_abstract_domain = Event_abstract_domain

open Domains.Abstract_domain
open Fixpoint
open Event_abstract_domain

(** This module type is useful to parametrize `Event_loop` by a list of modules. *)
module type Event_combinator =
sig
  type t
  val consume_task: t -> task -> (t * bool * event list)
  val produce_events: t -> Pengine2D.t -> (t * Pengine2D.t)
  val produce_tasks: t -> Pengine2D.t -> (t * Pengine2D.t)
end

module Event_atom(A: Event_abstract_domain) :
sig
  type t = A.t ref
  include Event_combinator with type t := t
end

module Event_cons(A: Event_abstract_domain)(B: Event_combinator) :
sig
  type t = (A.t ref * B.t)
  include Event_combinator with type t := t
end

(** `Event_loop` is a product of `Event_abstract_domain`.
    The main role of `Event_loop` is to schedule the tasks given by these domains. *)
module Event_loop(L: Event_combinator) :
sig
  include Abstract_domain
  val init: ad_uid -> L.t -> t
end
