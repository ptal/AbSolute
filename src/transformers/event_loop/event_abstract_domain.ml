(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Domains.Abstract_domain

type task = ad_uid * int
type event = ad_uid * int

module type Event_abstract_domain =
sig
  include Abstract_domain

  (** [exec_task t task] executes the `task` inside the abstract domain `t`.
      Precondition:
        - The UID of the task corresponds to the one of the abstract element.
        - The task ID was obtained through `drain_tasks`. *)
  val exec_task: t -> task -> (t * bool)

  (** [drain_events t] returns the events produced since the last call.
      The events are removed from `t`, so an immediate next call will return an empty list. *)
  val drain_events: t -> (t * event list)

  (** [drain_tasks t] returns the list of tasks of `t` that need to be register in `Event_loop` for scheduling.
      Each task is executed whenever one of the events in the list occurs. *)
  val drain_tasks: t -> (t * (task * event list) list)
end
