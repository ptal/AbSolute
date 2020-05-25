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

module type Schedulable_abstract_domain =
sig
  include Abstract_domain

  (** [exec_task t task] executes the `task` inside the abstract domain `t`.
      It maps to `true` if the task is entailed, `false` otherwise.
      Precondition:
        - The UID of the task corresponds to the one of the abstract element.
        - The task ID was obtained through `drain_tasks`. *)
  val exec_task: t -> task -> (t * bool)

  (** [drain_tasks t] returns the list of tasks of `t` that need to be register in `Event_loop` for scheduling.
      Each task is executed whenever one of the events in the list occurs. *)
  val drain_tasks: t -> (t * (task * event list) list)

  (** [remove t c] discards a constraint `c` previously added through `weak_incremental_closure`.
      Next time `exec_task` is called, the constraint will be considered entailed.
      Rational: It is necessary to move a constraint from a domain to another such as in `Delayed_product`. *)
  val remove: t -> I.rconstraint -> t
end
