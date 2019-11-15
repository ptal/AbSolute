(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module implements a propagation engine.
    For efficiency reasons, most of its internal structure is imperative.
    Nevertheless, it is safe to use this structure in a backtracking algorithm.
    The design of this engine is formally described in "Constraint Propagation – Models, Techniques, Implementation" (Tack, 2009). *)

module type Pengine_sig =
sig
  type t

  (** For efficiency, we force the event and task ID to be integers. *)
  type event = int
  type task_id = int

  val empty: unit -> t

  (** Extend the reactor with a new event. *)
  val extend_event: t -> t

  (** Extend the reactor with a new task. *)
  val extend_task: t -> t

  (** Schedule all the tasks connected to at least one variable in the provided list. *)
  val react: t -> event list -> unit

  (** Connect a task with a set of variables.
      If a variable is modified, the task is waken up. *)
  val subscribe: t -> task_id -> event list -> t

  (** The number of currently activated tasks. *)
  val num_active_tasks: t -> int

  (** `fixpoint engine f acc` iterates over the scheduled tasks until no task are scheduled anymore.
      `f acc t` performs the task `t` over `acc` and returns the new `(acc, t_fixpoint, events)` where
        `acc` is the new accumulator, `t_fixpoint` is true if the task is at fixpoint, and `events` is a list of events to further react on. *)
  val fixpoint: t -> ('a -> task_id -> ('a * bool * event list)) -> 'a -> (t * 'a)

  (** This function consumes the registered delta in the engine.
      See `Var_store.delta`. *)
  val delta: t -> event list
end

module Pengine : Pengine_sig
