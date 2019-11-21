(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module is similar to `Pengine` but provides support for events and tasks described by two integers (instead of one).
    This slight difference actually changes many of the internal structures.
  Rational:
    This engine is designed to propagate globally across abstract domains.
    The first integer is the UID of the abstract domain and the second its task ID or event. *)

type t

type uid = int
type task = uid * int
type event = uid * int

val empty: unit -> t

(** Schedule all the tasks connected to at least one event in the provided list.
    If the event does not exist in the engine, nothing happens.
    Rational: We suppose that no task is connected to that event. *)
val react: t -> event list -> unit

(** Connect a task to a set of event.
    The task is waken up by `react` whenever one of the event occurs.
    The task and corresponding events are automatically allocated in the engine. *)
val subscribe: t -> task -> event list -> t

(** The number of currently activated tasks.
    An active task is a task that did not reach its solution space yet, and has more pruning to do. *)
val num_active_tasks: t -> int

(** `fixpoint engine f acc` iterates over the scheduled tasks until no task are scheduled anymore.
    `f acc t` performs the task `t` over `acc` and returns the new `(acc, is_entailed, events)` where
      `acc` is the new accumulator, `is_entailed` is true if the task reached its solutions space, and `events` is a list of events to further react on.
     It returns the new engine and accumulator, as well as a Boolean sets to `true` if at least an event was generated.*)
val fixpoint: t -> ('a -> task -> ('a * bool * event list)) -> 'a -> (t * 'a * bool)

