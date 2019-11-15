(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Decompositions of the cumulative global constraint. *)

open Bounds
open Lang.Ast

module type Cumulative_decomposition =
sig
  include Scheduling.S
  module C: Bound_sig.S

  (** A task with resource consumption. *)
  type rtask = {
    task: task;
    id: int;(** The ID of a task is useful to create name of new variables involving this task.
                 It should uniquely identify the task. *)
    resources_usage: C.t
  }
  type name_factory
  val default_name_factory: name_factory
  val shared_constraints: rtask list -> int -> name_factory -> qformula
  val cumulative: rtask list -> int -> C.t -> name_factory -> formula
end

module Generic(T: Bound_sig.S)(C: Bound_sig.S) :
sig
  module S : (module type of Scheduling.Make(T))
  module C : Bound_sig.S
  type rtask = {
    task: S.task;
    id: int;
    resources_usage: C.t
  }
end with module C=C

(** `T` is the bound of task and `C` is the bound of resources / capacities. *)
module MakeTaskRD(T: Bound_sig.S)(C: Bound_sig.S) :
sig
  include module type of Generic(T)(C)

  (** Given two tasks `t1` and `t2`, get a fresh or existing name of the Boolean variable representing that `t1` is running when `t2` starts. *)
  type name_factory = rtask -> rtask -> string

  (** We have `task_1_runs_when_2_starts = 1` if the task `1` starts when the task `2` is running. *)
  val default_name_factory: name_factory

  (** Create constraints that are shared among the task-RD cumulative decomposition.
      `MakeTaskRD.cumulative` suppose these constraints are available.
      Any `resources_usage` in tasks can be used since it is not used to generate these constraints.
      `forall(t1 <> t2) task_<t2.id>_runs_when_<t1.id>_starts <=> overlap_before(t1,t2). *)
  val shared_constraints: rtask list -> int -> name_factory -> qformula

  (** Task-resource decomposition of cumulative.
      You must also retrieve the constraints of `shared_constraints`.
      Tasks not using any resource are automatically discarded.
      `name_factory` helps to share new Boolean variables created by the decomposition across all cumulatives.
      The decomposition is `forall(t1),
        capacity - t1.resource_usage >= sum(t2 where t1 <> t2) (task_<t2.id>_runs_when_<t1.id>_starts * t2.resource_usage)` *)
  val cumulative: rtask list -> int -> C.t -> name_factory -> formula
end

(** Time-resource decomposition of cumulative.
    For this decomposition, task variables are required to be discrete as we need to discretize over time.
    `C` is the bound of resources / capacities. *)
module MakeTimeRD(C: Bound_sig.S) :
sig
  include module type of Generic(Bound_int)(C)

  (** Given a task `t` and a time instant `i`, get a fresh or existing name of the Boolean variable representing that `t` is running at the instant `i`. *)
  type name_factory = rtask -> int -> string

  (** We have `task_1_runs_at_2 = 1` if the task `1` runs at the instant `2`. *)
  val default_name_factory: name_factory

  (** Similar to `MakeTaskRD.shared_constraints`.
      Here the decomposition is:
        `forall(i,t) task_<t.id>_runs_at_<i> <=> at_instant(t.task, i). *)
  val shared_constraints: rtask list -> int -> name_factory -> qformula

  (** Time-resource decomposition of cumulative.
      Similar to `MakeTaskRD.cumulative`.
      `horizon` is an upper bound on the latest possible time.
      The decomposition is `forall(i in 0..horizon)
        capacity >= sum(t) task_<t.id>_runs_at_<i> * t.resource_usage
  *)
  val cumulative: rtask list -> int -> C.t -> name_factory -> formula
end
