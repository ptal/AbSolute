(** Decompositions of the cumulative global constraint. *)

open Bounds
open Lang.Ast

(** `C` is the bound of resources / capacities. *)
module Make(C: Bound_sig.S) :
sig
  (** A task with resource consumption. *)
  type rtask = {
    id: int; (** The ID of a task is useful to create name of new variables involving this task.
                 It should uniquely identify the task. *)
    resources_usage: C.t
  }

  (** From the index of two tasks `t1` and `t2`, get a fresh or existing name of the Boolean variable representing that `t1` is running when `t2` starts. *)
  type name_factory = rtask -> rtask -> string

  (** We have `task_1_runs_when_2_starts = 1` if the task `1` starts when the task `2` is running. *)
  val default_name_factory: name_factory

  (** Task-resource decomposition of cumulative.
      Task not using the resource are automatically discarded.
      `name_factory` helps to share new Boolean variables created by the decomposition across all cumulatives.
      The decomposition is `forall(t1),
        capacity - t1.resource_usage >= sum(t2 where t1 <> t2) (task_2_runs_when_1_starts * t2.resource_usage)` *)
  val cumulative_task_RD: rtask list -> C.t -> name_factory -> formula
end