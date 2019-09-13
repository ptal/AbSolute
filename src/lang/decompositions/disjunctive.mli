open Bounds
open Lang.Ast

module Make(B: Bound_sig.S):
sig
  type task = {
    start: string;
    duration: B.t;
  }

  (** Ensure two tasks do not overlap.
      The resulting decomposition is `s1 - s2 <= -d1 \/ s2 - s1 <= -d2`. *)
  val non_overlap: task -> task -> formula

  (** Ensure that `t1` overlaps with `t2` and that `t1` starts before `t2`.
      The resulting decomposition is `s1 <= s2 /\ s2 - s1 < d1 *)
  val overlap_before: task -> task -> formula

  (** Ensure that `tasks` are never scheduled at the same time (they do not overlap). *)
  val disjunctive: task list -> formula

  (** Ensure that a task is running at the instant `i`. *)
  val at_instant: task -> B.t -> formula
end
