(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

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

  (** Given two tasks name `s1,s2`, and a duration `d`, ensure that `s2` starts at least `d` units after the start of `s1`.
      The resulting constraint is `s1 + d1 <= s2` that we rewrite to `s1 - s2 <= -d1`. *)
  val precedence: string -> string -> B.t -> formula
end