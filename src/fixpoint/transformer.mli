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
open Lang.Ast

(** This module is plugged onto a `Solver` in order to provide additional solving information.
    It provides statistics by default.
    By using the notion of "transformers" you can slightly altered the solving process to achieve:
      1. Branch and bound (minimization and maximization).
      2. Stopping the search under some conditions such as time.
      3. Printing some information of your choice.
      4. Etc.

    Rational: You can add your own transformer inside this module.
      It is not as extensible as I first wished (for example through the use of functors).
      One challenge is the management of backtrackable and global states and the dynamic creation of the strategy.
      If you want to try to make it more extensible, see my trial on [combinators](https://github.com/ptal/AbSolute/tree/0332a5b6e66d8138ff86db43294298af6282ec97/src/combinators). *)

(** Global statistics on the search process.
    These are computed by default. *)
type global_statistics = {
  start: Mtime_clock.counter;
  elapsed: Mtime.span;
  nodes: int;
  fails: int;
  sols: int;
  prunes: int;
  depth_max: int;
  restarts: int;
}

(** Backtrackable statistics of the search process.
    These are computed by default. *)
type bt_statistics = {
  depth: int;
}

val init_global_stats: unit -> global_statistics
val init_bt_stats: unit -> bt_statistics

module Make(A: Abstract_domain):
sig
  (** `kind` is the type of BAB (minimization or maximization), see `minimize_bab` and `maximize_bab`.
      We keep the logical and abstract representation of the variable `objective` to optimize.
      `best` contains the best solution so far, or `None` if none was found yet. *)
  type bab = {
    kind: cmpop;
    objective: (A.I.var_id * var);
    best: A.t option;
  }

  (** The arguments are as follows: `print_node status depth domain`.
      `print_node` is called in every node.
      `print_sol` is only called in solution nodes. *)
  type printer = {
    print_node: (string -> int -> A.t -> unit);
    print_sol: (A.t -> unit);
  }

  (** These are the transformers supported.
      They are compositional in the sense that we keep them in a list, and they are applied in turn when reaching a node. *)
  type transformer =
  | Printer of printer
  | BAB of bab
  | Timeout of Mtime.span
  | BoundSolutions of int

  (** See `type bab`. *)
  val make_bab: cmpop -> (A.I.var_id * var) -> transformer
  val minimize_bab: (A.I.var_id * var) -> transformer
  val maximize_bab: (A.I.var_id * var) -> transformer

  (** Global state which stays identical to the whole computation. *)
  type gs = {
    transformers: transformer list;
    stats: global_statistics;
  }

  (** Backtrackable state which is automatically backtracked with the search tree. *)
  type bs = {
    repr: A.I.t;
    domain: A.t;
    bt_stats: bt_statistics
  }

  type t = (gs * bs)

  (** Any of the event functions `on_*` can raise this exception to stop the search. *)
  exception StopSearch of t

  (** This exception prune the current subtree. *)
  exception Backjump of (int * t)

  (** Call `f t` and turn any `Bot_found` and `Conflict n` exception into a statefull `Backjump n t` exception.
      `n` is equal to `0` in case of `Bot_found`.
      Any function that can raise `Bot_found` or `Conflict` must be wrapped in this function.
      Rational: It prevents losing information on `t` obtained so far (such as the node counter). *)
  val wrap_exception: t -> (t -> t) -> t

  val init: A.I.t -> A.t -> transformer list -> t

  (** Apply all the transformers modifying the current state when entering a node (before `closure`). *)
  val on_node: t -> t

  (** Apply all the transformers concerned by a failed node. *)
  val on_fail: t -> t

  (** Apply all the transformers concerned by a solution node. *)
  val on_solution: t -> t

  (** Apply all the transformers concerned by an unknown node. *)
  val on_unknown: t -> t
end
