(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Bounds
open Lang.Ast
open Lang.Rewritting

module type Cumulative_decomposition =
sig
  include Scheduling.S
  module C: Bound_sig.S
  type rtask = {
    task: task;
    id: int;
    resources_usage: C.t
  }
  type name_factory
  val default_name_factory: name_factory
  val shared_constraints: rtask list -> D.t -> name_factory -> qformula
  val cumulative: rtask list -> D.t -> C.t -> name_factory -> formula
end

module Generic(T: Bound_sig.S)(C: Bound_sig.S) =
struct
  include Scheduling.Make(T)
  module C = C
  type rtask = {
    task: task;
    id: int;
    resources_usage: C.t
  }

  let remove_tasks_not_using_resource rtasks =
    List.filter (fun t -> C.neq t.resources_usage C.zero) rtasks

  let make_sum terms =
    if List.length terms = 0 then zero
    else Tools.fold_left_hd (fun a b -> Binary (a, ADD, b)) terms
end

let quantify_boolean formula vars =
  List.fold_left (fun f v ->
    Exists(v, Types.(Abstract Bool), f)
  ) formula (List.rev vars)

module MakeTaskRD(T: Bound_sig.S)(C: Bound_sig.S) =
struct
  include Generic(T)(C)

  type name_factory = rtask -> rtask -> string

  let default_name_factory t1 t2 =
    "task_" ^ (string_of_int t1.id) ^ "_runs_when_" ^ (string_of_int t2.id) ^ "_starts"

  let shared_constraints rtasks _ make_name =
    let formula = QFFormula (conjunction (
      Tools.for_all_distinct_pairs rtasks (fun t1 t2 ->
        let b = FVar (make_name t1 t2) in
        [Equiv (b, overlap_before t1.task t2.task)]
    ))) in
    let vars = Tools.for_all_distinct_pairs rtasks (fun t1 t2 -> [make_name t1 t2]) in
    quantify_boolean formula vars

  let cumulative rtasks _ capacity make_name =
    let tasks = remove_tasks_not_using_resource rtasks in
    conjunction (List.map (fun t1 ->
      (* Given the task `t1`, we retrieve the quantity of resource used by all other tasks `t2` during the execution of `t1`.
         A Boolean variable is used to discard the resources of tasks not executed at the same time as `t1`. *)
      let resources_profile = List.flatten (List.map (fun t2 ->
        if t1.id = t2.id then []
        else
          let b = Var (make_name t2 t1) in
          let r = Cst (C.to_rat t2.resources_usage, C.concrete_ty) in
          [(Binary (b, MUL, r))]
      ) tasks) in
      let sum = make_sum resources_profile in
      (* We subtract the resource used by `t1` to the capacity since it is already known. *)
      let remaining_capacity = Bound_rat.sub_up (C.to_rat capacity) (C.to_rat t1.resources_usage) in
      Cmp (Cst (remaining_capacity, C.concrete_ty), GEQ, sum)
    ) tasks)
end

module MakeTimeRD(C: Bound_sig.S) =
struct
  include Generic(Bound_int)(C)

  type name_factory = rtask -> int -> string

  let default_name_factory rtask instant =
    "task_" ^ (string_of_int rtask.id) ^ "_runs_at_" ^ (string_of_int instant)

  let shared_constraints rtasks horizon make_name =
    let formula = QFFormula (conjunction (List.flatten (List.map (fun i ->
      List.map (fun t ->
        Equiv (FVar (make_name t i), at_instant t.task i)
      ) rtasks
    ) (Tools.range 0 horizon)))) in
    let vars = List.flatten (List.map (fun i -> List.map (fun t ->
      make_name t i) rtasks) (Tools.range 0 horizon)) in
    quantify_boolean formula vars

  let cumulative rtasks horizon capacity make_name =
    conjunction (List.map (fun instant ->
      let resources_profile = List.map (fun t ->
        let b = Var (make_name t instant) in
        let r = Cst (C.to_rat t.resources_usage, C.concrete_ty) in
        Binary (b, MUL, r)
      ) rtasks in
      let sum = make_sum resources_profile in
      Cmp (Cst (C.to_rat capacity, C.concrete_ty), GEQ, sum)
    ) (Tools.range 0 horizon))
end
