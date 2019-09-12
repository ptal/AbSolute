open Core
open Bounds
open Lang.Ast

module Make(C: Bound_sig.S) =
struct
  let resource_ty = if C.is_continuous then Real else Int
  type rtask = {
    id: int;
    resources_usage: C.t
  }
  type name_factory = rtask -> rtask -> string

  let default_name_factory t1 t2 =
    "task_" ^ (string_of_int t1.id) ^ "_runs_when_" ^ (string_of_int t2.id) ^ "_starts"

  let remove_tasks_not_using_resource rtasks =
    List.filter (fun t -> C.neq t.resources_usage C.zero) rtasks

  let cumulative_task_RD rtasks capacity make_name =
    let tasks = remove_tasks_not_using_resource rtasks in
    conjunction (List.map (fun t1 ->
      (* Given the task `t1`, we retrieve the quantity of resource used by all other tasks `t2` during the execution of `t1`.
         A Boolean variable is used to discard the resources of tasks not executed at the same time as `t1`. *)
      let resources_profile = List.flatten (List.map (fun t2 ->
        if t1.id = t2.id then []
        else
          let b = Var (make_name t2 t1) in
          let r = Cst (C.to_rat t2.resources_usage, resource_ty) in
          [(Binary (b, MUL, r))]
      ) tasks) in
      let sum =
        if List.length resources_profile = 0 then zero
        else Tools.fold_left_hd (fun a b -> Binary (a, ADD, b)) resources_profile in
      (* We subtract the resource used by `t1` to the capacity since it is already known. *)
      let remaining_capacity = Bound_rat.sub_up (C.to_rat capacity) (C.to_rat t1.resources_usage) in
      Cmp (Cst (remaining_capacity, resource_ty), GEQ, sum)
    ) tasks)
end