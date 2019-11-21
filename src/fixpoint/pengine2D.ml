(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

type uid = int
type task = uid * int
type event = uid * int

module Store2D = Map.Make(
  struct
    type t = uid * int
    let compare (x0,y0) (x1,y1) =
      match compare x0 x1 with
      | 0 -> compare y0 y1
      | c -> c
  end)

type t = {
  (* Records the active tasks (those not yet entailed).
     This field is backtracked automatically. *)
  actives: unit Store2D.t;
  num_actives: int;

  (* `reactor.(e)` contains the set of tasks that reacts on the event `e`. *)
  reactor: (task list) Store2D.t;

  (* Contains all the tasks that must be propagated in order to reach a fixpoint.
     We also have the companion structure `inside_queue.(i).(j)` that is true if the task `(i,j)` is inside the queue.
     This field is local to a node, so it is not backtracked nor copied. *)
  scheduler: task CCDeque.t;
  inside_queue: CCBV.t CCVector.vector;
}

let empty () = {
  actives = Store2D.empty;
  num_actives = 0;
  reactor = Store2D.empty;
  scheduler=CCDeque.create ();
  inside_queue=CCVector.create ();
}

let subscribe pengine task events =
  let concat_task = function
      None -> Some [task]
    | Some tasks -> Some (task::tasks) in
  let reactor = List.fold_left
    (fun reactor ev -> Store2D.update ev concat_task reactor)
    pengine.reactor events in
  let actives = Store2D.add task () pengine.actives in
  let num_actives = pengine.num_actives + 1 in
  { pengine with reactor; actives; num_actives }

let get_inside_queue pengine i =
  let size = CCVector.size pengine.inside_queue in
  if i >= size then
    CCVector.append_list pengine.inside_queue
      (List.init (i-size+1) (fun _ -> CCBV.empty ()));
  CCVector.get pengine.inside_queue i

(* Schedule a task if it is active and not already present in the scheduler. *)
let schedule pengine task =
  let inside = get_inside_queue pengine (fst task) in
  if Store2D.mem task pengine.actives &&
     not (CCBV.get inside (snd task)) then
  begin
    CCDeque.push_back pengine.scheduler task;
    CCBV.set inside (snd task)
  end

(* Retrieve the next task to execute. *)
let pop pengine =
  let task = CCDeque.take_front pengine.scheduler in
  CCBV.reset (CCVector.get pengine.inside_queue (fst task)) (snd task);
  task

let react pengine events =
  let react_on_event pengine ev =
    List.iter (schedule pengine) (Store2D.find ev pengine.reactor) in
  List.iter (react_on_event pengine) events

let deactivate_task pengine task =
  let actives = Store2D.remove task pengine.actives in
  let num_actives = pengine.num_actives-1 in
  { pengine with actives; num_actives }

let num_active_tasks pengine = pengine.num_actives

(* An invariant is that the scheduler is empty at the end of this function, thus it is idempotent. *)
let fixpoint pengine f acc =
  let rec aux pengine acc has_changed =
    if CCDeque.is_empty pengine.scheduler then
      pengine, acc, has_changed
    else
    begin
      let task = pop pengine in
      let acc, task_entailed, events = f acc task in
      let pengine =
        if task_entailed then
          deactivate_task pengine task
        else pengine in
      react pengine events;
      aux pengine acc (has_changed || (List.length events > 0))
    end
  in
  try
    aux pengine acc false
  with e -> begin
    CCDeque.clear pengine.scheduler;
    CCVector.iter (fun x -> CCBV.clear x) pengine.inside_queue;
    raise e
  end
