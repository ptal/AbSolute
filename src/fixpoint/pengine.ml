module type Pengine_sig =
sig
  type t
  type event = int
  type task_id = int

  val empty: unit -> t
  val extend_event: t -> t
  val extend_task: t -> t
  val react: t -> event list -> unit
  val subscribe: t -> task_id -> event list -> t
  val deactivate_tasks: t -> (task_id -> bool) -> t
  val num_active_tasks: t -> int
  val fixpoint: t -> ('a -> task_id -> ('a * event list)) -> 'a -> 'a
end

module Pengine =
struct
  type task_id = int
  type event = int

  type t = {
    (* Records the active constraints (those not yet entailed).
       This field is backtracked automatically. *)
    actives: bool Parray.t;
    num_actives: int;

    (* `reactor.(v)` contains the set of constraints that contains the variable `v`. *)
    reactor: (task_id list) Parray.t;

    (* Contains all the constraint that must be propagated in order to reach a fix point.
       We also have the companion `inside_queue.(i)` that is true if the constraint `i` is inside the queue.
       This field is local to a node, so it is not backtracked nor copied. *)
    scheduler: task_id CCDeque.t;
    inside_queue: CCBV.t;
  }

  let empty_parray () = Parray.init 0 (fun _ -> failwith "unreachable")

  let empty () = {
    actives = empty_parray ();
    num_actives = 0;
    reactor = empty_parray ();
    scheduler=CCDeque.create ();
    inside_queue=CCBV.empty ();
  }

  let extend_parray pa a =
    let n = Parray.length pa in
    Parray.init (n+1) (fun i -> if i < n then Parray.get pa i else a)

  let extend_event engine =
    let reactor = extend_parray engine.reactor [] in
    { engine with reactor }

  let extend_task engine =
    let actives = extend_parray engine.actives true in
    let num_actives = engine.num_actives + 1 in
    { engine with actives; num_actives }

  (* Schedule a task if it is active and not already present in the reactor. *)
  let schedule engine task =
    if Parray.get engine.actives task &&
       not (CCBV.get engine.inside_queue task) then
    begin
      CCDeque.push_back engine.scheduler task;
      CCBV.set engine.inside_queue task
    end

  (* Retrieve the next task to execute. *)
  let pop engine =
    let task = CCDeque.take_front engine.scheduler in
    CCBV.reset engine.inside_queue task;
    task

  let react engine events =
    let react_on_event engine ev = List.iter (schedule engine) (Parray.get engine.reactor ev) in
    List.iter (react_on_event engine) events

  let fold_active_tasks engine f acc =
    let i = ref (-1) in
    Parray.fold_left (fun acc b ->
      i := !i + 1;
      if b then f acc !i else acc)
      acc engine.actives

  let deactivate_tasks engine f =
    let actives, num_actives = fold_active_tasks engine (fun (actives,n) i ->
      if f i then (Parray.set actives i false, n)
      else (actives, (n+1))) (engine.actives, 0) in
    { engine with actives; num_actives }

  let num_active_tasks engine = engine.num_actives

  (* A invariant is that the scheduler is empty at the end of this function, thus it is idempotent. *)
  let fixpoint engine f acc =
    let rec aux engine acc =
      if CCDeque.is_empty engine.scheduler then
        acc
      else
      begin
        let task = pop engine in
        let acc, events = f acc task in
        react engine events;
        aux engine acc
      end
    in
    try
      aux engine acc
    with e -> begin
      CCDeque.clear engine.scheduler;
      CCBV.clear engine.inside_queue;
      raise e
    end

  let subscribe engine task events =
    let subscribe_to_event reactor ev = Parray.set reactor ev (task::(Parray.get reactor ev)) in
    let reactor = List.fold_left subscribe_to_event engine.reactor events in
    { engine with reactor }
end