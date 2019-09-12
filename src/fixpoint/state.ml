
type global_statistics = {
  start: Mtime_clock.counter;
  elapsed: Mtime.span;
  nodes: int;
  fails: int;
  sols: int;
  pruned: int;
  depth_max: int;
  restarts: int;
}

let init_global_stats () = {
  start = Mtime_clock.counter ();
  elapsed = Mtime.Span.zero;
  nodes=0;
  fails=0;
  sols=0;
  pruned=0;
  depth_max=0;
  restarts=0;
}

type precision = float
type timeout_bound = Mtime.span
type max_solutions_bound = int

(* Global state proper to each strategy. *)
type 'a global = {
  precision: precision option;
  statistics: global_statistics option;
  timeout: timeout_bound option;
  max_solutions: max_solutions_bound option
}

type 'a state = 'a global

exception Field_not_found of string

let precision global =
  match global.precision with
  | Some(p) -> p
  | None -> raise (Field_not_found "precision")

let statistics s =
  match s.statistics with
  | Some(s) -> s
  | None -> raise (Field_not_found "statistics")

let timeout global =
  match global.timeout with
  | Some(t) -> t
  | None -> raise (Field_not_found "timeout")

let max_solutions global =
  match global.max_solutions with
  | Some(s) -> s
  | None -> raise (Field_not_found "max_solutions")

type 'a branch_kind =
  | Satisfiable of 'a
  | Fail of 'a
  | Prune of 'a
  | Unknown of 'a
  | Stop of 'a

type 'a branches = 'a global * 'a branch_kind list

let satisfiable abs = Satisfiable abs
let unknown abs = Unknown abs
