(* open Bench_desc_t

type measure = {
  problem_path: string;
  (* In this context `int64` represents the number of nano-seconds. *)
  (* An empty option represents a timeout. *)
  samples: int64 option;
  stats: State.global_statistics;
  optimum: Bound_rat.t option
}

let init stats problem_path =
  { problem_path=problem_path;
    samples=None;
    stats=stats;
    optimum=None }

let update_time config stats measure =
  let time_out = System.timeout_of_config config in
  let open State in
  if Mtime.Span.compare time_out stats.elapsed <= 0 then measure
  else { measure with samples=(Some (Mtime.Span.to_uint64_ns stats.elapsed)) }

let csv_line items = String.concat ", " items

let string_of_time_unit = function
  | `NSec -> "ns"
  | `MSec -> "ms"
  | `Sec -> "s"

let csv_field_name config = function
  | `ProblemPath -> "path"
  | `ProblemName -> "problem"
  | `Time(u) -> "time(" ^ (string_of_time_unit u)
      ^ "; " ^ (string_of_center config.center_of_trials) ^ " of " ^ (string_of_int config.trials) ^ " trials"
      ^ "; timeout=" ^ (string_of_int config.timeout) ^ "s)"
  | `Solutions -> "solutions"
  | `Fails -> "fails"
  | `Nodes -> "nodes"
  | `Optimum -> "optimum"

let csv_header config =
  let names = List.map (csv_field_name config) config.csv.fields in
  csv_line names

let csv_time_field config measure u =
  if measure.time = None then
    "timeout"
  else
    let time = match u with
      | `NSec -> Mtime.Span.to_ns measure.time
      | `MSec -> Mtime.Span.to_ms measure.time
      | `Sec -> Mtime.Span.to_s measure.time in
    Printf.sprintf "%.2f%s" time (string_of_time_unit u)

let csv_field_value (config : benchmark) measure = function
  | `ProblemPath -> measure.problem_path
  | `ProblemName -> Filename.basename measure.problem_path
  | `Time(u) -> csv_time_field config measure u
  | `Solutions -> (string_of_int measure.stats.sols)
  | `Fails -> (string_of_int measure.stats.fails)
  | `Nodes -> (string_of_int measure.stats.nodes)
  | `Optimum -> match measure.optimum, measure.time with
      | Some(o), _ -> Bound_rat.to_string o
      | None, Some _ -> "unsat"
      | None, None -> "none"

let bench_to_csv config measure =
  let values = List.map (csv_field_value config measure) config.csv.fields in
  csv_line values

let print_csv_line line =
  Printf.printf "%s\n" line;
  flush_all ()

let print_csv_header config = print_csv_line (csv_header config)
let print_as_csv config measure = print_csv_line (bench_to_csv config measure)

let print_exception problem_path msg = print_csv_line (Format.sprintf "%s: %s" problem_path msg)

type global_info = {
  total: int;
  solved: int;
  found_bound: int;
  proven_unsat: int;
  total_solving_time: int64;
}

let empty_info = { total=0; solved=0; found_bound=0; proven_unsat=0; total_solving_time=Int64.zero }

let add_measure info measure =
  let has_bound = match measure.optimum with Some _ -> true | None -> false in
  let finished_in_time = (List.length measure.samples) > 0 in
  { total=info.total+1;
    solved=info.solved + (if finished_in_time then 1 else 0);
    found_bound=info.found_bound + (if has_bound then 1 else 0);
    proven_unsat=info.proven_unsat + (if finished_in_time && (not has_bound) then 1 else 0);
    total_solving_time=Int64.add info.total_solving_time (if finished_in_time then measure.average else Int64.zero)
  }

let add_erroneous_measure info = { info with total=info.total +1 }

let print_bench_results name info =
begin
  let time = (Mtime.Span.to_s (Mtime.Span.of_uint64_ns info.total_solving_time)) in
  Printf.printf "%d / %d problems solved within the timeout.\n" info.solved info.total;
  Printf.printf "%d / %d problems bounded within the timeout.\n" info.found_bound (info.total - info.proven_unsat);
  Printf.printf "%d problems proven unsatisfiable within the timeout.\n" info.proven_unsat;
  Printf.printf "Cumulative running time: %.2fs.\n" time;
  Printf.printf "(%s, %d, %d, %d, %.2fs)\n\n" name info.solved info.found_bound info.proven_unsat time;
end

let print_solver solver_config model strategy =
  Printf.printf "%s,%s,%s,%s\n" solver_config.name solver_config.version model strategy
 *)