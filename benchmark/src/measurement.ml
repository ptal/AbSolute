open Bench_instance_t
open State

type measure = {
  problem_path: string;
  (* In this context `int64` represents the number of nano-seconds. *)
  (* An empty option represents a timeout. *)
  time: int64 option;
  stats: State.global_statistics;
  optimum: Bound_rat.t option
}

let init stats problem_path =
  { problem_path=problem_path;
    time=None;
    stats=stats;
    optimum=None }

let update_time bench stats measure =
  let time_out = System.timeout_of_bench bench in
  let open State in
  if Mtime.Span.compare time_out stats.elapsed <= 0 then measure
  else { measure with time=(Some (Mtime.Span.to_uint64_ns stats.elapsed)) }

let csv_line items = String.concat ", " items

let string_of_time_unit = function
  | `NSec -> "ns"
  | `MSec -> "ms"
  | `Sec -> "s"

let csv_field_name = function
  | `ProblemPath -> "path"
  | `ProblemName -> "problem"
  | `Time(u) -> "time"
  | `Solutions -> "solutions"
  | `Fails -> "fails"
  | `Nodes -> "nodes"
  | `Optimum -> "optimum"

let csv_header bench =
  let names = List.map csv_field_name bench.csv.Bench_desc_t.fields in
  csv_line names

let print_csv_line line =
  Printf.printf "%s\n" line;
  flush_all ()

let print_csv_header bench = print_csv_line (csv_header bench)

let csv_time_field measure u =
  match measure.time with
  | None -> "timeout"
  | Some(time) ->
    let time = Mtime.Span.of_uint64_ns time in
    let time = match u with
      | `NSec -> Mtime.Span.to_ns time
      | `MSec -> Mtime.Span.to_ms time
      | `Sec -> Mtime.Span.to_s time in
    Printf.sprintf "%.2f%s" time (string_of_time_unit u)

let csv_field_value measure = function
  | `ProblemPath -> measure.problem_path
  | `ProblemName -> Filename.basename measure.problem_path
  | `Time(u) -> csv_time_field measure u
  | `Solutions -> (string_of_int measure.stats.sols)
  | `Fails -> (string_of_int measure.stats.fails)
  | `Nodes -> (string_of_int measure.stats.nodes)
  | `Optimum -> match measure.optimum, measure.time with
      | Some(o), _ -> Bound_rat.to_string o
      | None, Some _ -> "unsat"
      | None, None -> "none"

let bench_to_csv bench measure =
  let values = List.map (csv_field_value measure) bench.csv.Bench_desc_t.fields in
  csv_line values

let print_as_csv bench measure = print_csv_line (bench_to_csv bench measure)

let print_exception problem_path msg = print_csv_line (Format.sprintf "%s: %s" problem_path msg)
