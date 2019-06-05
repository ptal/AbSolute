open Bench_instance_j
open Rcpsp
open Rcpsp_model
open System

module type RCPSP_sig =
sig
  include Abstract_domain.Abstract_domain
  val init_rcpsp: rcpsp_model -> R.t * t
end

module type Bencher_sig =
sig
  val start_benchmarking: bench_instance -> unit
end

module RCPSP_Box(SPLIT: Box_split.Box_split_sig) : RCPSP_sig =
struct
  include Box_reified.BoxReifiedZ(SPLIT)

  let init_rcpsp rcpsp =
    let vars = (rcpsp.box_vars)@(rcpsp.octagonal_vars) in
    let extend (dom,repr) v =
      let (dom,idx) = (extend dom ()) in
      (dom, R.extend repr (v, idx)) in
    let domain, repr = List.fold_left extend (empty,R.empty) vars in
    let constraints = List.flatten (List.map (R.rewrite repr) rcpsp.constraints) in
    let reified_cons = List.flatten (List.map (fun (b,c) -> R.rewrite_reified repr b c) rcpsp.reified_bconstraints) in
    repr, List.fold_left weak_incremental_closure domain (constraints@reified_cons)
end

module RCPSP_Octagon(SPLIT: Octagon_split.Octagon_split_sig) : RCPSP_sig =
struct
  include Box_octagon_disjoint.Make
    (Box_dom.Box_base(Box_split.First_fail_bisect))
    (Octagon.OctagonZ(SPLIT))

  let init_rcpsp rcpsp =
    let extend kind (dom,repr) v =
      let (dom,idx) = (extend dom kind) in
      (dom, R.extend repr (v, idx)) in
    let domain, repr = List.fold_left (extend (R.OctKind ())) (empty,R.empty) rcpsp.octagonal_vars in
    let domain, repr = List.fold_left (extend (R.BoxKind ())) (domain, repr) rcpsp.box_vars in
    let constraints = List.flatten (List.map (R.rewrite repr) rcpsp.constraints) in
    let reified_cons = List.flatten (List.map (fun (b,c) -> R.rewrite_reified repr b c) rcpsp.reified_bconstraints) in
    repr, List.fold_left weak_incremental_closure domain (constraints@reified_cons)
end

(* This module benches a repository of files with the parametrized abstract domain. *)
module Bencher(Rcpsp_domain: RCPSP_sig) =
struct
  module R = Rcpsp_domain.R
  module BAB = Bab.Make(Rcpsp_domain)

  let makespan_id repr rcpsp = R.to_abstract_var repr rcpsp.makespan
  let makespan repr rcpsp domain = Rcpsp_domain.project domain (makespan_id repr rcpsp)

  (* I. Printing utilities. *)

  let print_variables repr domain vars =
    let values = List.map
      (Rcpsp_domain.project domain)
      (List.map (R.to_abstract_var repr) vars) in
    let vars = List.combine vars values in
    begin
      List.iter (fun (v, (l, u)) ->
        let (l, u) = (Rcpsp_domain.B.to_string l, Rcpsp_domain.B.to_string u) in
        Printf.printf "%s=[%s,%s] \n" v l u;
      ) vars;
    end
  let print_depth depth = List.iter (fun _ -> Printf.printf(".")) (Tools.range 0 (depth-1))
  let no_print _ _ _ _ = ()
  let print_node repr with_var status rcpsp depth domain =
  begin
    print_depth depth;
    Printf.printf "[%s][%f]" status (Rcpsp_domain.volume domain);
    if with_var then (
      print_variables repr domain rcpsp.octagonal_vars;
      print_variables repr domain rcpsp.box_vars);
    Printf.printf "\n";
    flush_all ()
  end

  let no_print_makespan _ _ = ()
  let print_makespan repr with_var rcpsp domain =
    let (lb,ub) = makespan repr rcpsp domain in
    begin
      Printf.printf "makespan: (%s,%s)\n" (Rcpsp_domain.B.to_string lb) (Rcpsp_domain.B.to_string ub);
      if with_var then
        print_variables repr domain rcpsp.octagonal_vars
    end

  (* II. Bench and processing of the results. *)

  let update_with_optimum repr rcpsp best measure =
    match best with
    | Some best ->
        let (lb, _) = makespan repr rcpsp best in
        let open Measurement in
        { measure with optimum = Some (Rcpsp_domain.B.to_rat lb) }
    | None -> measure

  let bench_problem bench problem_path =
    try
      let rcpsp = Rcpsp_model.create_rcpsp (read_rcpsp problem_path) in
      let timeout = timeout_of_bench bench in
      let repr, domain = Rcpsp_domain.init_rcpsp rcpsp in
      let solver = BAB.init repr domain in
      let (best, stats) = BAB.minimize solver timeout (makespan_id repr rcpsp) in
      let stats = {stats with elapsed=Mtime_clock.count stats.start} in
      let measure = Measurement.init stats problem_path in
      let measure = Measurement.update_time bench stats measure in
      let measure = update_with_optimum repr rcpsp best measure in
      Measurement.print_as_csv bench measure
    with e -> begin
      (* Printexc.print_backtrace stdout; *)
      Measurement.print_exception problem_path (Printexc.to_string e)
    end

  let start_benchmarking bench =
  begin
    Measurement.print_csv_header bench;
    let problems = list_of_problems bench in
    List.iter (bench_problem bench) problems
  end
end

let make_octagon_strategy : string -> (module Octagon_split.Octagon_split_sig) = function
| "MSLF" -> (module Octagon_split.MSLF)
| "MSLF_all" -> (module Octagon_split.MSLF_all)
| "MSLF_simple" -> (module Octagon_split.MSLF_simple)
| "Max_min_LB" -> (module Octagon_split.Min_max_LB)
| "Min_max_LB" -> (module Octagon_split.Max_min_LB)
| s -> eprintf_and_exit ("The AbSolute strategy `" ^ s ^ "` is unknown for Octagon. Please look into `make_octagon_strategy` for a list of the supported strategies.")

let make_box_strategy : string -> (module Box_split.Box_split_sig) = function
| "First_fail_LB"  -> (module Box_split.First_fail_LB)
| "MSLF_simple" -> (module Box_split.MSLF_simple)
| s -> eprintf_and_exit ("The AbSolute strategy `" ^ s ^ "` is unknown for Box. Please look into `make_box_strategy` for a list of the supported strategies.")

let bench_absolute bench solver =
  match solver.domain with
  | "Octagon" ->
      let (module S: Octagon_split.Octagon_split_sig) = make_octagon_strategy solver.strategy in
      let (module M: RCPSP_sig) = (module RCPSP_Octagon(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.start_benchmarking bench
  | "Box" ->
      let (module S: Box_split.Box_split_sig) = make_box_strategy solver.strategy in
      let (module M: RCPSP_sig) = (module RCPSP_Box(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.start_benchmarking bench
  | d -> eprintf_and_exit ("The AbSolute domain `" ^ d ^ "` is unknown. Please look into `Absolute_bench.bench_absolute` for a list of the supported domains.")

let run_bench bench =
  match bench.solver_instance with
  | `AbSoluteKind(instance) -> bench_absolute bench instance
  | `MznKind(instance) -> Minizinc.bench_minizinc bench instance
  | `DecomposedKind(instance) -> Minizinc.bench_decomposed_mzn bench instance

let bench_from_json json_data =
  try
    bench_instance_of_string json_data
  with
  | Atdgen_runtime__Oj_run.Error(msg)
  | Yojson.Json_error(msg) ->
      eprintf_and_exit (Printf.sprintf
        "The benchmarks description file contains an error:\n\n\
         %s\n\n\
        [help] Be careful to the case: \"int\" is not the same as \"Int\".\n\
        [help] You can find a full example of the JSON format in benchmark/data/benchmarks.json." msg)

let () =
  (* Printexc.record_backtrace true; *)
  let bench = bench_from_json (get_bench_desc ()) in
  run_bench bench
  (* Printf.printf "%s" (Yojson.Safe.prettify (string_of_bench_instance bench)) *)
