open Bench_instance_j
open Rcpsp
open Rcpsp_model
open System

module type RCPSP_sig =
sig
  include Abstract_domain.Abstract_domain
  val init_rcpsp: rcpsp_model -> t
end

module type Bencher_sig =
sig
  val start_benchmarking: bench_instance -> unit
end

module RCPSP_Octagon(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  include Box_octagon_disjoint.Make
    (Box_dom.Box_base(Box_split.First_fail_bisect))
    (Octagon.OctagonZ(SPLIT))

  let init_rcpsp rcpsp = init rcpsp.box_vars rcpsp.octagonal_vars rcpsp.constraints rcpsp.reified_bconstraints
end

module RCPSP_Box(SPLIT: Box_split.Box_split_sig) =
struct
  include Box_reified.BoxReifiedZ(SPLIT)

  let init_rcpsp rcpsp =
    let vars = (rcpsp.box_vars)@(rcpsp.octagonal_vars) in
    init vars rcpsp.constraints rcpsp.reified_bconstraints
end

(* This module benches a repository of files with the parametrized abstract domain. *)
module Bencher(Rcpsp_domain: RCPSP_sig) =
struct
  let makespan rcpsp domain = Rcpsp_domain.project_one domain rcpsp.makespan

  (* I. Printing utilities. *)

  let print_variables domain vars =
    let vars = Rcpsp_domain.project domain vars in
    begin
      List.iter (fun (v, (l, u)) ->
        let (l, u) = (Rcpsp_domain.B.to_string l, Rcpsp_domain.B.to_string u) in
        Printf.printf "%s=[%s,%s] \n" v l u;
      ) vars;
    end
  let print_depth depth = List.iter (fun _ -> Printf.printf(".")) (Tools.range 0 (depth-1))
  let no_print _ _ _ _ = ()
  let print_node with_var status rcpsp depth domain =
  begin
    print_depth depth;
    Printf.printf "[%s][%f]" status (Rcpsp_domain.volume domain);
    if with_var then (
      print_variables domain rcpsp.octagonal_vars;
      print_variables domain rcpsp.box_vars);
    Printf.printf "\n";
    flush_all ()
  end

  let no_print_makespan _ _ = ()
  let print_makespan with_var rcpsp domain =
    let (lb,ub) = makespan rcpsp domain in
    begin
      Printf.printf "makespan: (%s,%s)\n" (Rcpsp_domain.B.to_string lb) (Rcpsp_domain.B.to_string ub);
      if with_var then
        print_variables domain rcpsp.octagonal_vars
    end

  (* II. Branch and bound support. *)

  let constraint_makespan rcpsp best domain =
    let open Csp in
    match best with
    | None -> domain
    | Some best ->
        let (_,ub) = makespan rcpsp best in
        let ub = Cst (Rcpsp_domain.B.to_rat ub, Int) in
        Rcpsp_domain.weak_incremental_closure domain (Var rcpsp.makespan, LT, ub)

  let solve bench stats print_node print_makespan rcpsp =
  begin
    let time_out = timeout_of_bench bench in
    let rec aux depth (best, stats) domain = begin
      (* Stop when we exceed the timeout. *)
      let open State in
      let elapsed = Mtime_clock.count stats.start in
      if (Mtime.Span.compare time_out elapsed) <= 0 then (best,stats) else
      try
        let stats = {stats with nodes=(stats.nodes+1)} in
        let domain = constraint_makespan rcpsp best domain in
        let domain = Rcpsp_domain.closure domain in
        match Rcpsp_domain.state_decomposition domain with
        | False ->
            print_node "false'" rcpsp depth domain;
            (best, {stats with fails=(stats.fails+1)})
        | True when (Rcpsp_domain.volume domain) = 1. ->
            print_makespan rcpsp domain;
            print_node "true" rcpsp depth domain;
            (Some domain, {stats with sols=(stats.sols+1)})
        | x ->
            let status = match x with True -> "almost true" | Unknown -> "unknown" | _ -> failwith "unreachable" in
            print_node status rcpsp depth domain;
            let branches = Rcpsp_domain.split domain in
            List.fold_left (aux (depth+1)) (best,stats) branches
      with Bot.Bot_found ->
      begin
        print_node "false" rcpsp depth domain;
        (best, {stats with fails=(stats.fails+1)})
      end
    end in
    let domain = Rcpsp_domain.init_rcpsp rcpsp in
    aux 0 (None,stats) domain
  end

  (* III. Bench and processing of the results. *)

  let update_with_optimum rcpsp best measure =
    match best with
    | Some best ->
        let (lb, _) = makespan rcpsp best in
        let open Measurement in
        { measure with optimum = Some (Rcpsp_domain.B.to_rat lb) }
    | None -> measure

  let bench_problem bench problem_path =
    try
      let rcpsp = Rcpsp_model.create_rcpsp (read_rcpsp problem_path) in
      let stats = State.init_global_stats () in
      let (best, stats) = solve bench stats no_print no_print_makespan rcpsp in
      let stats = {stats with elapsed=Mtime_clock.count stats.start} in
      let measure = Measurement.init stats problem_path in
      let measure = Measurement.update_time bench stats measure in
      let measure = update_with_optimum rcpsp best measure in
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
| s -> eprintf_and_exit ("The AbSolute strategy `" ^ s ^ "` is unknown for Octagon. Please look into `Factory.make_octagon_strategy` for a list of the supported strategies.")

let make_box_strategy : string -> (module Box_split.Box_split_sig) = function
| "First_fail_LB" -> (module Box_split.First_fail_LB)
| s -> eprintf_and_exit ("The AbSolute strategy `" ^ s ^ "` is unknown for Box. Please look into `Factory.make_box_strategy` for a list of the supported strategies.")

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
