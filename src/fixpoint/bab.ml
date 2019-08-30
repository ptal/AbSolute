open Lang
open Core
open Domains.Abstract_domain
open State

module type BAB_sig = functor(Domain: Abstract_domain) ->
sig
  type t

  (** `print_node status depth domain` is called in each node. *)
  val init : ?print_node:(string -> int -> Domain.t -> unit) -> ?print_bound:(Domain.t -> unit) -> Domain.R.t -> Domain.t -> t
  val minimize : t -> Mtime.span -> Domain.R.var_id -> (Domain.t option * global_statistics)
  val maximize : t -> Mtime.span -> Domain.R.var_id -> (Domain.t option * global_statistics)
end

module Make(Domain: Abstract_domain) =
struct
  type t = {
    repr: Domain.R.t;
    domain: Domain.t;
    print_node: string -> int -> Domain.t -> unit;
    print_bound: Domain.t -> unit;
  }

  let init ?print_node:(pn=(fun _ _ _ -> ())) ?print_bound:(pb=(fun _ -> ())) repr domain =
    {repr=repr; domain=domain; print_node=pn; print_bound=pb; }

  let optimize repr op x best domain =
    let open Ast in
    match best with
    | None -> domain
    | Some best ->
        let (_,ub) = Domain.project best x in
        let annot = if Domain.B.is_continuous then Real else Int in
        let ub = Cst (Domain.B.to_rat ub, annot) in
        let x = Domain.R.to_logic_var repr x in
        let optimization_constraint = Domain.R.rewrite repr (Var x, op, ub) in
        List.fold_left Domain.weak_incremental_closure domain optimization_constraint

  let bab op this timeout x =
  begin
    let rec aux depth (best, stats) domain =
      (* Stop when we exceed the timeout. *)
      let elapsed = Mtime_clock.count stats.start in
      if (Mtime.Span.compare timeout elapsed) <= 0 then (best,stats) else
      let stats = {stats with nodes=(stats.nodes+1)} in
      try
        let domain = optimize this.repr op x best domain in
        let domain = Domain.closure domain in
        match Domain.state_decomposition domain with
        | Kleene.False ->
            this.print_node "false'" depth domain;
            (best, {stats with fails=(stats.fails+1)})
        | Kleene.True when (Domain.volume domain) = 1. ->
            this.print_bound domain;
            this.print_node "true" depth domain;
            (Some domain, {stats with sols=(stats.sols+1)})
        | x ->
            let status = match x with Kleene.True -> "almost true" | Kleene.Unknown -> "unknown" | _ -> failwith "unreachable" in
            this.print_node status depth domain;
            let branches = Domain.split domain in
            List.fold_left (aux (depth+1)) (best,stats) branches
      with Bot.Bot_found ->
      begin
        this.print_node "false" depth domain;
        (best, {stats with fails=(stats.fails+1)})
      end
    in
    let stats = init_global_stats () in
    aux 0 (None, stats) this.domain
  end

  let minimize = bab Ast.LT
  let maximize = bab Ast.GT
end
