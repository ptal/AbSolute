(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Domains.Abstract_domain
open Lang.Ast
open Typing.Tast
open Core

type global_statistics = {
  start: Mtime_clock.counter;
  elapsed: Mtime.span;
  nodes: int;
  fails: int;
  sols: int;
  prunes: int;
  depth_max: int;
  restarts: int;
}

let init_global_stats () = {
  start = Mtime_clock.counter ();
  elapsed = Mtime.Span.zero;
  nodes=0;
  fails=0;
  sols=0;
  prunes=0;
  depth_max=0;
  restarts=0;
}

type bt_statistics = {
  depth: int;
}

let init_bt_stats () = {
  depth=0;
}

module Make(A: Abstract_domain) =
struct
  type bab = {
    kind: cmpop;
    objective: (A.I.var_id * vname);
    best: (A.snapshot * tqformula) option;
  }

  type printer = {
    print_node: (string -> int -> A.t -> unit);
    print_sol: (A.t -> unit);
  }

  type transformer =
  | Printer of printer
  | BAB of bab
  | Timeout of Mtime.span
  | BoundSolutions of int

  let make_bab kind objective = BAB { kind; objective; best=None }
  let minimize_bab = make_bab LT
  let maximize_bab = make_bab GT

  type gs = {
    transformers: transformer list;
    stats: global_statistics;
    domain: A.t;
  }

  type bs = {
    snapshot: A.snapshot;
    bt_stats: bt_statistics
  }

  type t = (gs * bs)

  let init domain transformers = {
    transformers=transformers;
    stats=init_global_stats ();
    domain=domain;
  },{
    snapshot=List.hd (A.lazy_copy domain 1);
    bt_stats=init_bt_stats ();
  }

  let unwrap_best gs =
    let rec aux = function
      | [] -> raise Not_found
      | BAB bab::_ ->
          begin match bab.best with
          | Some (_, (TQFFormula (_,TCmp (Var _, _, Cst (best,_))))) ->
              Some best
          | Some (_,_) -> failwith
              "[Transformer.unwrap_best] Best formula is suppose to be formatted as `v <op> best` where best is a constant."
          | None -> None
          end
      | _::l -> aux l
    in aux gs.transformers

  exception StopSearch of t
  exception Backjump of (int * t)

  let wrap_exception t f =
    try f t with
    | Bot.Bot_found -> raise (Backjump (0, t))
    | Conflict n -> raise (Backjump (n, t))

  let optimize (gs,bs) bab =
    match bab.best with
    | None -> (gs,bs)
    | Some (_,tf) ->
        wrap_exception (gs,bs) (fun (gs, bs) ->
          let domain, constraints = A.interpret gs.domain OverApprox tf in
          let domain = List.fold_left A.weak_incremental_closure domain constraints in
          ({gs with domain}, bs))

  let stop_if t = function
    | true -> raise (StopSearch t)
    | false -> t

  let on_node (gs,bs) =
    let nodes, depth = gs.stats.nodes+1, bs.bt_stats.depth+1 in
    let gs:gs = {gs with stats={gs.stats with nodes}} in
    let bs:bs = {bs with bt_stats={depth}} in
    let apply (gs,bs) = function
      | BAB bab -> (optimize (gs,bs) bab)
      | Timeout timeout ->
          let elapsed = Mtime_clock.count gs.stats.start in
          let gs = {gs with stats={gs.stats with elapsed}} in
          stop_if (gs,bs) ((Mtime.Span.compare timeout elapsed) <= 0)
      | _ -> gs,bs in
    List.fold_left apply (gs,bs) gs.transformers

  let on_fail (gs,bs) =
    let fails = gs.stats.fails+1 in
    let gs = {gs with stats={gs.stats with fails}} in
    let apply (gs,bs) = function
      | Printer printer ->
          printer.print_node "false" bs.bt_stats.depth gs.domain;
          (gs,bs)
      | _ -> gs,bs in
    List.fold_left apply (gs,bs) gs.transformers

  let on_solution (gs,bs) =
    let sols = gs.stats.sols+1 in
    let gs = {gs with stats={gs.stats with sols}} in
    let apply (gs,bs) transformer =
      match transformer with
      | BAB bab ->
          let v = fst bab.objective in
          let tv = A.I.to_logic_var (A.interpretation gs.domain) v in
          let (lb,ub) = A.project gs.domain v in
          let bound =
            (* let _ = Format.printf "new solution: [%a..%a]\n" A.B.pp_print lb A.B.pp_print ub in *)
            match bab.kind with
            | LT | LEQ -> lb
            | GT | GEQ -> ub
            | _ -> failwith "BAB kind should be < or >." in
          let bound = Cst (A.B.to_rat bound, Types.to_concrete_ty tv.ty) in
          (* let _ = Format.printf "Solution: [%a..%a]\n" A.B.pp_print lb A.B.pp_print ub; flush_all () in *)
          let tqf = TQFFormula (tv.uid, TCmp (Var (snd bab.objective), bab.kind, bound)) in
          let a = List.hd (A.lazy_copy gs.domain 1) in
          (gs,bs), BAB {bab with best=Some(a, tqf)}
      | Printer printer ->
          printer.print_sol gs.domain;
          printer.print_node "true" bs.bt_stats.depth gs.domain;
          (gs,bs), transformer
      | BoundSolutions s when s == gs.stats.sols -> raise (StopSearch (gs,bs))
      | _ -> (gs,bs), transformer in
    let (gs,bs), transformers = Tools.fold_map apply (gs,bs) gs.transformers in
    ({gs with transformers},bs)

  let on_unknown (gs,bs) =
    let apply (gs,bs) = function
      | Printer printer ->
          printer.print_node "unknown" bs.bt_stats.depth gs.domain;
          (gs,bs)
      | _ -> (gs,bs) in
    List.fold_left apply (gs,bs) gs.transformers
end
