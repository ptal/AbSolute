(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Lang
open Typing
open Typing.Tast
open Typing.Ad_type

type approx_kind =
| Exact
| UnderApprox
| OverApprox

let neg_approx = function
  | Exact -> Exact
  | UnderApprox -> OverApprox
  | OverApprox -> UnderApprox

let string_of_approx = function
  | Exact -> "exactly approximated"
  | UnderApprox -> "under-approximated"
  | OverApprox -> "over-approximated"

module type Interpretation_sig = sig
  type t
  type var_id
  type rconstraint
  val exact_interpretation: bool
  val empty: ad_uid -> t
  val exists: t -> Ast.vname -> bool
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> Ast.vname -> (var_id * Tast.tvariable)
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> Tast.tqformula
end

module Interpretation_base(V_ID:sig type var_id end) =
struct
  type var_id = V_ID.var_id

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t = var_id
    let compare = compare end)
  type t = {
    ad_uid: ad_uid;
    (* Maps each variable name to its index. *)
    env: (var_id * tvariable) Env.t;
    (* reversed mapping of `env`. *)
    renv: tvariable REnv.t;
  }

  let empty ad_uid = {ad_uid; env=Env.empty; renv=REnv.empty}
  let uid repr = repr.ad_uid
  let extend repr (id, v) = {
    repr with
    env=(Env.add v.name (id, v) repr.env);
    renv=(REnv.add id v repr.renv);
  }

  let to_logic_var repr id = REnv.find id repr.renv
  let to_abstract_var repr vname = Env.find vname repr.env
  let exists repr vname = Env.mem vname repr.env

  let equantify repr tf =
    Tast.quantify (List.map snd (REnv.bindings repr.renv)) tf

  let to_logic_var' repr id = (to_logic_var repr id).name
  let to_abstract_var' repr vname = fst (to_abstract_var repr vname)

  let to_abstract_var_wm repr vname =
    try
      to_abstract_var repr vname
    with Not_found ->
      raise (Ast.Wrong_modelling
        ("Variable `" ^ vname ^ "` does not belong to the current abstract element."))
end

module Interpretation_ground(V_ID:sig type var_id end) =
struct
  module IB = Interpretation_base(V_ID)
  include IB

  let interpret_gen' repr ad_name tf interpret =
    let open Ast in
    if snd tf = ctrue then repr, []
    else
    begin
      if (fst tf) <> (IB.uid repr) then
        raise (Wrong_modelling (ad_name ^ ".interpret: The formula has the UID "
          ^ (string_of_int (fst tf)) ^ " but the " ^ ad_name ^ " element has the UID "
          ^ (string_of_int (IB.uid repr)) ^ "."));
      interpret repr tf
    end

  let interpret_gen repr ad_name tf interpret_bconstraint =
    let open Ast in
    interpret_gen' repr ad_name tf (fun repr tf ->
      let rec aux (uid, f) =
        match f with
        | TCmp c -> interpret_bconstraint repr c
        | TFVar x -> interpret_bconstraint repr (Var x, EQ, one)
        | TNot ((_,TFVar x)) -> interpret_bconstraint repr (Var x, EQ, zero)
        | TAnd (tf1, tf2) -> (aux tf1)@(aux tf2)
        | _ -> raise (Wrong_modelling (
            ad_name ^ ".interpret: " ^ ad_name ^ " do not support logical constraints (see e.g. `Logic_completion`). UID = " ^
            (string_of_int uid) ^ " - " ^ ad_name ^ " UID: " ^ (string_of_int (IB.uid repr)) ^
            " - Formula " ^ (Lang.Pretty_print.string_of_formula (tformula_to_formula (uid,f)))))
      in
      (repr, aux tf)
    )

  let to_qformula_gen repr cs to_formula_one =
    let fs = List.map (fun c -> TQFFormula (to_formula_one repr c)) cs in
    let tqf = q_conjunction (IB.uid repr) fs in
    match tqf with
    | TQFFormula tf -> equantify repr tf
    | _ -> failwith "unreachable (to_qformula): no variable should be inserted."
end
