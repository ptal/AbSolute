(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Bounds
open Interpretation
open Lang
open Lang.Ast

exception Conflict of int

type ad_uid = int

module type Logical_abstract_domain =
sig
end

module type Abstract_domain =
sig
  module I: Interpretation_sig
  type t
  module B: Bound_sig.S
  val empty: ad_uid -> t
  val uid: t -> ad_uid
  val name: string
  val interpretation: t -> I.t
  val map_interpretation: t -> (I.t -> I.t) -> t
  val qinterpret: t -> approx_kind -> Ast.qformula -> t
  val extend: ?ty:Types.var_ty -> t -> (t * I.var_id * Types.var_abstract_ty)
  val project: t -> I.var_id -> (B.t * B.t)
  type snapshot
  val lazy_copy: t -> int -> snapshot list
  val restore: t -> snapshot -> t
  val closure: t -> (t * bool)
  val weak_incremental_closure: t -> I.rconstraint -> t
  val entailment: t -> I.rconstraint -> bool
  val split: t -> snapshot list
  val volume: t -> float
  val state: t -> Kleene.t
  val print: Format.formatter -> t -> unit
end

module type Small_abstract_domain =
sig
  type t
  module I: Interpretation_sig
  val name: string
  val interpretation: t -> I.t
  val map_interpretation: t -> (I.t -> I.t) -> t
  val extend: ?ty:Types.var_ty -> t -> (t * I.var_id * Types.var_abstract_ty)
  val weak_incremental_closure: t -> I.rconstraint -> t
end

module QInterpreter_base(A: Small_abstract_domain) =
struct
  module I = A.I
  let qinterpret a approx f =
    let rec aux a approx = function
    | QFFormula f ->
        let (i, cs) = I.interpret (A.interpretation a) approx f in
        let a = A.map_interpretation a (fun _ -> i) in
        List.fold_left A.weak_incremental_closure a cs
    | Exists (v, ty, qf) ->
        try
          (* Check if the variable `v` is or not in the abstract element yet. *)
          ignore(I.to_abstract_var (A.interpretation a) v);
          aux a approx qf
        with Not_found ->
          let a, v_id, aty = A.extend ~ty a in
          let a = A.map_interpretation a (fun r -> A.I.extend r (v, v_id, aty)) in
          aux a approx qf
    in
      try aux a approx f
      with Wrong_modelling msg ->
        raise (Wrong_modelling ("[" ^ A.name ^ "] " ^ msg))
end
