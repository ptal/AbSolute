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
open Lang.Ast
open Abstract_domain
open Interpretation

module type Logical_abstract_domain =
sig
  include Abstract_domain
  val interpretation: t -> I.t
  val qinterpret: t -> approx_kind -> qformula -> t option
  val print: Format.formatter -> t -> unit
end

module type Small_abstract_domain =
sig
  type t
  module I: Interpretation_sig
  val interpretation: t -> I.t
  val map_interpretation: t -> (I.t -> I.t) -> t
  val extend: ?ty:Types.var_ty -> t -> (t * I.var_id * Types.var_abstract_ty)
  val weak_incremental_closure: t -> I.rconstraint -> t
end

module QInterpreter_base(A: Small_abstract_domain) =
struct
  module I = A.I
  let rec qinterpret a approx = function
    | QFFormula f ->
      begin
        match I.interpret (A.interpretation a) approx f with
        | [] -> None
        | cs -> Some(List.fold_left A.weak_incremental_closure a cs)
      end
    | Exists (v, ty, qf) ->
        try
          (* Check if the variable `v` is or not in the abstract element yet. *)
          ignore(I.to_abstract_var (A.interpretation a) v);
          qinterpret a approx qf
        with Not_found ->
          let a, v_id, aty = A.extend ~ty a in
          let a = A.map_interpretation a (fun r -> A.I.extend r (v, v_id, aty)) in
          qinterpret a approx qf
end
