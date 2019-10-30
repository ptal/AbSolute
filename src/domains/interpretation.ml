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

type approx_kind =
| Exact
| UnderApprox
| OverApprox

module type Interpretation_sig = sig
  type t
  type var_id
  type rconstraint
  val empty: t
  val extend: t -> (Ast.var * var_id * Types.var_abstract_ty) -> t
  val to_logic_var: t -> var_id -> (Ast.var * Types.var_abstract_ty)
  val to_abstract_var: t -> Ast.var -> (var_id * Types.var_abstract_ty)
  val interpret: t -> Ast.formula -> approx_kind -> rconstraint list
  val to_qformula: t -> rconstraint list -> Ast.qformula
  val negate: rconstraint -> approx_kind -> rconstraint option
end

module Interpretation_base(V_ID:sig type var_id end) =
struct
  type var_id = V_ID.var_id

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t = var_id
    let compare = compare end)
  type t = {
    (* Maps each variable name to its index. *)
    env: (var_id * Types.var_abstract_ty) Env.t;
    (* reversed mapping of `env`. *)
    renv: (Ast.var * Types.var_abstract_ty) REnv.t;
  }

  let empty = {env=Env.empty; renv=REnv.empty}
  let extend repr (v,idx,ty) = {
    env=(Env.add v (idx,ty) repr.env);
    renv=(REnv.add idx (v,ty) repr.renv);
  }

  let to_logic_var repr idx = REnv.find idx repr.renv
  let to_abstract_var repr v = Env.find v repr.env

  let to_logic_var' repr idx = fst (to_logic_var repr idx)
  let to_abstract_var' repr v = fst (to_abstract_var repr v)
end
