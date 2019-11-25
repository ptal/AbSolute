(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module provides functions to detect and rewrite arbitrary constraint into an equivalent and relaxed octagonal constraints, if possible. *)

open Lang.Ast
open Bounds
open Dbm
open Domains.Interpretation

module type Octagon_interpretation_sig =
sig
  module B: Bound_sig.S
  type rconstraint = B.t dbm_constraint
  include module type of (Interpretation_base(struct type var_id=dbm_var end))

  (** Create octagonal constraints from a formula.
      Multiple constraints mean the formula has been decomposed into several octagonal constraints.
      For discrete bound, exact approximation is supported for (strict) inequalities.
      For continuous bound strict inequalities must be over- or under-approximated:
        * Over-approximation: it rewrites strict inequalities `<`,`>` into the inequalities `<=`,`>=`.
        * Under-approximation: `x + y < d` into `x + y <= d - w` where `w` is a small as possible.
      See also [Interpretation.interpret].*)
  val interpret: t -> approx_kind -> formula -> t * rconstraint list

  (** See [Interpretation.to_qformula] *)
  val to_qformula: t -> rconstraint list -> qformula
end

module Octagon_interpretation(B: Bound_sig.S): Octagon_interpretation_sig with module B=B
