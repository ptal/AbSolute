(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Typing.Tast
open Domains.Interpretation

module type Sat_interpretation_sig =
sig
  type rconstraint = Minisatml.Types.Lit.lit Minisatml.Vec.t
  include module type of (Interpretation_ground(
    struct type
      var_id=Minisatml.Solver.var
    end))

  val exact_interpretation: bool

  (** Create a set of clauses from a formula.
      The formula is rewritten into CNF. *)
  val interpret: t -> approx_kind -> tformula -> t * rconstraint list

  (** See [Interpretation.to_tqformula] *)
  val to_qformula: t -> rconstraint list -> tqformula
end

module Sat_interpretation: Sat_interpretation_sig
