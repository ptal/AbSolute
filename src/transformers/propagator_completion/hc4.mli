(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Pc_interpretation

module type PC_closure_sig = functor (I: PC_interpretation_sig) ->
sig
  module I: PC_interpretation_sig

  (** Perform the filtering of the constraint.
      Returns `(a, b)` where `a` is the resulting abstract element and `b` is true if the constraint is entailed. *)
  val incremental_closure: I.A.t -> I.rconstraint -> I.A.t * bool
  val entailment: I.A.t -> I.rconstraint -> bool
  val project: I.A.t -> I.var_id -> I.V.t
  val embed: I.A.t -> I.var_id -> I.V.bound * I.V.bound -> I.A.t
end with module I=I

module Make : PC_closure_sig
