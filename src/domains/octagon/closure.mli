(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Dbm

module type Closure_sig =
sig
  module DBM : DBM_sig

  val closure: DBM.t -> DBM.t

  (** Perform the incremental closure of the DBM from a constraint.
      The complexity is O(n^2) where `n` is the dimension of the DBM.
      It does not check for entailment before running (this is done in `Octagon`). *)
  val incremental_closure: DBM.t -> DBM.bound dbm_constraint -> DBM.t

  val is_consistent : DBM.t -> DBM.t
end

module ClosureZ : Closure_sig
module ClosureHoistZ : Closure_sig
module ClosureQ : Closure_sig
module ClosureF : Closure_sig
