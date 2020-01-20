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
open Octagon_interpretation
open Domains.Abstract_domain

module Octagon_interpretation = Octagon_interpretation
module Dbm = Dbm
module Octagon_split = Octagon_split
module Closure = Closure

module type Octagon_sig =
sig
  module DBM: DBM_sig
  module B = DBM.B
  module I : module type of (Octagon_interpretation(DBM.B))
  include Abstract_domain with
    module I := I and
    module B := B

  (** Perform the incremental closure of the DBM with the constraint. *)
  val incremental_closure: t -> I.rconstraint -> t * bool

  (** Low-level access to the DBM. *)
  val unwrap: t -> DBM.t
end

module OctagonZ(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig
module OctagonQ(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig
module OctagonF(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig

module Make
  (Closure: Closure.Closure_sig)
  (SPLIT: Octagon_split.Octagon_split_sig) :
Octagon_sig
