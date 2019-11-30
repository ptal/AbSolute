(* Copyright 2019 Pierre Talbot, Albin Coquereau

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module encapsulates a CDCL SAT solver based on MiniSat, and implemented by Albin Coquereau in OCaml in a library called Minisatml.
    WARNING: only one instance of this module can be used at once, this is because Minisatml relies on global variables. *)

open Bounds
open Sat_interpretation
open Event_loop.Event_abstract_domain

module Sat:
  Event_abstract_domain with
    module B=Bound_int and
    module I=Sat_interpretation
