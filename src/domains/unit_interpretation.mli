(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module is only provided when an abstract domain has no "representation".
    This is case for meta abstract domain coordinating others abstract domain.
    See also `Bound_unit`. *)

include Interpretation.Interpretation_sig with
  type t = unit and
  type var_id = unit and
  type rconstraint = unit