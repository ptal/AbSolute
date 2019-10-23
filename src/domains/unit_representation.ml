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

open Lang.Ast

module Unit_representation =
struct
  type t = unit
  type var_kind = unit
  type var_id = unit
  type rconstraint = unit

  let no_variable_exn () = raise (Wrong_modelling "`Unit_representation` does not allow to represent variables.")
  let empty = ()
  let extend _ _ = no_variable_exn ()
  let to_logic_var _ _ = no_variable_exn ()
  let to_abstract_var _ _ = no_variable_exn ()
  let rewrite _ _ = []
  let relax _ _ = []
  let negate _ = ()
end
