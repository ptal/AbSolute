(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module provides types and values for Kleene's three-valued
   logic in which there are three truth values indicating true, false
   and some unknown third value *)

type t = False | True | Unknown

val and_kleene : t -> t -> t
val or_kleene  : t -> t -> t
val not_kleene : t -> t
val of_bool    : bool -> t
val and_reified : t list -> t * int option
