(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

module Converter(B1: Bound_sig.S)(B2: Bound_sig.S) :
sig
  val convert_up: B1.t -> B2.t
  val convert_down: B1.t -> B2.t
end
