(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module provides a customizable solving algorithm for abstract domain. *)
open Domains.Abstract_domain
open Transformer

module Make(Domain: Abstract_domain):
sig
  module T: module type of(Transformer(Domain))

  val solve: T.t -> T.t
end
