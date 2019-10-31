(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Domains.Abstract_domain
open Domains.Logical_abstract_domain

module Logical_wrapper(A: Abstract_domain) :
sig
  include Logical_abstract_domain
  val init: A.t ref -> A.I.t -> t
  val unwrap: t -> A.t ref
end
