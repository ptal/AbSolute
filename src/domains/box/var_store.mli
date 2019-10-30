(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Vardom.Vardom_sig

module type Var_store_sig =
sig
  type t
  module V: Vardom_sig
  type cell=V.t
  type key=int

  val empty: t
  val extend: ?ty:Types.var_ty -> t -> (t * key * Types.var_abstract_ty)

  (** `set store k v` is a monotonic `set`.
      It merges `v` with `store[k]` using `V.meet`. *)
  val set: t -> key -> cell -> t
  val get: t -> key -> cell
  val lazy_copy : t -> int -> t list
  val copy : t -> t
  val iter: (key -> cell -> unit) -> t -> unit
  val fold: ('a -> key -> cell -> 'a) -> 'a -> t -> 'a

  (** This function consumes the registered delta in the store.
      The returned store has an empty list of delta. *)
  val delta: t -> t * key list
end

module type Var_store_functor = functor (V: Vardom_sig) -> Var_store_sig with module V=V

module Make : Var_store_functor
