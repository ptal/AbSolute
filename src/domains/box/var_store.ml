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
open Vardom

module type Var_store_sig =
sig
  type t
  module V: Vardom_sig.S
  type cell=V.t
  type key=int

  val empty: t
  val length: t -> int
  val extend: ?ty:Types.var_ty -> t -> (t * key * Types.var_abstract_ty)
  val set: t -> key -> cell -> t
  val get: t -> key -> cell
  val lazy_copy : t -> int -> t list
  val copy : t -> t
  val iter: (key -> cell -> unit) -> t -> unit
  val fold: ('a -> key -> cell -> 'a) -> 'a -> t -> 'a
  val delta: t -> t * key list
end

module type Var_store_functor = functor (V: Vardom_sig.S) -> Var_store_sig with module V=V

module Make(V: Vardom_sig.S) =
struct
  module V = V
  type cell = V.t
  type key = int
  module Store = Parray
  type t = {
    store: cell Store.t;
    delta: key list;
  }

  let empty = {
    store=Store.make 0 (fst (V.top ()));
    delta=[] }

  let length data = Parray.length data.store

  let extend ?ty data =
    let n = Store.length data.store in
    let v,aty = V.top ?ty () in
    let store = Store.init (n+1) (fun i -> if i < n then Store.get data.store i else v) in
    ({data with store}, n, aty)

  let set data k merge =
    let old = Store.get data.store k in
    let newval = Bot.debot (V.meet merge old) in
    if V.equal old newval then data
    else begin
      { store=Store.set data.store k newval;
        delta=k::data.delta}
    end

  let get data = Store.get data.store

  let lazy_copy data n = List.init n (fun _ -> data)

  let copy data = data

  let iter f data = Store.iteri f data.store

  let fold f acc data =
    let i = ref (-1) in
    Store.fold_left (fun acc x -> i := !i+1; f acc !i x) acc data.store

  let delta data = { data with delta=[] }, data.delta
end
