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
open Domains.Abstract_domain
open Transformer

module Make(Domain: Abstract_domain) =
struct
  module T=Transformer(Domain)

  let rec solve t =
    try
      let (gs,bs) = T.on_node t in
      let (gs,bs) = T.wrap_exception (gs,bs) (fun (gs,bs) ->
        (gs,{ bs with domain=Domain.closure bs.domain})) in
      match Domain.state_decomposition bs.domain with
      | Kleene.False -> T.on_fail (gs,bs)
      | Kleene.True -> T.on_solution (gs,bs)
      | Kleene.Unknown ->
          let (gs,bs) = T.on_unknown (gs,bs) in
          let branches = Domain.split bs.domain in
          let bss = List.map (fun domain -> {bs with domain}) branches in
          List.fold_left (fun (gs,_) bs -> solve (gs,bs)) (gs,bs) bss
    with
    | T.Backjump (0, t) -> t
    | T.Backjump (n, t) -> raise (T.Backjump ((n-1), t))
end
