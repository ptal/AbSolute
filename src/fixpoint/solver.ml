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

module Make(A: Abstract_domain) =
struct
  module T=Transformer.Make(A)

  let rec solve (gs,bs) =
    try
      (* Restore the current abstract domain with the snapshot registered in BS. *)
      let gs = T.{gs with domain=(A.restore gs.domain bs.snapshot)} in
      let (gs,bs) = T.on_node (gs,bs) in
      let (gs,bs) = T.wrap_exception (gs,bs) (fun (gs,bs) ->
        let rec fixpoint_closure (domain, has_changed) =
          if has_changed then fixpoint_closure (A.closure domain)
          else domain in
        ({gs with domain=(fixpoint_closure (gs.domain,true))}, bs)) in
      match A.state gs.domain with
      | Kleene.False -> T.on_fail (gs,bs)
      | Kleene.True -> T.on_solution (gs,bs)
      | Kleene.Unknown ->
          let (gs,bs) = T.on_unknown (gs,bs) in
          let branches = A.split gs.domain in
          let bss = List.map (fun snapshot -> {bs with snapshot}) branches in
          List.fold_left (fun (gs,_) bs -> solve (gs,bs)) (gs,bs) bss
    with
    | T.Backjump (0, t) -> T.on_fail t
    | T.Backjump (n, t) -> raise (T.Backjump ((n-1), t))
end
