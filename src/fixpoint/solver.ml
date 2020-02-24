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

  let solve ?strategy:(strategy=Simple) (gs,bs) =
    let rec aux stack (gs,bs) =
      match stack with
      | [] -> (gs,bs)
      | (depth,bs)::stack ->
        try
          (* Restore the current abstract domain with the snapshot registered in BS. *)
          let gs = T.{gs with domain=(A.restore gs.domain bs.snapshot)} in
          let (gs,bs) = T.on_node (gs,bs) in
          let (gs,bs) = T.wrap_exception (gs,bs) (fun (gs,bs) ->
            let rec fixpoint_closure domain =
              let domain = A.closure domain in
              if A.has_changed domain then fixpoint_closure domain
              else domain in
            ({gs with domain=(fixpoint_closure gs.domain)}, bs)) in
          match A.state gs.domain with
          | Kleene.False -> aux stack (T.on_fail (gs,bs))
          | Kleene.True -> aux stack (T.on_solution (gs,bs))
          | Kleene.Unknown ->
              let (gs,bs) = T.on_unknown (gs,bs) in
              let branches = A.split ~strategy gs.domain in
              (* In an `unknown` setting, an empty set of branches means that all the children nodes were detected inconsistent. *)
              if branches = [] then T.on_fail (gs,bs)
              else
                let bss = List.map (fun snapshot -> (depth+1),{bs with snapshot}) branches in
                aux (bss@stack) (gs,bs)
        with T.Backjump (n, t) ->
          let rec cut_below d = function
            | (depth,_)::stack when depth > d -> cut_below d stack
            | stack -> stack in
          aux (cut_below (depth-n) stack) (T.on_fail t)
    in aux [(0,bs)] (gs,bs)

end
