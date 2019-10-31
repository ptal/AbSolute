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
open Lang.Ast

module Logical_wrapper(A: Abstract_domain) =
struct

  module B = A.B
  module I = A.I

  (** The type of the abstract domain. *)
  type t = {
    a: A.t ref;
    r: I.t;
  }

  (* The following function essentially delegates the work to `A`. *)
  let wrap lw a =
    let _ = lw.a := a in lw

  let empty uid = {a = ref (A.empty uid); r = I.empty}
  let uid lw = A.uid !(lw.a)

  let lazy_copy lw n = List.init n (fun _ -> lw)
  let copy lw = lw
  let entailment lw c = A.entailment !(lw.a) c
  let project lw v = A.project !(lw.a) v

  let split _ = []
  let state_decomposition _ = Kleene.True
  let volume _ = 1.

  let print r fmt lw = A.print r fmt !(lw.a)

  let extend ?ty lw =
    let (a, id, aty) = A.extend ?ty !(lw.a) in
    wrap lw a, id, aty
  let closure lw = wrap lw (A.closure !(lw.a))
  let weak_incremental_closure lw c = wrap lw (A.weak_incremental_closure !(lw.a) c)

  (* These are the main function of this abstract domain. *)

  let init a r = {a;r}
  let unwrap lw = lw.a
  let interpretation lw = lw.r

  let rec qinterpret lw approx = function
    | QFFormula f ->
      begin
        match I.interpret lw.r approx f with
        | [] -> None
        | cs ->
            lw.a := List.fold_left A.weak_incremental_closure !(lw.a) cs;
            Some lw
      end
    | Exists (v, ty, qf) ->
        try
          ignore(I.to_abstract_var lw.r v);
          qinterpret lw approx qf
        (* We add the variable only if it is not already in `lw.r`. *)
        with Not_found ->
          let lw, v_id, aty = extend ~ty lw in
          let r = I.extend lw.r (v, v_id, aty) in
          qinterpret {lw with r} approx qf

  let print' fmt lw = print lw.r fmt lw

end
