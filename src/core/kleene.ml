(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

type t = False | True | Unknown

let and_kleene x y =
  match x, y with
  | False, _   | _, False -> False
  | Unknown, _ | _, Unknown -> Unknown
  | True, True -> True

let or_kleene x y =
  match x, y with
  | True, _   | _, True -> True
  | Unknown, _ | _, Unknown -> Unknown
  | False, False -> False

let not_kleene = function
  | True -> False
  | False -> True
  | Unknown -> Unknown

let of_bool = function
  | true -> True
  | false -> False

(* TODO: move from this module (to box-reified?)
   `conjunction` is the result of the entailment of a conjunction in a
   reified context.  It returns the entailment status of the
   conjunction, with an optional index representing the only `unknown`
   value, if any.  See `box_reified` for an example. *)
let and_reified conjunction =
  let (n, t, f, u) =
    List.fold_left (fun (n, t, f, u) -> function
        | True -> (n+1, t+1, f, u)
        | False -> (n+1, t, f+1, u)
        | Unknown -> (n+1, t, f, n::u)) (0,0,0,[]) conjunction
  in
  if f > 0 then (False, None)
  else if t = n then (True, None)
  else
    match u with
    | [h] -> (Unknown, Some h)
    | _ ->  (Unknown, None)
