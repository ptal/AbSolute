(** This file implements Kleene's three-valued logic in which there
   are three truth values indicating true, false and some
   indeterminate third value *)

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
