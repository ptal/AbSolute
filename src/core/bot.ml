(* Copyright 2014 Antoine Mine

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Adds a bottom element to a data-type. *)

type 'a bot = Bot | Nb of 'a

let strict_bot f x =
  match x with Bot -> Bot | Nb x -> f x

let lift_bot f x =
  match x with Bot -> Bot | Nb x -> Nb (f x)

let merge_bot2 x y =
  match x,y with
  | Bot,_  -> Bot
  | _,Bot -> Bot
  | Nb a, Nb b -> Nb (a,b)

let join_bot2 f x y =
  match x,y with Bot,a | a,Bot -> a | Nb a,Nb b -> Nb (f a b)

let meet_bot2 f x y =
  match x,y with Bot, _ | _, Bot -> Bot | Nb a, Nb b -> Nb (f a b)

let meet_bot f x y =
  match y with Bot -> Bot | Nb a -> f x a


let nobot =
  function Nb x -> x | Bot -> failwith "unexpected bottom encountered"


exception Bot_found

let debot =
  function Nb x -> x | Bot -> raise Bot_found

let rebot f x =
  try Nb (f x) with Bot_found -> Bot

let bot_to_string f = function Bot -> "_|_" | Nb x -> f x

let is_Bot = function
  | Bot -> true
  | _ -> false
