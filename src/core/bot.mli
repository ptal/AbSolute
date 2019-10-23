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
exception Bot_found

val strict_bot: ('a -> 'b bot) -> 'a bot -> 'b bot
val lift_bot: ('a -> 'b) -> 'a bot -> 'b bot
val merge_bot2: 'a bot -> 'b bot -> ('a * 'b) bot
val join_bot2: ('a -> 'a -> 'a) -> 'a bot -> 'a bot -> 'a bot
val meet_bot2: ('a -> 'b -> 'c) -> 'a bot -> 'b bot -> 'c bot
val meet_bot: ('a -> 'b -> 'c bot) -> 'a -> 'b bot -> 'c bot
val nobot: 'a bot -> 'a
val debot: 'a bot -> 'a
val rebot: ('a -> 'b) -> 'a -> 'b bot
val bot_to_string: ('a -> string) -> 'a bot -> string
val is_Bot: 'a bot -> bool
