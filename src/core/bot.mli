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
