(** This module provides types and values for Kleene's three-valued
   logic in which there are three truth values indicating true, false
   and some unknown third value *)

type t = False | True | Unknown

val and_kleene : t -> t -> t
val or_kleene  : t -> t -> t
val not_kleene : t -> t
val of_bool    : bool -> t
val and_reified : t list -> t * int option
