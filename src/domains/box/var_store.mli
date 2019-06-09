open Box_representation

module type Var_store_sig =
sig
  type t
  module I: Vardom_sig.Vardom_sig
  type cell=I.t
  type key=box_var

  val empty: t
  val extend: t -> (t * key)

  (** `set store k v` is a monotonic `set`.
      It merges `v` with `store[k]` using `I.meet`. *)
  val set: t -> key -> cell -> t
  val get: t -> key -> cell
  val lazy_copy : t -> int -> t list
  val copy : t -> t
  val iter: (key -> cell -> unit) -> t -> unit
  val fold: ('a -> key -> cell -> 'a) -> 'a -> t -> 'a
  val print: Format.formatter -> Box_rep.t -> t -> unit

  (** This function consumes the registered delta in the store.
      The returned store has an empty list of delta. *)
  val delta: t -> t * key list
end

module type Var_store_functor = functor (I: Vardom_sig.Vardom_sig) -> Var_store_sig with module I=I

module Make : Var_store_functor
