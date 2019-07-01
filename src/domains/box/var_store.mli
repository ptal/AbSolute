open Vardom_sig

module type Var_store_sig =
sig
  type t
  module V: Vardom_sig
  type cell=V.t
  type key=int

  val empty: t
  val extend: t -> (t * key)

  (** `set store k v` is a monotonic `set`.
      It merges `v` with `store[k]` using `V.meet`. *)
  val set: t -> key -> cell -> t
  val get: t -> key -> cell
  val lazy_copy : t -> int -> t list
  val copy : t -> t
  val iter: (key -> cell -> unit) -> t -> unit
  val fold: ('a -> key -> cell -> 'a) -> 'a -> t -> 'a

  (** This function consumes the registered delta in the store.
      The returned store has an empty list of delta. *)
  val delta: t -> t * key list
end

module type Var_store_functor = functor (V: Vardom_sig) -> Var_store_sig with module V=V

module Make : Var_store_functor
