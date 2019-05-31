open Box_representation

module type Var_store_sig =
sig
  type t
  module I: Itv_sig.ITV
  type cell=I.t
  type key=box_var

  val empty: t
  val extend: t -> (t * key)
  val set: t -> key -> cell -> t
  val get: t -> key -> cell
  val lazy_copy : t -> int -> t list
  val copy : t -> t
  val iter: (key -> cell -> unit) -> t -> unit
  val fold: ('a -> key -> cell -> 'a) -> 'a -> t -> 'a
  val print: Format.formatter -> Box_rep.t -> t -> unit
end

module type Var_store_functor = functor (I: Itv_sig.ITV) -> Var_store_sig with module I=I

module Make : Var_store_functor
