open Vardom_sig

module type Var_store_sig =
sig
  type t
  module V: Vardom_sig
  type cell=V.t
  type key=int

  val empty: t
  val extend: t -> (t * key)
  val set: t -> key -> cell -> t
  val get: t -> key -> cell
  val lazy_copy : t -> int -> t list
  val copy : t -> t
  val iter: (key -> cell -> unit) -> t -> unit
  val fold: ('a -> key -> cell -> 'a) -> 'a -> t -> 'a
  val delta: t -> t * key list
end

module type Var_store_functor = functor (V: Vardom_sig) -> Var_store_sig with module V=V

module Make(V: Vardom_sig) =
struct
  module V = V
  type cell = V.t
  type key = int
  module Store = Parray
  type t = {
    store: cell Store.t;
    delta: key list;
  }

  let empty = {
    store=Store.make 0 (V.create V.TOP);
    delta=[] }

  let extend data =
    let n = Store.length data.store in
    let store = Store.init (n+1) (fun i -> if i < n then Store.get data.store i else (V.create V.TOP)) in
    ({data with store}, n)

  let set data k merge =
    let old = Store.get data.store k in
    let newval = Bot.debot (V.meet merge old) in
    if V.equal old newval then data
    else begin
      { store=Store.set data.store k newval;
        delta=k::data.delta}
    end

  let get data = Store.get data.store

  let lazy_copy data n = List.init n (fun _ -> data)

  let copy data = data

  let iter f data = Store.iteri f data.store

  let fold f acc data =
    let i = ref (-1) in
    Store.fold_left (fun acc x -> i := !i+1; f acc !i x) acc data.store

  let delta data = { data with delta=[] }, data.delta
end
