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
  val delta: t -> ('a -> key -> 'a) -> 'a -> 'a
end

module type Var_store_functor = functor (I: Itv_sig.ITV) -> Var_store_sig with module I=I

module Make(I: Itv_sig.ITV) =
struct
  module I = I
  type cell = I.t
  type key = box_var
  module Store = Parray
  type t = {
    store: cell Store.t;
    (** This field is not backtracked, neither copied.
       It acts mostly as a cache, thus it might over-approximate the set of deltas.
       This does not hurt correctness (some propagators will be rescheduled even if it is not necessary). *)
    delta: bool array;
  }

  let empty = {
    store=Store.make 0 I.top;
    delta=Array.make 0 false; }

  let extend data =
    let n = Store.length data.store in
    let store = Store.init (n+1) (fun i -> if i < n then Store.get data.store i else I.top) in
    let delta = Array.init (n+1) (fun i -> if i < n then Array.get data.delta i else false) in
    ({store; delta}, n)

  let set data k merge =
    let old = Store.get data.store k in
    let newval = Bot.debot (I.meet merge old) in
    if I.equal old newval then data
    else begin
      Array.set data.delta k true;
      {data with store=Store.set data.store k newval}
    end

  let get data = Store.get data.store

  let lazy_copy data n = List.init n (fun _ -> data)

  let copy data = data

  let iter f data = Store.iteri f data.store

  let fold f acc data =
    let i = ref (-1) in
    Store.fold_left (fun acc x -> i := !i+1; f acc !i x) acc data.store

  let print fmt repr data =
    let print_entry v i =
      Format.fprintf fmt "%s=%a \n" (Box_rep.to_logic_var repr v) I.print i in
    iter print_entry data

  let delta data f acc =
    let acc = ref acc in
    for i=0 to (Array.length data.delta)-1 do
      if Array.get data.delta i then
        acc := f (!acc) i
    done;
    !acc
end
