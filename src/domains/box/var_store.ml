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

module Make(I: Itv_sig.ITV) =
struct
  module I = I
  type cell = I.t
  type key = box_var
  module Store = Parray
  type t = cell Store.t

  let empty = Store.make 0 I.top
  let extend store =
    let n = Store.length store in
    let store = Store.init (n+1) (fun i -> if i < n then Store.get store i else I.top) in
    (store, n)
  let set = Store.set
  let get = Store.get
  let lazy_copy store n = List.init n (fun _ -> store)
  let copy store = store
  let iter = Store.iteri
  let fold f acc store =
    let i = ref (-1) in
    Store.fold_left (fun acc x -> i := !i+1; f acc !i x) acc store
  let print fmt repr store =
    let print_entry v i =
      Format.fprintf fmt "%s=%a \n" (Box_rep.to_logic_var repr v) I.print i in
    iter print_entry store
end
