module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem:  key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val singleton: key -> 'a -> 'a t
    val remove: key -> 'a t -> 'a t
    val merge: (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    val exists: (key -> 'a -> bool) -> 'a t -> bool
    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal: 'a t -> int
    val bindings: 'a t -> (key * 'a) list
    val min_binding: 'a t -> (key * 'a)
    val max_binding: 'a t -> (key * 'a)
    val choose: 'a t -> (key * 'a)
    val split: key -> 'a t -> 'a t * 'a option * 'a t
    val find: key -> 'a t -> 'a
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t


    (* [AM] additions by Antoine Mine' *)

    val of_list: (key * 'a) list -> 'a t

    val map2: (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val iter2: (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
    val fold2: (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
    val for_all2: (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val exists2: (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool

    val map2z: (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val iter2z: (key -> 'a -> 'a -> unit) -> 'a t -> 'a t -> unit
    val fold2z: (key -> 'a -> 'a -> 'b -> 'b) -> 'a t -> 'a t -> 'b -> 'b
    val for_all2z: (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val exists2z: (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val map2o: (key -> 'a -> 'c) -> (key -> 'b -> 'c) -> (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val iter2o: (key -> 'a -> unit) -> (key -> 'b -> unit) -> (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
    val fold2o: (key -> 'a -> 'c -> 'c) -> (key -> 'b -> 'c -> 'c) -> (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
    val for_all2o: (key -> 'a -> bool) -> (key -> 'b -> bool) -> (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val exists2o: (key -> 'a -> bool) -> (key -> 'b -> bool) -> (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool

    val map2zo: (key -> 'a -> 'a) -> (key -> 'a -> 'a) -> (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val iter2zo: (key -> 'a -> unit) -> (key -> 'a -> unit) -> (key -> 'a -> 'a -> unit) -> 'a t -> 'a t -> unit
    val fold2zo: (key -> 'a -> 'b -> 'b) -> (key -> 'a -> 'b -> 'b) -> (key -> 'a -> 'a -> 'b -> 'b) -> 'a t -> 'a t -> 'b -> 'b
    val for_all2zo: (key -> 'a -> bool) -> (key -> 'a -> bool) -> (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
     val exists2zo: (key -> 'a -> bool) -> (key -> 'a -> bool) -> (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val map_slice: (key -> 'a -> 'a) -> 'a t -> key -> key -> 'a t
    val iter_slice: (key -> 'a -> unit) -> 'a t -> key -> key -> unit
    val fold_slice: (key -> 'a -> 'b -> 'b) -> 'a t -> key -> key -> 'b -> 'b
    val for_all_slice: (key -> 'a -> bool) -> 'a t -> key -> key -> bool
    val exists_slice: (key -> 'a -> bool) -> 'a t -> key -> key -> bool

    val key_equal: 'a t -> 'a t -> bool
    val key_subset: 'a t -> 'a t -> bool

    val find_greater: key -> 'a t -> key * 'a
    val find_less: key -> 'a t -> key * 'a
    val find_greater_equal: key -> 'a t -> key * 'a
    val find_less_equal: key -> 'a t -> key * 'a
  end

module Make(Ord: OrderedType): S with type key = Ord.t
