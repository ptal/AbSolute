(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Bounds
open Lang

type dbm_var = {
  l: int;
  c: int;
}
type 'b dbm_constraint = {
  v: dbm_var;
  d: 'b;
}
type dbm_interval = {
  lb: dbm_var;
  ub: dbm_var;
}

(* The test might be `c/2 >= l/2`, however we can avoid two divisions in the regular case by considering `c > l`.
   For example, in the valid case where c=3 and l=2, we still have the same result: c=2^1, l=3^1 . *)
let make_var l c = if c > l then {l=c lxor 1; c=l lxor 1} else {l=l; c=c}

let check_coherence v = if v.c/2 > v.l/2 then raise (Failure "variable must be initialized with `make_var` to be coherent.")

let inv v = (check_coherence v; { l=v.l lxor 1; c=v.c lxor 1 })
let is_lower_bound v = (check_coherence v; (v.l mod 2) = 0)
let is_upper_bound v = (check_coherence v; (v.l mod 2) = 1)
let as_interval v = if is_lower_bound v then {lb=v; ub=inv v} else {lb=inv v; ub=v}
let is_rotated v = (v.l / 2) <> (v.c / 2)

(* Precondition: `v` is coherent, i.e. v.x/2 <= v.y/2 *)
let matpos v = (check_coherence v; v.c + ((v.l+1)*(v.l+1))/2)

let event_of_var = matpos

module type Fold_interval_sig =
sig
  val fold: ('a -> dbm_interval -> 'a) -> 'a -> int -> 'a
end

module Fold_intervals =
struct
  let fold f accu dimension =
    List.fold_left (fun accu l ->
      List.fold_left (fun accu c ->
        let accu = if l <> c then (* if rotated there are two intervals. *)
          f accu (as_interval {l=l*2; c=c*2})
        else
          accu in
        f accu (as_interval {l=l*2; c=c*2+1})
      ) accu (Tools.range 0 l)
    ) accu (Tools.range 0 (dimension-1))
end

let make_canonical_var k = {l=k*2; c=k*2+1}
let make_canonical_itv k = as_interval (make_canonical_var k)

module Fold_intervals_canonical =
struct
  let fold f accu dimension =
    List.fold_left (fun accu k ->
      f accu (make_canonical_itv k)
    ) accu (Tools.range 0 (dimension-1))
end

module Fold_intervals_rotated =
struct
  let fold f accu dimension =
    List.fold_left (fun accu l ->
      List.fold_left (fun accu c ->
        let accu = f accu (as_interval {l=l*2; c=c*2}) in
        f accu (as_interval {l=l*2; c=c*2+1})
      ) accu (Tools.range 0 (l-1))
    ) accu (Tools.range 1 (dimension-1))
end

(* We parametrize the DBM with a value store which is mutable (with Array) or immutable (with Parray).
   This signature unifies these two interfaces. *)
module type Array_store_sig =
sig
  type 'a t
  val init : int -> (int -> 'a) -> 'a t
  val make : int -> 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set' : 'a t -> int -> 'a -> 'a t
  val to_list : 'a t -> 'a list
  val copy: 'a t -> 'a t
  val lazy_copy: 'a t -> int -> ('a t) list
end

module Array_store =
struct
  include Array
  type 'a t = 'a array
  let set' m i e = (set m i e; m)
  let lazy_copy m n = m::(List.init (n-1) (fun _ -> copy m))
end

module Parray_store =
struct
  include Parray
  let set' = Parray.set
  let copy m = m
  let lazy_copy m n = List.init n (fun _ -> m)
end

module type DBM_sig =
sig
  module B: Bound_sig.S
  type bound = B.t
  type t
  val init: int -> t
  val empty: t
  val extend: ?ty:Types.var_ty -> t -> (t * dbm_var * Types.var_abstract_ty)
  val get : t -> dbm_var -> bound
  val set : t -> bound dbm_constraint -> t
  val project: t -> dbm_interval -> (bound * bound)
  val lazy_copy : t -> int -> t list
  val copy : t -> t
  val dimension: t -> int
  val to_list: t -> bound list
  val print: Format.formatter -> t -> unit
  val delta: t -> t * int list
end

module MakeDBM(A: Array_store_sig)(B:Bound_sig.S) = struct
  module B=B
  type bound = B.t
  type t = {
    dim: int;
    m: bound A.t;
    delta: int list;
  }

  let init n =
    let rec size n = if n = 0 then 0 else (n*2*2) + size (n-1) in
    {dim=n; m=A.make (size n) B.inf; delta=[]}

  let empty = {dim=0; m=A.make 0 B.inf; delta=[]}

  let extend ?(ty=Types.Abstract B.abstract_ty) dbm =
    if ty=(Abstract Bool) then
      raise (Ast.Wrong_modelling "Octagon should not handled Boolean variables, it is too inefficient.");
    Ast.type_dispatch (module B) "Octagon" ty (fun () ->
      let rec size n = if n = 0 then 0 else (n*2*2) + size (n-1) in
      let n = size dbm.dim in
      let n' = size (dbm.dim+1) in
      let dbm' = { dbm with
        dim=(dbm.dim+1);
        m=A.init n' (fun i -> if i < n then A.get dbm.m i else B.inf)} in
      (dbm', make_canonical_var dbm.dim, B.abstract_ty))

  let get dbm v = A.get dbm.m (matpos v)

  let set dbm dbm_cons =
    let pos = matpos dbm_cons.v in
    if B.gt (A.get dbm.m pos) dbm_cons.d then
      { dbm with m=A.set' dbm.m pos dbm_cons.d;
          delta=pos::dbm.delta }
    else
      dbm

  let project dbm itv = (B.neg (get dbm itv.lb)), get dbm itv.ub
  let lazy_copy dbm n = List.map (fun m -> {dbm with m}) (A.lazy_copy dbm.m n)
  let copy dbm = {dbm with m=(A.copy dbm.m)}

  let dimension dbm = dbm.dim

  let to_list dbm = A.to_list dbm.m

  let print fmt dbm =
    for l = 0 to (2*dbm.dim-1) do
      for c = 0 to (2*dbm.dim-1) do
        Format.fprintf fmt "%s " (B.to_string (get dbm (make_var l c)))
      done;
      Format.fprintf fmt "\n"
    done

  let delta dbm = { dbm with delta=[] }, dbm.delta
end

module MakeCopy = MakeDBM(Array_store)
module MakeTrailing = MakeDBM(Parray_store)
module Make = MakeTrailing

(* module Make(B:Bound_sig.S) = struct
  module B=B
  type bound = B.t
  type t = {
    dim: int;
    m: bound Parray.t;
    depth: int;
    num_set_global: int ref;
    num_true_set_global: int ref;
    num_set: int;
    true_set: (int, unit) Hashtbl.t;
  }

  let init n =
    let rec size n = if n = 0 then 0 else (n*2*2) + size (n-1) in
    {dim=n; m=Parray.make (size n) B.inf; depth=0; num_set = 0; true_set = Hashtbl.create (size n)
     ; num_set_global = ref 0; num_true_set_global = ref 0; }

  (* Precondition: `v` is coherent, i.e. v.x/2 <= v.y/2 *)
  let matpos v = (check_coherence v; v.c + ((v.l+1)*(v.l+1))/2)

  let get dbm v = Parray.get dbm.m (matpos v)

  let set dbm dbm_cons =
    let pos = matpos dbm_cons.v in
    if B.gt (Parray.get dbm.m pos) dbm_cons.d then
      (if Hashtbl.mem dbm.true_set pos then () else (Hashtbl.add dbm.true_set pos (); dbm.num_true_set_global := !(dbm.num_true_set_global) + 1);
       dbm.num_set_global := !(dbm.num_set_global) + 1;
      {dbm with m=Parray.set dbm.m pos dbm_cons.d; num_set = dbm.num_set + 1})
    else
      dbm

  let project dbm itv = (B.neg (get dbm itv.lb)), get dbm itv.ub

  let copy_one dbm = { dbm with depth=dbm.depth+1; num_set=0; true_set=Hashtbl.create (Parray.length dbm.m)}
  let copy dbm n =
    let _ = List.iter (fun _ -> Printf.printf ".") (Tools.range 1 dbm.depth) in
    let _ = Printf.printf "%d : %d / %d (parray = %d) / %d - %d - %f%%\n" dbm.depth dbm.num_set (Hashtbl.length dbm.true_set) (Parray.length dbm.m) !(dbm.num_set_global) !(dbm.num_true_set_global) ((float_of_int !(dbm.num_true_set_global)) /. (float_of_int !(dbm.num_set_global)) *. 100.) in
    let _ = flush_all () in
    List.map (fun _ -> copy_one dbm) (Tools.range 1 n)

  let dimension dbm = dbm.dim

  let to_list dbm = Parray.to_list dbm.m

  let print fmt dbm =
    for l = 0 to (2*dbm.dim-1) do
      for c = 0 to (2*dbm.dim-1) do
        Format.fprintf fmt "%s " (B.to_string (get dbm (make_var l c)))
      done;
      Format.fprintf fmt "\n"
    done
end
*)