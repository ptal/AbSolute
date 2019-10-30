(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Box_interpretation

module type Box_closure_sig = functor (R: Box_interpretation_sig) ->
sig
  module R : Box_interpretation_sig
  module Store = R.Store

  (** Perform the filtering of the constraint.
      Returns `(store, b)` where `store` is the resulting store and `b` is true if the constraint is entailed. *)
  val incremental_closure: Store.t -> R.rconstraint -> (Store.t * bool)
  val entailment: Store.t -> R.rconstraint -> Kleene.t

end with module R=R

module Make : Box_closure_sig
