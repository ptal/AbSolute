open Core
open Dbm
open Octagon_representation

module Octagon_representation = Octagon_representation
module Dbm = Dbm
module Octagon_split = Octagon_split
module Closure = Closure

module type Octagon_sig =
sig
  module DBM : DBM_sig
  module B = DBM.B
  module R : Octagon_rep_sig with module B=B
  type t

  (** Create an empty octagon. *)
  val empty: t

  (** Extend the DBM with an additional variable. *)
  val extend: t -> R.var_kind -> (t * R.var_id)

  (** Give an interval view of two DBM variables. *)
  val project: t -> R.var_id -> (B.t * B.t)

  (** See `Abstract_domain.lazy_copy` *)
  val lazy_copy: t -> int -> t list

  (** See `Abstract_domain.copy` *)
  val copy: t -> t

  (** Perform the closure of the DBM. *)
  val closure: t -> t

  (** Perform the incremental closure of the DBM with the constraint. *)
  val incremental_closure: t -> R.rconstraint -> t

  (** Add the octagonal constraint in the octagon, if it is not entailed and without closing the DBM.
      It throws `Bot_found` if the constraint is disentailed (see `entailment`). *)
  val weak_incremental_closure: t -> R.rconstraint -> t

  (** Given an octagonal constraint, return `True` if it is entailed by the octagon, `False` if it is disentailed, and `Unknown` if it be entailed or disentailed in the future. *)
  val entailment: t -> R.rconstraint -> Kleene.t

  (** Same as `entailment` but in addition: add the constraint in the octagon, propagate and returns `False` if it is inconsistent. *)
  val strong_entailment: t -> R.rconstraint -> Kleene.t

  (** See `Abstract_domain.split` *)
  val split: t -> t list

  val volume: t -> float

  val state_decomposition: t -> Kleene.t

  val print: R.t -> Format.formatter -> t -> unit

  (** Low-level access to the DBM. *)
  val unwrap: t -> DBM.t
end

module OctagonZ(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig
module OctagonQ(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig
module OctagonF(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig

module Make
  (Closure: Closure.Closure_sig)
  (SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig
