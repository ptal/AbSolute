(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Signature of abstract domain.
    We suppose here that the constraints are encapsulated in the abstract element. *)

open Core
open Bounds
open Interpretation
open Lang

(** Exception raised whenever a failure is encountered.
    In addition to `Bot.Bot_found`, `Conflict n` forces the backtrack in the search tree to the level `n`. *)
exception Conflict of int

type ad_uid = int

(** A logical abstract domain is an abstract domain that internalizes its connection to the logical formula.
    In practice, it often consists in a pair `(A.t, A.I.t)` of the abstract element and its interpretation.
    The function `qinterpret` allows us to easily add constraints, the variables being added automatically if needed.
    `Logical_abstract_domain` is included in the signature of `Abstract_domain`.
    See also `Logical_wrapper`. *)
module type Logical_abstract_domain =
sig
  (** The representation of the variables and constraints inside the
     abstract element.  It allows users to turn a logic specification
     into an abstract element. *)
  module I: Interpretation_sig

  (** The type of the abstract domain. *)
  type t

  (** Read-only access to the interpretation structure. *)
  val interpretation: t -> I.t

  (** Allow to modify the interpretation.
      Be careful using this function as it delegates to you the work to keep the interpretation and abstract domain consistent. *)
  val map_interpretation: t -> (I.t -> I.t) -> t

  (** Interpret an existentially quantified logical formula into an abstract element and directly call `weak_incremental_closure` on this new constraint.
      Existentially quantified variables are added into the abstract element if not already present.
      /!\ All variables are supposed to have a different name, otherwise they are considered equal; this differs from the usual existential connector in logic.
      We return `None` if the constraint could not be interpreted.
      Raise `Not_found` if some free variables are not already in the abstract domain.
      Raise `Bot_found` if the constraint is detected unsatisfiable in constant time.
      See also `Interpretation_sig.interpret`. *)
  val qinterpret: t -> approx_kind -> Ast.qformula -> t option
end

module type Abstract_domain =
sig
  include Logical_abstract_domain

  (** The module of the bound handled by this abstract domain. *)
  module B: Bound_sig.S

  (** An empty abstract element identified by a unique identifier (UID).
      See also `uid`. *)
  val empty: ad_uid -> t

  (** Retrieve the UID of this abstract element.
      UIDs are mainly useful to perform event-based propagation and associate event and task to a specific abstract domain.
      See also `Transformers.Event_loop`. *)
  val uid: t -> ad_uid

  (** Extend the abstract element with a variable of the given type.
      The ID of the fresh variable is returned along with its abstract type.
      Raise `Wrong_modelling` if the abstract domain does not support variables
      of the given type. *)
  val extend: ?ty:Types.var_ty -> t -> (t * I.var_id * Types.var_abstract_ty)

  (** Project the lower and upper bounds of a single variable. *)
  val project: t -> I.var_id -> (B.t * B.t)

  (** A snapshot is a copy of the state of the abstract domain at some point in time. *)
  type snapshot

  (** [lazy_copy a n] creates a series of `n` snapshots of the current element `a`.
      For fully functional abstract domain, it is as easy as `List.init n (fun _ -> a)`.
      Postcondition: `a` must not be used anymore unless in `restore`.
      Internally, some informations can be shared by the different snapshots (until restored).
      This function is useful in a backtracking algorithm. *)
  val lazy_copy: t -> int -> snapshot list

  (** Restore the abstract element to the same state as when the snapshot was created. *)
  val restore: t -> snapshot -> t

  (** Closure of the abstract element: it tries to remove as much
     inconsistent values as possible from the abstract element
     according to the constraints encapsulated. *)
  val closure: t -> t

  (** Weak incremental closure add the constraint into the abstract
     element.  This operation is in constant time and must not perform
     any closure algorithm.  It can however raise `Bot_found` if the
     constraint is detected disentailed in constant time.
     Precondition: The variables in `c` must all belong to the abstract element. *)
  val weak_incremental_closure: t -> I.rconstraint -> t

  (** `entailment a c` returns `True` if the constraint `c` is
     entailed by the abstract element `a`.  Being entailed means that
     the constraint is redundant in comparison to the information
     already in `a`.  It returns `False` if adding `c` to `a` would
     make the element inconsistent.  If `c` can become either `True`
     or `False` in the future, then `Unknown` is returned. *)
  val entailment: t -> I.rconstraint -> Kleene.t

  (** Divide the abstract element into sub-elements.
      For exhaustiveness, the union of `split t` should be equal to `t`. *)
  val split: t -> snapshot list

  (** The volume is crucial to get information on the current state of
     the abstract element:
     - `volume t = 0` means that the current abstract element is failed.
     - `volume t = 1` (on integers) means that the current assignment is
                      satisfiable (note that `1` is exactly representable in
                                  a floating point number).
     - On float and rational, the notion of "satisfiability" depends on the
       expected precision of the abstract element. *)
  val volume: t -> float

  (** An element belongs to one category: failed, satisfiable and
     unknown.  Note that this function cannot be recovered from
     `volume` because `state_decomposition` can return satisfiable
     even if `volume t > 1`.
     Normally, `False` is never returned because `Bot_found` is raised
     in case of unsatisfiability in `incremental_closure` or `closure`. *)
  val state_decomposition: t -> Kleene.t

  (** Print the current element in the abstract element using the
      logical names of variables. *)
  val print: Format.formatter -> t -> unit
end

(** This module is just useful for `QInterpreter_base` below. *)
module type Small_abstract_domain =
sig
  type t
  module I: Interpretation_sig
  val interpretation: t -> I.t
  val map_interpretation: t -> (I.t -> I.t) -> t
  val extend: ?ty:Types.var_ty -> t -> (t * I.var_id * Types.var_abstract_ty)
  val weak_incremental_closure: t -> I.rconstraint -> t
end

(** `QInterpret_base` provides a default implementation of the function `qinterpret`.
    It can be included in the abstract domains. *)
module QInterpreter_base(A: Small_abstract_domain) :
sig
  val qinterpret: A.t -> approx_kind -> Ast.qformula -> A.t option
end
