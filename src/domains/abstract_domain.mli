(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Signatures of abstract domain and some extensions.
    We suppose here that the constraints are encapsulated in the abstract element. *)

open Core
open Bounds
open Interpretation
open Typing
open Typing.Ad_type

(** Exception raised whenever a failure is encountered.
    In addition to `Bot.Bot_found`, `Conflict n` forces the backtrack in the search tree to the level `n`. *)
exception Conflict of int

(** We require that abstract domains internalize their connection to the logical formula.
    In practice, it often consists in a pair `(A.t, A.I.t)` of the abstract element and its interpretation.
    The function `interpret` allows us to easily add constraints, the variables being added automatically if needed.
    See also `Interpretation.Interpretation_base`. *)
module type Abstract_domain =
sig
  (** The representation of the variables and constraints inside the
   abstract element.  It allows users to turn a logic specification
   into an abstract element. *)
  module I: Interpretation_sig

  (** The type of the abstract domain. *)
  type t

  (** The module of the bound handled by this abstract domain. *)
  module B: Bound_sig.S

  (** An empty abstract element identified by a unique identifier (UID).
      See also `uid`. *)
  val empty: ad_uid -> t

  (** Retrieve the UID of this abstract element.
      For instance, UIDs are useful to perform event-based propagation and to associate event and task to a specific abstract element.
      See also `Transformers.Event_loop`. *)
  val uid: t -> ad_uid

  (** Name of the abstract domain.
      This is particularly useful to print `Wrong_modelling` messages. *)
  val name: string

  (** Synthesize the type of the current abstract element.
      This function is useful once you created an abstract element, and wish to retrieve a symbolic representation of this element.
      See also [Typing.Infer].
      `None` if the abstract element does not have a "useful type" (for instance if it cannot represent constraints, e.g. `Event_loop`). *)
  val type_of: t -> ad_ty option

  (** Read-only access to the interpretation structure. *)
  val interpretation: t -> I.t

  (** Allow to modify the interpretation.
      Be careful using this function as it delegates to you the work to keep the interpretation and abstract domain consistent. *)
  val map_interpretation: t -> (I.t -> I.t) -> t

  (** Interpret an existentially quantified logical formula into an abstract element.
        1. Existentially quantified variables are added into the abstract element if not already present.
        2. Constraints are transformed into `rconstraint` and can be added into the abstract element with `weak_incremental_closure`.

      Usage:
        It should be considered as an incremental interpretation function.
        For instance, if you interpret `∃x∃y.x < y` first, and then interpret `∃y∃z.y = z`, it is equivalent to: `∃x∃y∃z.x < y /\ y = z`.
        You can add a variable without constraints with `∃x.true`; use `I.to_logic_variable` to retrieve the `var_id` and other information about "x".

      Exception:
        Raise `Wrong_modelling` if:
          1. the constraint could not be interpreted, or
          2. some free variables are not already in the abstract element.
        Raise `Bot_found` if the constraint is detected unsatisfiable (this is optional).

      Note:
        All variables are supposed to have a different name, otherwise they are considered equal; this differs from the usual existential connector in logic.

      See also `Interpretation_sig.interpret`. *)
  val interpret: t -> approx_kind -> Tast.tqformula -> (t * I.rconstraint list)

  (** Project the lower and upper bounds of a single variable.
      Raise `Wrong_modelling` if the variable is not in the current abstract element. *)
  val project: t -> I.var_id -> (B.t * B.t)

  (** A snapshot is a copy of the state of the abstract domain at some point in time.
      For purely functional abstract domains, it will be equal to `t`. *)
  type snapshot

  (** [lazy_copy a n] creates a series of `n` snapshots of the current element `a`.
      For purely functional abstract domain, it is as easy as `List.init n (fun _ -> a)`.
      Postcondition: `a` must not be used anymore unless in `restore`.
      Internally, some informations can be shared by the different snapshots (until restored).
      This function is useful in a backtracking algorithm. *)
  val lazy_copy: t -> int -> snapshot list

  (** Restore the abstract element to the same state as when the snapshot was created. *)
  val restore: t -> snapshot -> t

  (** Closure of the abstract element: it tries to remove as much inconsistent values as possible from the abstract element according to the encapsulated constraints (added through `weak_incremental_closure`).
      Returns `true` if a change on this abstract domain (or subdomain) occur, `false` otherwise (in this case `closure(t)=t`). *)
  val closure: t -> (t * bool)

  (** Weak incremental closure add the constraint into the abstract element.
      This operation should be of low time complexity.
      Raise `Bot_found` if the constraint is detected unsatisfiable (this is optional).
      Precondition: The variables in the constraint must all belong to the abstract element. *)
  val weak_incremental_closure: t -> I.rconstraint -> t

  (** [entailment a c] returns is `true` if the constraint `c` is entailed by the abstract element `a`.
      Being entailed means that the constraint is redundant in comparison to the information already in `a`.
      To test for disentailment, you must call `entailment` on the negation of the constraint.
      It is not possible to return a `Kleene` value due to over-approximation of constraint.
      Indeed, `c` might be an over-approximation of its logical constraint `l`, and therefore the disentailment of `c` does not imply the disentailment of `l`.
      This is explained more formally in the paper "Combining Constraint Languages via Abstract Interpretation" (Talbot and al., 2019). *)
  val entailment: t -> I.rconstraint -> bool

  (** Divide the abstract element into sub-elements.
      For exhaustiveness, the union of `split a` should be equal to `a`.
      The list is empty if the abstract element cannot be split (either because `state a != unknown` or because it does not provide a split operator). *)
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

  (** An element belongs to one category: failed, satisfiable and unknown.
      Note that this function cannot be recovered from `volume` because `state` can return satisfiable even if `volume t > 1`.
      Normally, `False` is never returned because `Bot_found` is raised in case of unsatisfiability in `weak_incremental_closure` or `closure`. *)
  val state: t -> Kleene.t

  (** Print the current element in the abstract element using the
      logical names of variables. *)
  val print: Format.formatter -> t -> unit
end

(*
(** This module is just useful for `QInterpreter_base` below. *)
module type Small_abstract_domain =
sig
  type t
  module I: Interpretation_sig
  val name: string
  val interpretation: t -> I.t
  val map_interpretation: t -> (I.t -> I.t) -> t
  val extend: ?ty:Types.var_ty -> t -> (t * I.var_id * Types.var_abstract_ty)
  val weak_incremental_closure: t -> I.rconstraint -> t
end

(** `QInterpret_base` provides a default implementation of the function `qinterpret`.
    It can be included in the abstract domains. *)
module QInterpreter_base(A: Small_abstract_domain) :
sig
  (** Extends the abstract element with a fresh variable.
      Returns `true` if the variable has been added, and `false` if it was already present. *)
  val extend_var: A.t -> (Ast.var * Types.var_ty) -> A.t * bool
  val qinterpret: A.t -> approx_kind -> Tast.tqformula -> A.t
end
*)