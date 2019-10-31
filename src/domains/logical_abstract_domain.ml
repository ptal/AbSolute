(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang
open Abstract_domain
open Interpretation

(** A logical abstract domain is an abstract domain that internalizes its connection to the logical formula.
    In practice, it often consists in a pair `(A.t, A.I.t)` of the abstract element and its interpretation.
    The function `interpret` allows us to easily add constraints, the variables being added automatically if needed. *)
module type Logical_abstract_domain =
sig
  include Abstract_domain

  (** Extract the interpretation structure. *)
  val interpretation: t -> I.t

  (** Interpret an existentially quantified logical formula into an abstract element and directly call `weak_incremental_closure` on this new constraint.
      Existentially quantified variables are added into the abstract element if not already present.
      /!\ All variables are supposed to have a different name, otherwise they are considered equal; this differs from the usual existential connector in logic.
      We return `None` if the constraint could not be interpreted.
      Raise `Not_found` if some free variables are not already in the abstract domain.
      Raise `Bot_found` if the constraint is detected unsatisfiable in constant time.
      See also `Interpretation_sig.interpret`. *)
  val qinterpret: t -> approx_kind -> Ast.qformula -> t option

  (** Simplified interface of `Abstract_domain.print`. *)
  val print': Format.formatter -> t -> unit
end
