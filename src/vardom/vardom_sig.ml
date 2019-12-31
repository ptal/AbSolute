(* Copyright 2019 Mathieu Vavrille, Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Generic signature for variable domains, called `vardom` here.
    Examples of vardom are intervals of integers, floats, rational, sets or BDD.
    When implementing your vardom, consider using `Vardom_factory` to factorize some code. *)

open Core
open Core.Bot
open Bounds
open Lang
open Typing
open Vardom_factory

(** Language of unary and binary operations that can be applied to `vardom`. *)
type unop_kind = NEG | ABS | NOT | PREF of int | SUFF of int
type binop_kind = ADD | SUB | MUL | POW | XOR | AND | OR

module type Vardom_sig = sig

  include Vardom_factory_sig

  (** Name of the Vardom.
      See also `Abstract_domain.name`. *)
  val name: string

  val type_of: unit -> Ad_type.vardom_ty

  (** Top element (the less precise element) of the Vardom.
      In case of a concrete type, a suited abstract representation is picked,
      otherwise, the abstract type must be exactly the one supported in this vardom.
      Raise `Wrong_modelling` if the vardom cannot represent the given type. *)
  val top: ?ty:Types.var_ty -> unit -> (t * Types.var_abstract_ty)

  (** Convert a vardom to a possibly over-approximated floating point range. *)
  val to_float_range : t -> float * float

  (** Convert a vardom to a possibly over-approximated rational range. *)
  val to_rational_range : t -> Bound_rat.t * Bound_rat.t

  (** Convert a vardom to a possibly over-approximated `bound` range. *)
  val to_range : t -> bound * bound

  (** Lower bound (smallest element in the vardom). *)
  val lb: t -> bound

  (** Upper bound (largest element in the vardom). *)
  val ub: t -> bound

  val print: Format.formatter -> t -> unit

  (** Convert the current values to its logical representation. *)
  val to_expr: t -> (Ast.cmpop * Ast.expr) * (Ast.cmpop * Ast.expr)

  (************************************************************************)
  (** {1 SET-THEORETIC } *)
  (************************************************************************)

  (** operations *)

  val join: t -> t -> t
  val meet: t -> t -> t bot

  (** predicates *)
  val contains_float: t -> float -> bool

  val is_singleton: t -> bool
  val to_bot: t -> t bot

  val equal : t -> t -> bool

  (** mesure *)
  val float_size: t -> float

  (** pruning *)
  val prune : (t -> t -> t list) option

  (************************************************************************)
  (** {1 INTERVAL ARITHMETICS (FORWARD EVALUATION)} *)
  (************************************************************************)

  (** return valid values (possibly Bot, if dividend is nul) *)
  val div: t -> t -> t bot

  (* Given an operation (unary or binary), return the operation on the vardom *)
  val unop: unop_kind -> t -> t
  val binop: binop_kind -> t -> t -> t

  (** function calls (sqrt, exp, ln ...) are handled here :
     given a function name and and a list of argument,
     it returns a possibly bottom result *)
  val eval_fun : string -> t list -> t bot

  (************************************************************************)
  (** {1 FILTERING (TEST TRANSFER FUNCTIONS)}                             *)
  (************************************************************************)

  (** given two interval arguments, return a subset of each argument
      by removing points that cannot satisfy the predicate;
      may also return Bot if no point can satisfy the predicate
      simplified interface since a > b <=> b < a *)

  val filter_leq: t -> t -> (t * t) bot
  val filter_lt : t -> t -> (t * t) bot
  val filter_eq : t -> t -> (t * t) bot
  val filter_neq: t -> t -> (t * t) bot

  (** given the interval argument(s) and the expected interval result of
     a numeric operation, returns refined interval argument(s) where
     points that cannot contribute to a value in the result are
     removed;
     may also return Bot if no point in an argument can lead to a
     point in the result *)

  val filter_div: t -> t -> t -> (t*t) bot

  val filter_unop: unop_kind -> t -> t -> t bot
  val filter_binop: binop_kind -> t -> t -> t -> (t*t) bot

  (** filtering function calls like (sqrt, exp, ln ...) is done here :
     given a function name, a list of argument, and a result,
     it remove points that cannot satisfy the relation : f(arg1,..,argn) = r;
     it returns a possibly bottom result *)
  val filter_fun: string -> t list -> t -> (t list) bot

  (** Only filters the first argument *)
  val filter_div_f: t -> t -> t -> t bot
  val filter_root_f: t -> t -> t -> t bot

  val filter_binop_f: binop_kind -> t -> t -> t -> t bot
end

module type Vardom_functor = functor (B: Bound_sig.S) -> Vardom_sig with module B=B
