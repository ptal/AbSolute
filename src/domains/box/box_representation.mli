open Lang.Ast
open Vardom
open Var_store

module type Box_rep_sig =
sig
  module Vardom: Vardom_sig.Vardom_sig

  (** Box_rep depends on the store to represent the `var_id`.
      However, the store is contained in `Box` itself and not here.
      NOTE: We could parametrize Box_rep_sig with a `Store` if we have different kind of stores in the future. *)
  module Store: Var_store_sig with module V=Vardom

  type t
  type var_kind = unit
  type var_id = Store.key
  type var_dom = Store.cell

  (** We annotate each node of this expression with its interval evaluation.
    This is useful for the HC4 algorithm.
    The `value` field is never backtracked, it is only useful to propagate inside on node of the search tree. *)
  type rexpr = {
    node: node;
    mutable value: Vardom.t
  }
  and node =
    | BFuncall of string * rexpr list
    | BUnary   of unop * rexpr
    | BBinary  of rexpr * binop * rexpr
    | BVar     of var_id
    | BCst     of Vardom.t

  type rconstraint = rexpr * cmpop * rexpr

  val empty: t

  (** Initialize the rewriter with the map between variable's name and store's index. *)
  val extend: t -> (var * var_id) -> t

  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id

  (** Simple rewriting: substitute the variable names by their respective indices. *)
  val rewrite: t -> formula -> rconstraint list

  (** Currently the same than `rewrite`. *)
  val relax: t -> formula -> rconstraint list

  (** Negate the constraint. *)
  val negate: rconstraint -> rconstraint

  (** Convert an "abstract" constraint to its logical equivalent. *)
  val to_logic_constraint: t -> rconstraint -> bconstraint

  (** Create an expression from a node.
      The vardom ref is initialized to TOP. *)
  val make_expr: node -> rexpr

  (** List of variables occuring in the constraint (duplicates possible). *)
  val vars_of_constraint: rconstraint -> var_id list
end

module type Box_rep_functor = functor (Vardom: Vardom_sig.Vardom_sig) -> Box_rep_sig
  with module Vardom=Vardom

module Box_rep: Box_rep_functor
