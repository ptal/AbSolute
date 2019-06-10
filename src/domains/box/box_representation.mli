open Csp

type box_var = int
type box_constraint = box_var gbconstraint
type box_expr = box_var gexpr

module type Box_rep_sig =
sig
  type t
  type var_kind = unit
  type var_id = box_var
  type rconstraint = box_constraint

  val empty: t

  (** Initialize the rewriter with the map between variable's name and store's index. *)
  val extend: t -> (Csp.var * var_id) -> t

  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id

  (** Simple rewriting: substitute the variable names by their respective indices. *)
  val rewrite: t -> bconstraint -> rconstraint list

  (** Currently the same than `rewrite`. *)
  val relax: t -> bconstraint -> rconstraint list

  (** Negate the constraint. *)
  val negate: rconstraint -> rconstraint
end

module Box_rep: Box_rep_sig
