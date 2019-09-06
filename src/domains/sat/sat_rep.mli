open Lang.Ast

module type Sat_rep_sig =
sig
  type t
  type var_kind = unit
  type var_id = Minisatml.Solver.var
  type rconstraint = Minisatml.Types.Lit.lit Minisatml.Vec.t

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
end

module Sat_rep: Sat_rep_sig
