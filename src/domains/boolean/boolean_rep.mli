open Csp

module type Boolean_rep_sig =
sig
  type t
  type var_kind = unit
  type var_id = Libsatml.Solver.var
  type rconstraint = Libsatml.Types.Lit.lit Libsatml.Vec.t

  val empty: t

  (** Initialize the rewriter with the map between variable's name and store's index. *)
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id

  (** Simple rewriting: substitute the variable names by their respective indices. *)
  val rewrite: t -> bformula -> rconstraint list

  (** Currently the same than `rewrite`. *)
  val relax: t -> bformula -> rconstraint list

  (** Negate the constraint. *)
  val negate: rconstraint -> rconstraint
end

module Boolean_rep: Boolean_rep_sig
