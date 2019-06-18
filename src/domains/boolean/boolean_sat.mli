open Libsatml

module type Boolean_rep_sig =
sig
  type t
  type var_kind = unit
  type var_id = Solver.var
  type rconstraint = Lit.t Vec.t

  val empty: t

  (** Initialize the rewriter with the map between variable's name and store's index. *)
  val extend: t -> (Csp.var * var_id) -> t

  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id

  (** Transform the formula in CNF form.
      We do not rewrite arithmetic constraint yet. *)
  val rewrite: t -> bconstraint -> rconstraint list

  (** Currently the same than `rewrite`. *)
  val relax: t -> bconstraint -> rconstraint list

  (** Negate the constraint. *)
  val negate: rconstraint -> rconstraint
end

module Boolean_rep: Boolean_rep_sig

module type Boolean_sig =
sig
  type t
  module R = Boolean_rep
  module B = Bound_int

  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (I.B.t * I.B.t)
  val project_itv: t -> R.var_id -> itv
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> kleene
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> kleene
  val print: R.t -> Format.formatter -> t -> unit
end

module Boolean_sat : Boolean_sig
