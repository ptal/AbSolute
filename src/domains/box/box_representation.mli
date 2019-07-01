open Csp

type box_var = int

module type Box_rep_functor = functor (Vardom: Vardom_sig) ->
sig
  type t
  type var_kind = unit
  type var_id = box_var

  (** We annotate each node of this expression with its interval evaluation.
    This is useful for the HC4 algorithm.
    The `Vardom.t ref` field is never backtracked, it is only useful to propagate inside on node of the search tree. *)
  type rexpr = node * Vardom.t ref
  and node =
    | BFuncall of string * node list
    | BUnary   of unop * node
    | BBinary  of binop * node * node
    | BVar     of var_id
    | BCst     of Vardom.t

  type rconstraint = rexpr * cmpop * rexpr

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

module Box_rep: Box_rep_functor
