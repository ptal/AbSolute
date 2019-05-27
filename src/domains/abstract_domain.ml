(** Signature of abstract domain.
    It differs from `Adcp_sig` because we suppose here that the constraints are encapsulated in the abstract element. *)

type kleene = False | True | Unknown

let rec and_kleene x y =
  match x, y with
  | False, _ -> False
  | True, True -> True
  | True, Unknown -> Unknown
  | Unknown, Unknown -> Unknown
  | _ -> and_kleene y x

(* `conjunction` is the result of the entailment of a conjunction in a reified context.
   It returns the entailment status of the conjunction, with an optional index representing the only `unknown` value, if any.
   See `box_reified` for an example. *)
let and_reified conjunction =
  let (n, t, f, u) =
    List.fold_left (fun (n, t, f, u) -> function
      | True -> (n+1, t+1, f, u)
      | False -> (n+1, t, f+1, u)
      | Unknown -> (n+1, t, f, n::u)) (0,0,0,[]) conjunction in
  if f > 0 then (False, None)
  else if t = n then (True, None)
  else if (List.length u) = 1 then (Unknown, Some(List.hd u))
  else (Unknown, None)

(* This exception is raised when a constraint is passed to an abstract domain that cannot represent this constraint. *)
exception Wrong_modelling of string

(** Every abstract domain has a different variable and constraint representation according to their internal implementation.
    We ask every abstract domain to provide a representation module in order to connect the logic specification (`Csp.bconstraint`) and the representation of the abstract domain.
    This module can also rewrite a logic constraint into a more suited representation of the abstract domain. *)
module type Representation_sig =
sig
  type t
  (* Variable ID as represented in the abstract domain. *)
  type rvar
  (* Constraint representation in the abstract domain. *)
  type rconstraint

  (** Initialize the rewriter with the map between logical variables and abstract domain's variables. *)
  val init: (Csp.var * rvar) list -> t

  val to_logic_var: t -> rvar -> Csp.var
  val to_abstract_var: t -> Csp.var -> rvar

  (** Rewrite a logic constraint into an abstract constraint. *)
  val rewrite: t -> Csp.bconstraint -> rconstraint list

  (** Same as `rewrite` but the obtained constraints can over-approximate the set of initial constraints (the set of solutions might be greater with the obtained constraints). *)
  val relax: t -> Csp.bconstraint -> rconstraint list

  (** Negate the constraint.
      The negated constraint can be an over-approximation of the initial constraint. *)
  val negate: rconstraint -> rconstraint
end

module type Abstract_domain =
sig
  (** The module of the bound handled by this abstract domain. *)
  module B: Bound_sig.BOUND

  (** The representation of the variables and constraints inside the abstract domain.
      It allows users to turn a logic specification into an abstract domain. *)
  module R: Representation_sig

  (** The type of the abstract domain. *)
  type t

  (** Project the lower and upper bounds of a single variable. *)
  val project_one: t -> R.rvar -> (B.t * B.t)

  (** Project the lower and upper bounds of all the variables in `vars`. *)
  val project: t -> R.rvar list -> (R.rvar * (B.t * B.t)) list

  (** Closure of the abstract domain: it tries to remove as much inconsistent values as possible from the abstract element according to the constraints encapsulated. *)
  val closure: t -> t

  (** Weak incremental closure add the constraint into the abstract domain.
      This operation is in constant time and must not perform any closure algorithm.
      It can however raise `Bot_found` if the constraint is detected disentailed in constant time. *)
  val weak_incremental_closure: t -> R.rconstraint -> t

  (** Divide the abstract element into sub-elements.
      For exhaustiveness, the union of `split t` should be equal to `t`. *)
  val split: t -> t list

  (** The volume is crucial to get information on the current state of the abstract element:
        - `volume t = 0` means that the current abstract element is failed.
        - `volume t = 1` (on integers) means that the current assignment is satisfiable (note that `1` is exactly representable in a floating point number).
        - On float and rational, the notion of "satisfiability" depends on the expected precision of the abstract element. *)
  val volume: t -> float

  (** An element belongs to one category: failed, satisfiable and unknown.
      Note that this function cannot be recovered from `volume` because `state_decomposition` can return satisfiable even if `volume t > 1` on Z or a given precision on F and Q. *)
  val state_decomposition: t -> kleene
end

(** This is a default abstract representation in case the abstract domain directly manipulates the logic specification. *)
module Logical_representation =
struct
  type rvar = Csp.var
  type rconstraint = Csp.bconstraint
  type t = ()

  let init _ : t = ()
  let to_logic_var _ v = v
  let to_abstract_var _ v = v
  let rewrite _ c = [c]
  let relax _ c = [c]
  let negate = Csp.neg_bconstraint
end

