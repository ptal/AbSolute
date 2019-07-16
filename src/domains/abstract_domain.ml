(** Signature of abstract domain.
    It differs from `Adcp_sig` because we suppose here that the constraints are encapsulated in the abstract element. *)


(** Every abstract domain has a different variable and constraint representation according to their internal implementation.
    We ask every abstract domain to provide a representation module in order to connect the logic specification (`Csp.bformula`) and the representation of the abstract domain.
    This module can also rewrite a logic constraint into a more suited representation of the abstract domain. *)
module type Representation_sig =
sig
  type t

  (** Kind of the variable in the abstract domain. *)
  type var_kind

  (** Variable ID as represented in the abstract domain. *)
  type var_id

  (** Constraint representation in the abstract domain. *)
  type rconstraint

  (** An empty representation. *)
  val empty: t

  (** Add a mapping between a logical variable and its representation in the abstract domain. *)
  val extend: t -> (Csp.var * var_id) -> t

  val to_logic_var: t -> var_id -> Csp.var
  val to_abstract_var: t -> Csp.var -> var_id

  (** Rewrite a logic constraint into an abstract constraint. *)
  val rewrite: t -> Csp.bformula -> rconstraint list

  (** Same as `rewrite` but the obtained constraints can over-approximate the set of initial constraints (the set of solutions might be greater with the obtained constraints). *)
  val relax: t -> Csp.bformula -> rconstraint list

  (** Negate the constraint.
      The negated constraint can be an over-approximation of the initial constraint. *)
  val negate: rconstraint -> rconstraint
end

(** Exception raised whenever a failure is encountered.
    In addition to `Bot.Bot_found`, `Conflict n` forces the backtrack in the search tree to the level `n`. *)
exception Conflict of int

module type Abstract_domain =
sig
  (** The module of the bound handled by this abstract domain. *)
  module B: Bound_sig.BOUND

  (** The representation of the variables and constraints inside the abstract domain.
      It allows users to turn a logic specification into an abstract domain. *)
  module R: Representation_sig

  (** The type of the abstract domain. *)
  type t

  (** An empty abstract domain. *)
  val empty: t

  (** Extend the abstract domain with a variable.
      An abstract domain can handle variables of different kind, the meaning of "kind" is proper to the abstract domain. *)
  val extend: t -> R.var_kind -> (t * R.var_id)

  (** Project the lower and upper bounds of a single variable. *)
  val project: t -> R.var_id -> (B.t * B.t)

  (** `lazy_copy a n` creates `n` copies of the element `a` with the assumption that this one will not be used anymore, thus it might be returned in the copied list.
      Internally, some informations can be shared by the different copies (until they are modified).
      This function is useful in a backtracking algorithm. *)
  val lazy_copy: t -> int -> t list

  (** `copy a` copies the element `a`.
      This function is useful in "probe" algorithm where we perform computations that we do not want to impact the current node. *)
  val copy: t -> t

  (** Closure of the abstract domain: it tries to remove as much inconsistent values as possible from the abstract element according to the constraints encapsulated. *)
  val closure: t -> t

  (** Weak incremental closure add the constraint into the abstract domain.
      This operation is in constant time and must not perform any closure algorithm.
      It can however raise `Bot_found` if the constraint is detected disentailed in constant time. *)
  val weak_incremental_closure: t -> R.rconstraint -> t

  (** `entailment a c` returns `True` if the constraint `c` is entailed by the abstract domain `a`.
      Being entailed means that the constraint is redundant in comparison to the information already in `a`.
      It returns `False` if adding `c` to `a` would make the element inconsistant.
      If `c` can become either `True` or `False` in the future, then `Unknown` is returned. *)
  val entailment: t -> R.rconstraint -> Kleene.t

  (** Divide the abstract element into sub-elements.
      For exhaustiveness, the union of `split t` should be equal to `t`. *)
  val split: t -> t list

  (** The volume is crucial to get information on the current state of the abstract element:
        - `volume t = 0` means that the current abstract element is failed.
        - `volume t = 1` (on integers) means that the current assignment is satisfiable (note that `1` is exactly representable in a floating point number).
        - On float and rational, the notion of "satisfiability" depends on the expected precision of the abstract element. *)
  val volume: t -> float

  (** An element belongs to one category: failed, satisfiable and unknown.
      Note that this function cannot be recovered from `volume` because `state_decomposition` can return satisfiable even if `volume t > 1` on integers or a given precision on floating numbers and rational. *)
  val state_decomposition: t -> Kleene.t

  (** Print the current element in the abstract domain using the initial names of variables. *)
  val print: R.t -> Format.formatter -> t -> unit
end
