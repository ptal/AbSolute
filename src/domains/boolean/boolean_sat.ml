open Satml

module type Boolean_sat_sig =
sig
  type t

  module R = Boolean_rep

  (** Boolean are representable on integers.
      NOTE: We use `Bound_int` instead of introducing a new `Bound_bool`. *)
  module B = Bound_int

  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (B.t * B.t)
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> Kleene.t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> Kleene.t
  val print: R.t -> Format.formatter -> t -> unit
end

module Boolean_sat =
struct
  (* Unfortunately, minisatml relies on a global variable.
     We should update it later. *)
  type t = {
    (* The depth is necessary to backtrack the state of minisat, it corresponds to the decision level. *)
    depth: int;
  }

  module R = Boolean_rep

  (** Boolean are representable on integers.
      NOTE: We use `Bound_int` instead of introducing a new `Bound_bool`. *)
  module B = Bound_int

  let empty = { depth=0 }

  (* Minisatml does not use functional structure for automatic backtracking, so we trigger the backtracking manually.
     For safety, we guard all public functions of this module with `backtrack_state`.
     It avoids creating a new function such as `backtrack` only for this abstract domain.
     If later, we notice that such a function is useful for other domain, we might add it to `Abstract_domain`. *)
  let backtrack_state b =
    if Solver.decisionLevel () <> b.depth then
      Solver.cancelUntil b.depth

  let extend b () =
    backtrack_state b;
    (b, Solver.newVar ())

  let project b v =
    backtrack_state b;
    match Solver.value v with
    | LTrue -> (B.one, B.one)
    | LFalse -> (B.zero, B.zero)
    | LUndef -> (B.zero, B.one)

  let lazy_copy b n =
    backtrack_state b;
    Solver.newDecisionLevel ();
    let b = {depth=Solver.decisionLevel ()} in
    List.init n (fun _ -> b)

  let copy b = failwith "`copy` is not supported on `Boolean_sat`."

  (* The learnt clause are stored in the watched literals array.
     Actually, there is no explicit "learnt clause database". *)
  let learn_clause clause =
    let clause = Clause.clause_new (Vec.get_data clause) (Vec.size clause) ~learnt:true in
    attachClause clause;
    uncheckedEnqueue_clause (Vec.get clause 0) clause;
    varDecayActivity ()

  (* This closure extracts the propagation and conflict resolution parts from `Solver.search` in minisatml. *)
  let closure b =
    backtrack_state b;
    let conflict_clause = ref (Solver.propagate ()) in
    (* In case of a conflict, we compute the backjump point and register the learnt clause. *)
    if !conflict_clause <> Solver.dummy_clause then
      if decisionLevel () = 0 then raise Bot.Bot_found;
      let learnt_clause = Vec.init 0 dummy_lit in
      let backtrack_level = ref 0 in
      analyze conflict_clause learnt_clause backtrack_level;
      learn_clause learnt_clause;
      raise (Conflict !backtrack_level)
    end
    else begin
      (* No Conflict *)
      let next = ref dummy_lit in
      begin
        try
          while decisionLevel() < Vec.size env.assumptions do
            (* Perform user provided assumption. *)
            let p = Vec.get env.assumptions (decisionLevel ()) in
            if value p = Lbool.LTrue then
              (* Dummy decision level: *)
              newDecisionLevel ()
            else if value p = Lbool.LFalse then begin
              analyzeFinal (Lit.tild p) env.conflict;
              raise (Search Lbool.LFalse)
            end else begin
              next := p;
              raise Break
            end
          done
        with Break -> ()
        end;
        if !next = dummy_lit then begin
          (* New variable decision: *)
          env.decisions <- env.decisions + 1;
          next := pickBranchLit env.polarity_mode env.random_var_freq;
          if !next = dummy_lit then
            (* Model found *)
            raise (Search Lbool.LTrue)
      end;
      (* Increase decision level and enqueue next *)
      assert (value !next = Lbool.LUndef);
      newDecisionLevel ();

      uncheckedEnqueue !next;
    end
  done;
  Lbool.LUndef
with Search b -> b

  let weak_incremental_closure = : t -> R.rconstraint -> t
    backtrack_state b;

  let entailment = : t -> R.rconstraint -> Kleene.t
    backtrack_state b;

  let split = : t -> t list
    backtrack_state b;

  let volume = : t -> float
    backtrack_state b;

  let state_decomposition = : t -> Kleene.t
    backtrack_state b;

  let print = : R.t -> Format.formatter -> t -> unit
    backtrack_state b;
end
