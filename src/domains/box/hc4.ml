open Csp
open Bot
open Box_representation
open Kleene

module type Box_closure_sig = functor (R: Box_rep_sig) ->
sig
  module R: Box_rep_sig
  module Store = R.Store
  val incremental_closure: Store.t -> R.rconstraint -> Store.t * bool
  val entailment: Store.t -> R.rconstraint -> Kleene.t
end with module R=R

module Make(R: Box_rep_sig) =
struct
  module R = R
  module Store = R.Store
  module V = Store.V

  let expr_val expr = R.(expr.value)
  let exprs_val exprs = List.map expr_val exprs

  (* I. Evaluation part

     First step of the HC4-revise algorithm: it computes the intervals for each node of the expression.
     For example: given `x + 3` with `x in [1..3]`, it annotates `+` with `[4..6]`.
     It stores this new valued expression tree (`node`) into `rexpr.value`.

     This function is also useful for testing transfer functions errors (e.g. division by zero).
     - We raise Bot_found in case the expression only evaluates to error values.
     - Otherwise, we return only the non-error values.
   *)
  let rec eval store expr =
    let open R in
    match expr.node with
    | BFuncall(name, args) ->
      begin
        List.iter (eval store) args;
        let r = debot (V.eval_fun name (exprs_val args)) in
        expr.value <- r
      end
    | BVar v -> expr.value <- R.Store.get store v
    | BCst v -> expr.value <- v
    | BUnary (o,e1) ->
      begin
        eval store e1;
        match o with
        | NEG -> expr.value <- V.unop V.NEG (expr_val e1)
      end
    | BBinary (o,e1,e2) ->
      begin
        eval store e1;
        eval store e2;
        let v1 = expr_val e1 and v2 = expr_val e2 in
        let v = match o with
        | ADD -> V.binop V.ADD v1 v2
        | SUB -> V.binop V.SUB v1 v2
        | DIV -> debot (V.div v1 v2)
        | MUL ->
          let r = V.binop V.MUL v1 v2 in
          if V.equal v1 v2 then
            (* special case: squares are positive *)
            V.unop V.ABS r
          else r
        | POW -> V.binop V.POW v1 v2 in
        expr.value <- v
      end

  (* II. Refine part

     Second step of the HC4-revise algorithm.
     It propagates the intervals from the root of the expression tree `e` to the leaves.
     For example: Given `y = x + 3`, `x in [1..3]`, `y in [1..5]`.
                  Then after `eval` we know that the node at `+` has the interval `[4..6]`.
                  Therefore we can intersect `y` with `[4..6]` due to the equality.
     Note that we can call again `eval` to restrain further `+`, and then another round of `refine` will restrain `x` as well.
     We raise `Bot_found` in case of unsatisfiability.

     NOTE: This step is functional: it does not modify the `rexpr.value` field.
   *)

  (* refines binary operator to handle constants *)
  let refine_bop f1 f2 e1 e2 x (b:bool) =
    let open R in
    match e1.node, e2.node, b with
    | BCst _, BCst _, _ -> Nb (e1.value, e2.value)
    | BCst _, _, true -> merge_bot2 (Nb e1.value) (f2 e2.value e1.value x)
    | BCst _, _, false -> merge_bot2 (Nb e1.value) (f2 e2.value x e1.value)
    | _, BCst _, _ -> merge_bot2 (f1 e1.value e2.value x) (Nb e2.value)
    | _, _, true -> merge_bot2 (f1 e1.value e2.value x) (f2 e2.value e1.value x)
    | _, _, false -> merge_bot2 (f1 e1.value e2.value x) (f2 e2.value x e1.value)

  (* u + v = r => u = r - v /\ v = r - u *)
  let refine_add u v r =
    refine_bop (V.filter_binop_f V.ADD) (V.filter_binop_f V.ADD) u v r true

  (* u - v = r => u = r + v /\ v = u - r *)
  let refine_sub u v r =
    refine_bop (V.filter_binop_f V.SUB) (V.filter_binop_f V.ADD) u v r false

  (* u * v = r => (u = r/v \/ v=r=0) /\ (v = r/u \/ u=r=0) *)
  let refine_mul u v r =
    refine_bop (V.filter_binop_f V.MUL) (V.filter_binop_f V.MUL) u v r true

  (* u / v = r => u = r * v /\ (v = u/r \/ u=r=0) *)
  let refine_div u v r =
    refine_bop V.filter_div_f (V.filter_binop_f V.MUL) u v r false

  let rec refine store root expr =
    let open R in
    match expr with
    | BFuncall(name,args) ->
       let res = V.filter_fun name (List.map (fun e -> R.(e.value)) args) root in
       List.fold_left2 (fun acc res e -> refine acc res e.node) store (debot res) args
    | BVar v -> R.Store.set store v root
    | BCst i -> ignore (debot (V.meet root i)); store
    | BUnary (op,e) ->
       let j = match op with
         | NEG -> V.filter_unop V.NEG e.value root
       in refine store (debot j) e.node
    | BBinary (o,e1,e2) ->
       let j = match o with
         | ADD -> refine_add e1 e2 root
         | SUB -> refine_sub e1 e2 root
         | MUL -> refine_mul e1 e2 root
         | DIV -> refine_div e1 e2 root
         | POW -> V.filter_binop V.POW e1.value e2.value root
       in
       let j1,j2 = debot j in
       refine (refine store j1 e1.node) j2 e2.node

  (* III. HC4-revise algorithm (combining eval and refine).

     Apply the evaluation followed by the refine step of the HC4-revise algorithm.
     It prunes the domain of the variables in `store` according to the constraint `e1 o e2`.
  *)
  let hc4_revise store (e1,op,e2) =
    let i1,i2 = expr_val e1, expr_val e2 in
    let j1,j2 = match op with
      | LT  -> debot (V.filter_lt i1 i2)
      | LEQ -> debot (V.filter_leq i1 i2)
      (* a > b <=> b < a*)
      | GEQ -> let j2,j1 = debot (V.filter_leq i2 i1) in (j1,j2)
      | GT  -> let j2,j1 = debot (V.filter_lt i2 i1) in (j1,j2)
      | NEQ -> debot (V.filter_neq i1 i2)
      | EQ  -> debot (V.filter_eq i1 i2)
    in
    let refined_store = if V.equal j1 i1 then store else refine store j1 R.(e1.node) in
    if j2 = i2 then refined_store else refine refined_store j2 R.(e2.node)

  let hc4_eval_revise store (e1,op,e2) =
  begin
    eval store e1;
    eval store e2;
    let store = hc4_revise store (e1,op,e2) in
    try
      ignore(hc4_revise store (e1,neg op,e2));
      store, false
    with Bot_found -> store, true
  end

  let incremental_closure store c = hc4_eval_revise store c

  let entailment store (e1,op,e2) =
    try
      eval store e1;
      eval store e2;
      ignore(hc4_revise store (e1,op,e2));
      try
        ignore(hc4_revise store (e1,neg op,e2));
        Unknown
      with
      | Bot_found -> True
    with
    | Bot_found -> False
end
