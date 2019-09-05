open Ast
open Bounds

let is_zero = Bound_rat.equal Bound_rat.zero
let is_neg c = Bound_rat.sign c = -1

let apply f e1 e2 b op =
  let (e1', b1) = f e1 b in
  let (e2', b2) = f e2 b in
  (Binary (e1', op, e2'), b1 || b2)

let inv = function
  | EQ  -> EQ
  | LEQ -> GEQ
  | GEQ -> LEQ
  | NEQ -> NEQ
  | GT  -> LT
  | LT  -> GT

let neg = function
  | EQ  -> NEQ
  | LEQ -> GT
  | GEQ -> LT
  | NEQ -> EQ
  | GT  -> LEQ
  | LT  -> GEQ

let rec distribute (op, c) = function
  | Funcall (name, args) ->  Binary (Funcall(name, args), op, c)
  | Cst (a,annt) -> Binary (Cst (a,annt), op, c)
  | Var v -> Binary (Var v, op, c)
  | Unary (NEG, e) -> Unary (NEG, distribute (op, c) e)
  | Binary (_, binop, _) as expr when binop = POW -> Binary (expr, op, c)
  | Binary (e, binop, Cst (a,annt)) when binop = op -> Binary (e, op, Binary (Cst (a,annt), MUL, c))
  | Binary (Cst (a,annt), binop, e) when binop = op -> Binary (Binary (Cst (a,annt), op, c), op, e)
  | Binary (Cst (a,annt), (DIV|MUL as binop), e) -> Binary (Binary (Cst (a,annt), op, c), binop, e)
  | Binary (e, DIV, Cst (a,annt)) -> Binary (e, op, Binary (c, DIV, Cst (a,annt)))
  | Binary (e, MUL, Cst (a,annt)) -> Binary (e, MUL, Binary (Cst (a,annt), op, c))
  | Binary (e1, binop, e2) -> Binary (distribute (op, c) e1, binop, distribute (op, c) e2)

let rec expand = function
  | Funcall (name, args) -> Funcall (name, args)
  | Cst (c,ant) -> Cst (c,ant)
  | Var v -> Var v
  | Unary (unop, e) -> Unary (unop, expand e)
  | Binary (Cst (c,ant), MUL, e) | Binary (e, MUL, Cst (c,ant)) -> distribute (MUL, Cst (c,ant)) e
  | Binary (e, DIV, Cst (c,ant)) -> distribute (DIV, Cst (c,ant)) e
  | Binary (e1, binop, e2) -> Binary (expand e1, binop, expand e2)

(* simplifies elementary function *)
let rec simplify_expr expr change =
  (* Format.printf "  --> %a@." print_expr expr; *)
  match expr with
  | Funcall (name, args) -> (Funcall (name, args), change)
  | Cst (c,ant) -> (Cst (c,ant), change)
  | Var v -> (Var v, change)
  | Unary (NEG, e) ->
     (match e with
      | Cst (c,_) when is_zero c -> (zero, true)
      | Cst (c,ant) -> (Cst ((Bound_rat.neg c),ant), true)
      | Unary (NEG, e') -> simplify_expr e' true
      | Binary (Cst (a,ant), SUB, Cst (b,_)) -> (Cst ((Bound_rat.sub b a),ant), true)
      | Binary (a1, SUB, a2) -> simplify_expr (Binary (a2, SUB, a1)) true
      | _ -> let (e', b) = simplify_expr e change in (Unary (NEG, e'), b)
     )
  | Binary (e1, b, e2) ->
     (match b with
      | ADD ->
         (match e1, e2 with
          | Cst (a,ant), Cst (b,ant') -> (Cst ((Bound_rat.add a b),join_annot ant ant'), true)
          | Cst (z,_), e2 when is_zero z -> simplify_expr e2 change
          | e1, Cst (z,_) when is_zero z -> simplify_expr e1 change
          | e1 , Cst (c,annot) when is_neg c ->
             simplify_expr (Binary(e1, SUB, Cst ((Bound_rat.neg c),annot))) true
          | Cst (c,annot), e1 when is_neg c ->
             simplify_expr (Binary(e1, SUB, Cst ((Bound_rat.neg c),annot))) true
          | e1, Unary(NEG, e) -> simplify_expr (Binary(e1, SUB, e)) true
          | Unary(NEG, e), e2 -> simplify_expr (Binary(e2, SUB, e)) true
          | e1, e2 -> apply simplify_expr e1 e2 change ADD
         )
      | SUB ->
         (match e1, e2 with
          | Cst (a,ant), Cst (b,ant') -> (Cst ((Bound_rat.sub a b),(join_annot ant ant')), true)
          | Cst (c,_), _ when is_zero c->
             let (e, _) = simplify_expr e2 change in (Unary (NEG, e), true)
          | _, Cst (c,_) when is_zero c -> simplify_expr e1 change
          | e1 , Cst (c,ant') when is_neg c -> simplify_expr (Binary(e1, ADD, Cst ((Bound_rat.neg c),ant'))) true
          | Cst (c,ant), e1 when is_neg c -> simplify_expr (Unary(NEG, Binary(e1, ADD, Cst ((Bound_rat.neg c),ant)))) true
          | _, Unary(NEG, e) -> simplify_expr (Binary(e1, ADD, e)) true
          | Unary(NEG, e), _ -> simplify_expr (Unary(NEG, (Binary(e, ADD, e2)))) true
          | _, _ -> apply simplify_expr e1 e2 change SUB
         )
      | MUL ->
         (match e1, e2 with
          | Cst (a,ant), Cst (b,ant') -> (Cst ((Bound_rat.mul a b),join_annot ant ant'), true)
          | Cst (c,_), _ when is_zero c -> (zero, true)
          | _, Cst (c,_) when is_zero c -> (zero, true)
          | Cst (c,_), _ when Bound_rat.equal Bound_rat.one c -> simplify_expr e2 change
          | _, Cst (c,_) when Bound_rat.equal Bound_rat.one c -> simplify_expr e1 change
          | e1 , Cst (c,annot) when is_neg c -> simplify_expr (Unary(NEG, (Binary(e1, MUL, Cst ((Bound_rat.neg c),annot))))) true
          | Cst (c,annt), e1 when is_neg c -> simplify_expr (Unary(NEG, Binary(e1, MUL, Cst ((Bound_rat.neg c),annt)))) true
          | e', Unary(NEG, e) | Unary(NEG, e), e' -> simplify_expr (Unary(NEG, (Binary(e, MUL, e')))) true
          | _, _ -> apply simplify_expr e1 e2 change MUL
         )
      | DIV ->
         (match e1, e2 with
          | _, Cst (c,_) when is_zero c -> (zero, true) (* TODO treat NaN *)
          | Cst (c,_), _ when is_zero c -> (zero, true)
          (* | Cst a, Cst b when Bound_rat.equal a b -> (one, true)
           * | Cst a, Cst b when Bound_rat.equal a (Bound_rat.neg b) -> (Cst (Bound_rat.of_int (-1)), true)
           * | Cst a, Cst b -> (Cst (Bound_rat.div a b), true)
           * | _, Cst c when Bound_rat.equal c Bound_rat.one -> simplify_expr e1 change
           * | e1, Unary(NEG, e2) | Unary(NEG, e1), e2 -> simplify_expr (Unary(NEG, (Binary(DIV, e1, e2)))) true
           * | e1 , Cst c when is_neg c -> simplify_expr (Unary(NEG, (Binary(DIV, e1, Cst (Bound_rat.neg c))))) true
           * | Cst c, e2 when is_neg c -> simplify_expr (Unary(NEG, Binary(DIV, Cst (Bound_rat.neg c), e2))) true *)
          | _, _ -> apply simplify_expr e1 e2 change DIV
         )
      | POW ->
         (match e1, e2 with
          (* | Cst a, Cst b -> (Cst (power a b), true)
           * | Cst c, _ when is_zero c -> (zero, true)
           * | _, Cst c when is_zero c -> (one, true)
           * | _, Cst c when Bound_rat.equal Bound_rat.one c -> simplify_expr e1 change *)
          | _, _ -> apply simplify_expr e1 e2 change POW
         )
     )

let rec simplify_fp expr =
  let (e, b) = simplify_expr expr false in
  if b then
    simplify_fp e
  else
    e

let rec simplify_formula = function
  | Cmp (e1,op,e2) -> Cmp (simplify_fp e1, op, simplify_fp e2)
  | FVar v -> FVar v
  | Equiv (b1,b2) -> Equiv (simplify_formula b1, simplify_formula b2)
  | Imply (b1,b2) -> Imply (simplify_formula b1, simplify_formula b2)
  | And (b1,b2) -> And (simplify_formula b1, simplify_formula b2)
  | Or (b1,b2) -> Or (simplify_formula b1, simplify_formula b2)
  | Not b -> Not (simplify_formula b)

let left_hand_side (e1, op, e2) =
  match e1, e2 with
  | Cst (c,_), _  when is_zero c -> (inv op, e2)
  | _, Cst (c,_) when is_zero c -> (op, e1)
  | _, _ -> (op, simplify_fp (Binary (e1, SUB, e2)))

let rec left_hand = function
  | Cmp c -> left_hand_side c
  | FVar _ -> failwith "`left_hand` on a boolean variable."
  | Equiv (b1,_) | Imply (b1,_) | And (b1,_) | Or (b1,_) ->
      left_hand b1
  | Not b -> left_hand b

let is_arith = function
  | Cmp (_, _, _) -> true
  | _ -> false

let rec iter_expr f = function
  | Binary (e1,_,e2) as b -> f b; iter_expr f e1; iter_expr f e2
  | Unary (_,e) as u -> f u; iter_expr f e
  | x -> f x

let rec iter_subformula f_expr f_formula = function
  | Cmp (e1,_,e2) as constr ->
     f_formula constr;
     iter_expr f_expr e1;
     iter_expr f_expr e2
  | FVar _ -> ()
  | (Equiv (b1,b2) as constr)
  | (Imply (b1,b2) as constr)
  | (And (b1,b2) as constr)
  | (Or  (b1,b2) as constr) ->
     f_formula constr;
     iter_subformula f_expr f_formula b1;
     iter_subformula f_expr f_formula b2
  | Not b as constr ->
     f_formula constr;
     iter_subformula f_expr f_formula b

let rec map_constraint f = function
  | Cmp c -> Cmp (f c)
  | FVar _ -> failwith "map_constraint not applicable on Boolean variables."
  | Equiv (b1,b2) -> Equiv (map_constraint f b1, map_constraint f b2)
  | Imply (b1,b2) -> Imply (map_constraint f b1, map_constraint f b2)
  | And (b1,b2) -> Or (map_constraint f b1, map_constraint f b2)
  | Or (b1,b2) -> And (map_constraint f b1, map_constraint f b2)
  | Not b -> Not (map_constraint f b)

let rec neg_formula = function
  | Cmp (e1,op,e2) -> Cmp(e1,neg op,e2)
  (* Trivial negation for equiv and imply in order to keep the structure of the formula (we do not rewrite the formula into AND and OR here). *)
  | FVar v -> Not (FVar v)
  | Equiv (b1,b2) -> Not (Equiv (b1, b2))
  | Imply (b1,b2) -> Not (Imply (b1, b2))
  | And (b1,b2) -> Or (neg_formula b1, neg_formula b2)
  | Or (b1,b2) -> And (neg_formula b1, neg_formula b2)
  | Not b -> b

let neg_bconstraint (e1,op,e2) = (e1,neg op,e2)

(*****************************************)
(*        PREPROCESSING FUNCTIONS        *)
(*****************************************)

let rec replace_cst_expr (id, cst) expr =
  match expr with
  | Var v when v = id -> Cst (cst,Real)
  | Unary (op, e) -> Unary (op, replace_cst_expr (id, cst) e)
  | Binary (e1, op, e2) -> Binary (replace_cst_expr (id, cst) e1, op, replace_cst_expr (id, cst) e2)
  | Funcall (v, e) -> Funcall (v, List.map(replace_cst_expr (id, cst))e)
  | _ as e -> e

let rec replace_cst_formula cst = function
  | Cmp (e1, op, e2) -> Cmp (replace_cst_expr cst e1, op, replace_cst_expr cst e2)
  | FVar v -> FVar v
  | Equiv (b1, b2) -> Equiv (replace_cst_formula cst b1, replace_cst_formula cst b2)
  | Imply (b1, b2) -> Imply (replace_cst_formula cst b1, replace_cst_formula cst b2)
  | And (b1, b2) -> And (replace_cst_formula cst b1, replace_cst_formula cst b2)
  | Or (b1, b2) -> Or (replace_cst_formula cst b1, replace_cst_formula cst b2)
  | Not b -> Not (replace_cst_formula cst b)

module Variables = Set.Make(struct type t=var let compare=compare end)

let rec get_vars_expr = function
  | Cst (_,_)          -> []
  | Var v              -> [v]
  | Unary (_, e)       -> get_vars_expr e
  | Binary (e1, _, e2) -> List.rev_append (get_vars_expr e1) (get_vars_expr e2)
  | Funcall (_,args)   -> List.concat (List.map get_vars_expr args)

let get_vars_set_expr expr = Variables.of_list (get_vars_expr expr)

let rec get_vars_formula = function
  | Cmp (e1, _, e2) -> List.append (get_vars_expr e1) (get_vars_expr e2)
  | FVar v -> [v]
  | Equiv (b1, b2)
  | Imply (b1, b2)
  | And (b1, b2)
  | Or (b1, b2) -> List.append (get_vars_formula b1) (get_vars_formula b2)
  | Not b -> get_vars_formula b

let get_vars_set_formula formula = Variables.of_list (get_vars_formula formula)

let vars_of_bconstraint c = get_vars_formula (Cmp c)

let is_defined_over vars c =
  List.for_all (fun v -> List.mem v vars) (get_vars_formula (Cmp c))

let from_cst_to_expr (id, (l, u)) =
  if l = u then [(Var id, EQ, Cst (l,Real))]
  else [(Var id, GEQ, Cst (l,Real)); (Var id, LEQ, Cst (u,Real))]

let csts_to_expr csts =
  List.fold_left (fun l cst -> List.append (from_cst_to_expr cst) l) [] csts

let rec replace_var_in_expr f e =
  match e with
  | Cst (c,a) -> Cst (c,a)
  | Var v -> f v
  | Unary (op, e) -> Unary (op, replace_var_in_expr f e)
  | Binary (e1, op, e2) -> Binary (replace_var_in_expr f e1, op, replace_var_in_expr f e2)
  | Funcall (fname, args) -> Funcall (fname, (List.map (replace_var_in_expr f) args))

let rec mapfold_conjunction f = function
  | Cmp c -> f c
  | And (f1, f2) -> (mapfold_conjunction f f1)@(mapfold_conjunction f f2)
  | FVar _ -> raise (Wrong_modelling "unsupported boolean variable")
  | Equiv _ -> raise (Wrong_modelling "unsupported equivalence")
  | Imply _ -> raise (Wrong_modelling "unsupported implication")
  | Or _ -> raise (Wrong_modelling "unsupported disjunction")
  | Not _ -> raise (Wrong_modelling "unsupported negation")
