open Csp
open Apron
open Apron_utils
open Abstract_domain

module type ADomain = sig
  type t
  val get_manager: t Manager.t
end

(* Translation functor for AbSolute's constraint language to APRON values *)
module Representation (D:ADomain) = struct

  type var_kind = Environment.typvar

  (* In apron, there are two environments, one containing the integer variables
     and the other containing the real variables. Hence, the ids contain both
     the type of a variable, and its index in the corresponding env *)
  type var_id      = var_kind * int

  type rconstraint = Tcons1.t

  module Env = Tools.VarMap
  module Renv = Mapext.Make(struct type t=var_id let compare = compare end)

  type t = {
    elm  : D.t Abstract1.t;   (* actual abstract element *)
    env  : var_id Env.t;      (* map each variable to its associated id *)
    renv : var Renv.t;        (* reversed mapping of `env`. *)
  }

  let man = D.get_manager

  let empty : t = {
      elm  = Abstract1.top man empty_env;
      env  = Env.empty;
      renv = Renv.empty;
    }

  let extend ({env;renv;_} as repr : t) ((var, var_id):Csp.var*var_id) =
    (* let int_env,real_env =
     *   match kind with
     *   | Environment.INT -> [||], [||]
     *   | Environment.REAL ->
     * in
     * let new_env = Environment.add elm.Abstract1.env int_arr real_arr in
     * let elm = Abstract1.change_environment man elm new_env false in *)
    let env' = Env.add var var_id env in
    let renv' = Renv.add var_id var renv in
    {repr with env=env';renv=renv'}

  let to_logic_var (elm:t) (id:var_id) : Csp.var =
    Renv.find id elm.renv

  let to_abstract_var (elm:t) (v:Csp.var) : var_id  =
    Env.find v elm.env

  let rec expr_to_apron env (e:expr) : Texpr1.expr =
    match e with
    | Funcall (name,args) ->
       (match name,args with
        | "sqrt",[x] ->
           let e1 = expr_to_apron env x in
           Texpr1.Unop (Texpr1.Sqrt, e1, Texpr1.Real, Texpr1.Near)
        | _ -> raise (Wrong_modelling("operation not available with apron"))
       )
    | Var v ->
       let var = Var.of_string v in
       if not (Environment.mem_var env var)
       then failwith ("variable not found: "^v);
       Texpr1.Var var
    | Cst (c,_) -> Texpr1.Cst (Coeff.s_of_mpqf (Bound_rat.to_mpqf c))
    | Unary (o,e1) ->
       let r = match o with
         | NEG  -> Texpr1.Neg
       in
       let e1 = expr_to_apron env e1 in
       Texpr1.Unop (r, e1, Texpr1.Real, Texpr1.Near)
    | Binary (o,e1,e2) ->
       let r = match o with
         | ADD -> Texpr1.Add
         | SUB -> Texpr1.Sub
         | DIV -> Texpr1.Div
         | MUL -> Texpr1.Mul
         | POW -> Texpr1.Pow
       in
       let e1 = expr_to_apron env e1
       and e2 = expr_to_apron env e2 in
       Texpr1.Binop (r, e1, e2, Texpr1.Real, Texpr1.Near)

  let cmp_expr_to_tcons b env =
    let cmp_to_apron (e1,op,e2) =
      match op with
      | EQ  -> e1, e2, Tcons1.EQ
      | NEQ -> e1, e2, Tcons1.DISEQ
      | GEQ -> e1, e2, Tcons1.SUPEQ
      | GT  -> e1, e2, Tcons1.SUP
      | LEQ -> e2, e1, Tcons1.SUPEQ
      | LT  -> e2, e1, Tcons1.SUP
    in
    let e1,e2,op = cmp_to_apron b in
    let e = Binary (SUB, e1, e2) in
    let e = Texpr1.of_expr env (expr_to_apron env e) in
    Tcons1.make e op

  let rewrite elm (bcons:Csp.bconstraint) : rconstraint list =
    let env = Abstract1.env elm in
    [cmp_expr_to_tcons bcons env]

  let relax = rewrite

  let negate : rconstraint -> rconstraint = assert false
end
