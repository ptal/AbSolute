
exception Wrong_modelling of string



type t = int

(* ordering *)
(* ******** *)
  
let equal = (=)
let leq a b = a land lnot b = 0
let geq a b = b land lnot a = 0
let neq = (<>)
let lt a b = leq a b && neq a b
let gt a b = geq a b && neq a b
let compare a b = if a lt b then -1 else (if a gt b then 1 else 0)
  
let odd (x:t) : bool = x mod 2 = 1
let even (x:t) : bool = x mod 2 = 0

(* These functions are not exact because the order is partial *)
let min (x:t) (y:t) : t = if x > y then y else x
let max (x:t) (y:t) : t = if y < x then y else x
  
let sign x = if x = 0 then 0 else 1
  
(** True if the bound elements do not have machine-representable successors or predecessors. *)
let is_continuous = false
  
(** `succ` and `prec` returns the number right after the current one, if it exists.
    Otherwise, it acts as the identity function.
    For example, it exists for integers, but not for float and rational. *)
let succ a = if lnot a = 0 then a else (a+1)
let prec a = if a = 0 then a else (a-1)
  
(* construction *)
(* ************ *)
  
(* operators and conversions are tagged with a _up or _down suffix
   to indicate rounding direction
 *)
           
let of_int_up a = a
let of_int_down a = a
let of_float_up _ = raise (Wrong_modelling "of_float_up not suited for bit-vectors")
let of_float_down _ = raise (Wrong_modelling "of_float_down not suited for bit-vectors")
let of_rat_up _ = raise (Wrong_modelling "of_rat_up not suited for bit-vectors")
let of_rat_down _ = raise (Wrong_modelling "of_rat_down not suited for bit-vectors")

(* TODO Maybe these two are possible... *)
let of_string_up _ = raise (Wrong_modelling "of_string_up not suited for bit-vectors")
let of_string_down _ = raise (Wrong_modelling "of_string_down not suited for bit-vectors")
  
(* printing *)
(* ******** *)
  
let to_string b = string_of_int b
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)
  
(* conversion *)
(* ********** *)
  
let to_float_up _ = raise (Wrong_modelling "to_float_up not suited for bit-vectors")
let to_float_down _ = raise (Wrong_modelling "to_float_down not suited for bit-vectors")
let to_rat _ = raise (Wrong_modelling "to_rat not suited for bit-vectors")
let to_int_up a = a
let to_int_down a = a
  
(* classification *)
(* ************** *)
  
type kind =
  | FINITE      (* finite number *)
  | MINF | INF  (* -oo or +oo *)
  | INLETID     (* for NaN and other invalid numbers *)
  
let classify _ = FINITE
  
(* useful constants *)
(* **************** *)
  
let zero = 0
let one = 1
let two = 2
let minus_one = -1 (* TODO, this should not happen ! *)
let inf = -1
let minus_inf = -1
  
  
(* operators *)
(* ********* *)
  
(* exact operators *)
let neg _ _ = raise (Wrong_modelling "neg not suited for bit-vectors")
let abs _ = raise (Wrong_modelling "abs not suited for bit-vectors")
  
(* operators with rounding *)
let add_up _ _ = raise (Wrong_modelling "add_up not suited for bit-vectors")
let sub_up _ _ = raise (Wrong_modelling "sub_up not suited for bit-vectors")
let mul_up _ _ = raise (Wrong_modelling "mul_up not suited for bit-vectors")
let div_up _ _ = raise (Wrong_modelling "div_up not suited for bit-vectors")
let add_down _ _ = raise (Wrong_modelling "add_down not suited for bit-vectors")
let sub_down _ _ = raise (Wrong_modelling "sub_down not suited for bit-vectors")
let mul_down _ _ = raise (Wrong_modelling "mul_down not suited for bit-vectors")
let div_down _ _ = raise (Wrong_modelling "div_down not suited for bit-vectors")
  
let bound_mul _ _ = raise (Wrong_modelling "bound_mul not suited for bit-vectors")
let bound_div _ _ = raise (Wrong_modelling "bound_div not suited for bit-vectors")
  
let sqrt_up _ _ = raise (Wrong_modelling "sqrt_up not suited for bit-vectors")
let sqrt_down _ _ = raise (Wrong_modelling "sqrt_down not suited for bit-vectors")
  
let pow_up _ _ = raise (Wrong_modelling "pow_up not suited for bit-vectors")
let pow_down _ _ = raise (Wrong_modelling "pow_down not suited for bit-vectors")
let root_up _ _ = raise (Wrong_modelling "root_up not suited for bit-vectors")
let root_down _ _ = raise (Wrong_modelling "root_down not suited for bit-vectors")
  
let cos_up _ = raise (Wrong_modelling "cos_up not suited for bit-vectors")
let cos_down _ = raise (Wrong_modelling "cos_down not suited for bit-vectors")
let sin_up _ = raise (Wrong_modelling "sin_up not suited for bit-vectors")
let sin_down _ = raise (Wrong_modelling "sin_down not suited for bit-vectors")
let tan_up _ = raise (Wrong_modelling "tan_up not suited for bit-vectors")
let tan_down _ = raise (Wrong_modelling "tan_down not suited for bit-vectors")
  
let acos_up _ = raise (Wrong_modelling "acos_up not suited for bit-vectors")
let acos_down _ = raise (Wrong_modelling "acos_down not suited for bit-vectors")
let asin_up _ = raise (Wrong_modelling "asin_up not suited for bit-vectors")
let asin_down _ = raise (Wrong_modelling "asin_down not suited for bit-vectors")
let atan_up _ = raise (Wrong_modelling "atan_up not suited for bit-vectors")
let atan_down _ = raise (Wrong_modelling "atan_down not suited for bit-vectors")
  
let exp_up _ = raise (Wrong_modelling "exp_up not suited for bit-vectors")
let exp_down _ = raise (Wrong_modelling "exp_down not suited for bit-vectors")
let ln_up _ = raise (Wrong_modelling "ln_up not suited for bit-vectors")
let ln_down _ = raise (Wrong_modelling "ln_down not suited for bit-vectors")
let log_up _ = raise (Wrong_modelling "log_up not suited for bit-vectors")
let log_down _ = raise (Wrong_modelling "log_down not suited for bit-vectors")
  
(* integer rounding *)
let floor a = a
let ceil a = a
                 
