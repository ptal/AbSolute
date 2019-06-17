(** Generic signature for intervals. The interface is functional.*)

open Bot
exception Wrong_modelling of string
                           
module Bdd = struct

  (************************************************************************)
  (** {1 TYPES} *)
  (************************************************************************)

  (** an interval is a pair of bounds (lower,upper);
     intervals are always non-empty: lower <= upper;
     functions that can return an empty interval return it as Bot *)
  type t = T | F | N of t*t
  module B = Bound_int
  type bound = B.t
  type var_kind = ZERO | ONE | TOP_INT | TOP_REAL | TOP
                  | OF_BOUNDS of bound*bound | OF_INTS of int*int | OF_RATS of Bound_rat.t*Bound_rat.t | OF_FLOATS of float*float
                  | OF_INT of int | OF_RAT of Bound_rat.t | OF_FLOAT of float
                  | COMPLETE of int (* complete BDD *)
  type unop_kind = NEG | ABS | NOT | PREF of int | SUFF of int
  type binop_kind = ADD | SUB | MUL | POW | XOR | AND | OR

  (* Functions on BDDs *)
                                                      
  (* One should never use the N constructor but this one instead (to ensure maximal sharing) *)
  let bdd_of =
    let main_hash = Hashtbl.create 101 in
    Hashtbl.add main_hash (ref F,ref F) F;
    let aux a b = 
      try Hashtbl.find main_hash (ref a,ref b)
      with Not_found -> let t = N(a,b) in Hashtbl.add main_hash (ref a,ref b) t; t in
    aux

  let is_leaf m = m == T || m == F
    
  (************************************************************************)
  (** {1 CONSTRUCTOR} *)
  (************************************************************************)
    
  let rec complete_bdd depth =
    (* Returns the complete BDD of given depth *)
    match depth with
    | 0 -> T
    | _ -> let res = complete_bdd (depth-1) in
           bdd_of res res
           
  let create var = match var with
    | COMPLETE(i) -> complete_bdd i
    | _ -> raise (Wrong_modelling "his creation of variable is not suited (or not implemented) for BDDs")

  (************************************************************************)
  (** {1 PRINTING and CONVERSIONS } *)
  (************************************************************************)

  let lb b : bound =
    let rec aux t acc =
      match t with
      | T -> acc
      | F -> failwith "A bound has been asked on an empty BDD"
      | N(F,a) -> aux a (2*acc+1)
      | N(a,_) -> aux a (2*acc) in
    aux b 0
    
  let ub b : bound =
    let rec aux t acc =
      match t with
      | T -> acc
      | F -> failwith "A bound has been asked on an empty BDD"
      | N(a,F) -> aux a (2*acc)
      | N(_,a) -> aux a (2*acc+1) in
    aux b 0
  let to_float_range _ = failwith "BDDs can't be translated to float range"
  let to_rational_range _ = failwith "BDDs can't be translated to float range"
  let to_range b = lb b, ub b

  (** returns the type annotation of the represented values *)
  let to_annot _ = failwith "TODO I don't know what is the point of this function"
  let print fmt = Format.printf fmt "BDDs are too hard to print"

  let to_expr _ = failwith "BDDs can't be represented by expressions"

  (************************************************************************)
  (** {1 SET-THEORETIC } *)
  (************************************************************************)

  (** operations *)

  let join =
    (* The union of two bdds. *)
    let union_hash = Hashtbl.create 101 in
    let rec aux m m' =
      try Hashtbl.find union_hash (ref m,ref m')
      with Not_found ->
        let t = match m, m' with
          | _, T | T, _ -> T
          | F, _ -> m'
          | _, F -> m
          | N(a,b), N(c,d) -> bdd_of (aux a c) (aux b d)
        in
        Hashtbl.add union_hash (ref m,ref m') t;
        t
    in aux
     
  let meet =
  (* The intersection of two bdds *)
  let inter_hash = Hashtbl.create 101 in
  let rec aux m m' =
    try Hashtbl.find inter_hash (ref m,ref m')
    with Not_found ->
      let res = match m, m' with
        | F, _ | _, F-> Bot
        | T, T -> Nb(T)
        | N(a,b), N(c,d) -> meet_bot2 bdd_of (aux a c) (aux b d)
        | _ -> failwith "intersection: the BDDs do not have the same depth"
      in
      Hashtbl.add inter_hash (ref m,ref m') res;
      res
  in aux

  (** predicates *)
  let contains_float t f =
    float_of_int (lb t) <= f && f <= float_of_int (ub t)

  let rec is_singleton t = match t with
    | T | F -> true
    | N(F,a) | N(a,F) -> is_singleton a
    | _ -> false
         
  let check_bot t = if t == F then Bot else Nb(t)
                  
  let equal = (==)
            
  (** mesure *)
  let float_size =
    (* Returns the depth of the bdd and the cardinality of the set it represents (bounded by max_int, bigger than this will return max_int *)
    let data = Hashtbl.create 101 in
    let rec aux m =
      try Hashtbl.find data (ref m)
      with Not_found ->
        let res = match m with
          | T -> 1.
          | F -> 0.
          | N(a,b) -> aux a +. aux b
        in
        Hashtbl.add data (ref m) res;
        res
    in aux

  (** split *)

  (** returns a split priority. The higher the better *)
  let score = float_size

  (** Splits at the first possible depth *)
  let rec split m = match m with
    | T | F -> raise (Wrong_modelling "split_backtrack: The BDD has cardinal 1 (or maybe 0) and can't be splitted")
    | N(T,T) -> bdd_of T F, bdd_of F T
    | N(b,a) when is_leaf b -> let c,d = split a in
                               bdd_of b c, bdd_of b d
    | N(a,b) when is_leaf b -> let c,d = split a in
                               bdd_of c b, bdd_of d b
    | N(a,b) -> bdd_of a F, bdd_of F b
  let split_on t _ = split t

  (** pruning TODO is it useful ? *)
  let prune = None

  (************************************************************************)
  (** {1 INTERVAL ARITHMETICS (FORWARD EVALUATION)} *)
  (************************************************************************)

  (* Old stuff, now replaced with generic function
  val neg: t -> t
  val abs: t -> t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t

  (** returns valid value when the exponant is a singleton positive integer. fails otherwise*)
  val pow: t -> t -> t*)

  (** return valid values (possibly Bot, if dividend is nul) *)
  let div _ _ = raise (Wrong_modelling "division is not suited for bdds")
              
  let bdd_not =
    let hash_and = Hashtbl.create 101 in
    let rec aux m =
      try Hashtbl.find hash_and (ref m)
      with Not_found ->
        let res =
          match m with
          | N(a,b) -> bdd_of (aux b) (aux a)
          | _ -> m
        in
        Hashtbl.add hash_and (ref m) res;
        res
    in aux

  let pref =
    let hash_pref = Hashtbl.create 101 in
    let rec aux m d =
      try Hashtbl.find hash_pref (ref m,d)
      with Not_found ->
            let res =
              match m,d with
              | F,_ -> F
              | _,0 -> T
              | T,_ -> failwith "the length of the prefixes asked is too big"
              | N(a,b),_ -> bdd_of (aux a (d-1)) (aux b (d-1)) in
            Hashtbl.add hash_pref (ref m,d) res;
            res in
    aux
    
     
  (* Given an operation (unary or binary), return the operation on the vardom *)
  let unop op m = match op with
    | NOT -> bdd_not m
    | PREF(i) -> pref m i
    | SUFF(_) -> failwith "The suffix operation has to be improved, depending on the problems"
    | _ -> raise (Wrong_modelling "operation not supported by BDD domain")
             
  let bdd_and =
    let hash_and = Hashtbl.create 101 in
    let rec aux m m' =
      try Hashtbl.find hash_and (ref m,ref m')
      with Not_found ->
        let res =
          match m, m' with
          | T, T -> T
          | F,_ | _,F -> F
          | T,_ | _,T -> failwith "bdd_and: not the same depth"
          | N(a,b), N(c,d) -> bdd_of (join (aux a c) (join (aux a d) (aux b c))) (aux b d)
        in
        Hashtbl.add hash_and (ref m,ref m') res;
        res
    in aux
     
  let bdd_or =
    let hash_or = Hashtbl.create 101 in
    let rec aux m m' =
      try Hashtbl.find hash_or (ref m,ref m')
      with Not_found ->
        let res =
          match m, m' with
          | T, T -> T
          | F,_ | _,F -> F
          | T,_ | _,T -> failwith "bdd_or: not the same depth"
          | N(a,b), N(c,d) -> bdd_of (aux a c) (join (aux a d) (join (aux b c) (aux b d)))
        in
        Hashtbl.add hash_or (ref m,ref m') res;
        res
    in aux
     
  let bdd_xor =
    let hash_xor = Hashtbl.create 101 in
    let rec aux m m' =
      try Hashtbl.find hash_xor (ref m,ref m')
      with Not_found ->
        let res =
          match m, m' with
          | T, T -> T
          | F,_ | _,F -> F
          | T,_ | _,T -> failwith "bdd_xor: not the same depth"
          | N(a,b), N(c,d) -> bdd_of (join (aux a c) (aux b d)) (join (aux a d) (aux b c))
        in
          Hashtbl.add hash_xor (ref m,ref m') res;
          res
    in aux
     
  let binop op = match op with
    | XOR -> bdd_xor
    | AND -> bdd_and
    | OR -> bdd_or
    | _ -> raise (Wrong_modelling "binary operation not supported on BDDs")
    
  (** function calls (sqrt, exp, ln ...) are handled here :
      given a function name and and a list of argument,
     it returns a possibly bottom result *)
  let eval_fun _ _ = raise (Wrong_modelling "There is no function to be applied on BDDs")

  (************************************************************************)
  (** {1 FILTERING (TEST TRANSFER FUNCTIONS)}                             *)
  (************************************************************************)

  (** given two interval arguments, return a subset of each argument
      by removing points that cannot satisfy the predicate;
      may also return Bot if no point can satisfy the predicate
      simplified interface since a > b <=> b < a *)

  let filter_leq _ _ = raise (Wrong_modelling "filter leq not defined on BDDs")
  let filter_lt _ _ = raise (Wrong_modelling "filter leq not defined on BDDs")
  let filter_eq b1 b2 = let r = meet b1 b2 in
                       lift_bot (fun x -> x,x) r
  let filter_neq _ _ = raise (Wrong_modelling "filter leq not defined on BDDs")

  (** given the interval argument(s) and the expected interval result of
     a numeric operation, returns refined interval argument(s) where
     points that cannot contribute to a value in the result are
     removed;
     may also return Bot if no point in an argument can lead to a
     point in the result *)

  (* Old stuff
  val filter_neg: t -> t -> t bot
  val filter_abs: t -> t -> t bot

  val filter_add: t -> t -> t -> (t*t) bot
  val filter_sub: t -> t -> t -> (t*t) bot
  val filter_mul: t -> t -> t -> (t*t) bot

  val filter_pow: t -> t -> t -> (t*t) bot *)
  let filter_div _ _ = failwith "BDDs can't filter div"

  let bot_if_false b = if b == F then Bot else Nb(b)
                     
  let filter_unop op (b:t) (r:t) = match op with
    | NOT -> meet b (bdd_not r)
    | PREF(_) -> failwith "TODO prefix filter"
    | SUFF(_) -> failwith "The suffix operation has to be improved, depending on the problems"
    | _ -> raise (Wrong_modelling "operation not supported by BDD domain")

  (* b1 xor b2 = r <=> b1 = r xor b2 <=> b2 = r xor b1 *)
  let filter_binop op (b1:t) (b2:t) (r:t) = match op with
    | XOR -> let new_dom1 = bdd_xor r b2 in
             let new_dom2 = bdd_xor r b1 in
             (bot_if_false new_dom1,bot_if_false new_dom2)
    | AND -> failwith "TODO inverse and"
    | OR -> failwith "TODO inverse or"
    | _ -> raise (Wrong_modelling "binary operation not supported on BDDs")

  (** filtering function calls like (sqrt, exp, ln ...) is done here : Bonjour

Sauf erreur, vous serez hébergé ce soir dans une résidence de l'école au 132 rue Gambetta (pas loin du centre voir plus bas).
Pour vous remettre les clés ce soir, je vous propose deux rendez vous devant la résidence au 132 :
- un à 19h,
- un à 21h45,
Vous venez à celui qui à votre préférence.
En cas de problème, vous pouvez m'appeler au 0 610 490 356.

A ce soir
Michel
     given a function name, a list of argument, and a result,
     it remove points that cannot satisfy the relation : f(arg1,..,argn) = r;
     it returns a possibly bottom result *)
  let filter_fun _ _ = failwith "No function to filter with BDDs"

  (** Only filters the first argument *)
    (* Old stuff 
  val filter_add_f: t -> t -> t -> t bot
  val filter_sub_f: t -> t -> t -> t bot
  val filter_mul_f: t -> t -> t -> t bot
  val filter_pow_f: t -> t -> t -> t bot *)
  let filter_div_f _ _ _ = failwith "division can't be done on BDDs"
  let filter_root_f _ _ _ = failwith "root can't be done on BDDs"
                          
  let filter_binop_f op (_b1:t) (b2:t) (r:t) = match op with
    | XOR -> let new_dom1 = bdd_xor r b2 in
             bot_if_false new_dom1
    | AND -> failwith "TODO inverse and"
    | OR -> failwith "TODO inverse or"
    | _ -> raise (Wrong_modelling "binary operation not supported on BDDs")

  (** generate a random float within the given interval *)
  let spawn _ = raise (Wrong_modelling "Can't spawn a float from a BDD")

  (** shrinks each bound of the interval by the given value *)
  let shrink = raise (Wrong_modelling "Can't shrink the bounds of a BDD")
end

