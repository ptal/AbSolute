(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Core.Tools
open Ast
open Bounds

let print_unop fmt = function
  | NEG -> Format.fprintf fmt "-"

let print_binop fmt = function
  | ADD -> Format.fprintf fmt "+"
  | SUB -> Format.fprintf fmt "-"
  | MUL -> Format.fprintf fmt "*"
  | DIV -> Format.fprintf fmt "/"
  | POW -> Format.fprintf fmt "^"

let print_cmpop fmt = function
  | EQ  -> Format.fprintf fmt "="
  | LEQ -> Format.fprintf fmt "<="
  | GEQ -> Format.fprintf fmt ">="
  | NEQ -> Format.fprintf fmt "<>"
  | GT  ->  Format.fprintf fmt ">"
  | LT  -> Format.fprintf fmt "<"

let print_var fmt s = Format.fprintf fmt "%s" s

let prior_level = function
  | Funcall(_),Funcall(_) -> 0
  | Funcall(_),_          -> 1
  | Binary(_,POW,_),Binary(_,POW,_)  -> 0
  | Binary(_,POW,_),Binary(_)  -> 0
  | Binary(_,(MUL | DIV),_),Binary(_,(MUL | DIV),_) -> 0
  | Binary(_,(MUL | DIV),_),_ -> 1
  | _ -> 0

let must_parenthesis_right (father:expr) (e:expr) =
  match father,e with
  | Binary(_,x,_),Binary(_,SUB,_) -> x<>SUB
  | _ -> false

let print_expr fmt t =
  let rec loop parenflag (k:unit->unit) fmt t =
    match t with
    | Funcall("sqr",[x]) -> loop false k fmt (Binary(x,POW,Cst(Bound_rat.two,Int)))
    | Funcall(("POWER"|"power"),[x;y]) -> loop false k fmt (Binary(x,POW,y))
    | Funcall (name,args) ->
       Format.fprintf fmt "%a(" print_var name;
       Format.pp_print_list ~pp_sep:pp_sep_comma
         (loop false (fun () -> Format.fprintf fmt ")"; k())) fmt args;
    | Binary (t1,b,t2) as father ->
       let right_prior = prior_level (father,t2) in
       let k =
         if parenflag then begin
             Format.fprintf fmt "(";
             (fun () -> Format.fprintf fmt ")";k())
           end
         else k
       in
       loop
         (prior_level (father,t1) > 0)
         (fun () ->
           Format.fprintf fmt " %a " print_binop b;
           loop
             (right_prior > 0 || (right_prior=0 && must_parenthesis_right father t2)) k
             fmt t2)
         fmt t1
    | Unary (u,e) ->
       Format.fprintf fmt "(%a" print_unop u;
       loop true (fun () -> Format.fprintf fmt ")"; k ()) fmt e
    | Var v -> Format.fprintf fmt "%a" print_var v; k ()
    | Cst (c,_) -> Format.fprintf fmt "%a" Bound_rat.pp_print c; k ()
  in
  loop false (fun () -> ()) fmt t

let print_formula fmt e =
  let rec aux fmt = function
    | Cmp (e1,c,e2) -> Format.fprintf fmt "%a %a %a"
        print_expr e1 print_cmpop c print_expr e2
    | FVar v -> Format.fprintf fmt "%s" v
    | Equiv (b1,b2) -> Format.fprintf fmt "(%a <=> %a)" aux b1 aux b2
    | Imply (b1,b2) -> Format.fprintf fmt "(%a => %a)" aux b1 aux b2
    | And (b1,b2) -> Format.fprintf fmt "(%a && %a)" aux b1 aux b2
    | Or (b1,b2) -> Format.fprintf fmt "(%a || %a)" aux b1 aux b2
    | Not b -> Format.fprintf fmt "not %a" aux b in
  aux fmt e

let rec print_qformula fmt = function
  | Exists(v,ty,qf) ->
      Format.fprintf fmt "(%s:%s)%a"
        v (Types.string_of_ty ty) print_qformula qf
  | QFFormula f -> Format.fprintf fmt "%a" print_formula f

let print_constraint fmt c =
  Format.fprintf fmt "%a" print_formula (Cmp c)

let output_constraints constraints =
  List.iter (Format.printf "%a\n" print_constraint) constraints

let print_constraints = Format.pp_print_list ~pp_sep:pp_sep_newline print_formula

let string_of_constraint c =
  print_constraint Format.str_formatter c;
  Format.flush_str_formatter ()

let string_of_formula f =
  Format.fprintf Format.str_formatter "%a" print_formula f;
  Format.flush_str_formatter ()