
{
 open Lexing
 open Csp
 open Parser


(* keyword table *)
let kwd_table = Hashtbl.create 10
let _ =
  List.iter (fun (a,b) -> Hashtbl.add kwd_table a b)
    [
      "info",           TOK_ANNOT;
      "draw",           TOK_DRAW;
      "init",           TOK_INIT;
      "objective", 	TOK_OBJ;
      "constraints",    TOK_CONSTR;
      "constants",      TOK_CST;
      "sqrt",           TOK_SQRT;
      "cos",            TOK_COS;
      "sin",            TOK_SIN;
      "tan",            TOK_TAN;
      "cot",            TOK_COT;
      "acos",           TOK_ACOS;
      "asin",           TOK_ASIN;
      "atan",           TOK_ATAN;
      "acot",           TOK_ACOT;
      "ln",             TOK_LN;
      "log",            TOK_LOG;
      "exp",            TOK_EXP;
      "nroot",          TOK_NROOT;
      "max",            TOK_MAX;
      "min",            TOK_MIN;
      "int",            TOK_INT;
      "real",           TOK_REAL;
      "oo",             TOK_INF;
      "-oo",            TOK_MINF
   ]

(* (exact) parsing of decimal constants constants *)
(*let parse_const c =
  let rec div10 x n =
    if n <= 0 then x else div10 (x /. (float_of_int 10)) (n-1)
  in
  try
    let p = String.index c '.' in
    let p' = String.length c - p - 1 in
    let x = (String.sub c 0 p)^(String.sub c (p+1) p') in
    div10 (float_of_string x) p'
  with Not_found ->
    float_of_string c
*)

let parse_const = float_of_string
}



(* character classes *)
let space = [' ' '\t' '\r']+
let newline = "\n" | "\r" | "\r\n"
let digit = ['0'-'9']
let const = ( digit+ | "." digit+ | digit+ "." digit* )

rule token = parse

(* identifier or reserved keyword *)
| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '%'? as id
{ try Hashtbl.find kwd_table id with Not_found -> TOK_id id }


(* symbols *)
| "("    { TOK_LPAREN }
| ")"    { TOK_RPAREN }
| "{"    { TOK_LBRACE }
| "}"    { TOK_RBRACE }
| "["    { TOK_LBRACKET }
| "]"    { TOK_RBRACKET }
| ","    { TOK_COMMA }
| ";"    { TOK_SEMICOLON }
| "+"    { TOK_PLUS }
| "-"    { TOK_MINUS }
| "*"    { TOK_MULTIPLY }
| "/"    { TOK_DIVIDE }
| "^"    { TOK_POW }
| "<"    { TOK_LESS }
| ">"    { TOK_GREATER }
| "<="   { TOK_LESS_EQUAL }
| ">="   { TOK_GREATER_EQUAL }
| "=="   { TOK_EQUAL_EQUAL }
| "!="   { TOK_NOT_EQUAL }
| "="    { TOK_ASSIGN }
| "&&"   { TOK_AND }
| "||"   { TOK_OR }
| "|"    { TOK_PIPE }
| "!"    { TOK_NOT }
| ":"    { TOK_COLON }
(* literals *)
| const as c { TOK_const (float_of_string c) }

(* spaces, comments *)
| "/*" { comment lexbuf; token lexbuf }
| "//" [^ '\n' '\r']* { token lexbuf }
| newline { new_line lexbuf; token lexbuf }
| space { token lexbuf }

(* end of file *)
| eof { TOK_EOF }

(* nested comments (handled recursively)  *)
and comment = parse
| "*/" { () }
| [^ '\n' '\r'] { comment lexbuf }
| newline { new_line lexbuf; comment lexbuf }
