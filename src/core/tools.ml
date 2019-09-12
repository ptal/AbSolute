(** A small set of useful utilities *)

(***********************)
(** {1} Printing stuff *)
(***********************)

(** same as failwith but uses a format instead *)
let fail_fmt fmt = Format.kasprintf failwith fmt

(** terminal output with a color given in parameter
  restoring default color after use *)
let color_printf fmt col x =
  Format.kasprintf (fun s -> Format.fprintf fmt "%s%s%s" col s "\027[0m") x

(** red terminal output *)
let red_fprintf fmt x = color_printf fmt "\027[31m" x

(** blue terminal output *)
let cyan_fprintf fmt x = color_printf fmt "\027[36m" x

(** green terminal output *)
let green_fprintf fmt x = color_printf fmt "\027[32m" x

(** yellow terminal output *)
let yellow_fprintf fmt x = color_printf fmt "\027[33m" x

(** 2D table print indentation *)
let matrix_print_indent fmt mat =
  let sizes = Array.make (Array.length mat.(0)) 0 in
  for i = 0 to  Array.length mat.(0) -1 do
    let maxsize = ref 0 in
    for j = 0 to  Array.length mat -1 do
      maxsize := max (!maxsize) (String.length mat.(j).(i));
    done;
    sizes.(i) <- !maxsize;
  done;
  for i = 0 to  Array.length mat -1 do
    for j = 0 to  Array.length mat.(0) -1 do
      let dif = sizes.(j) - (String.length mat.(i).(j)) in
      let fill = String.make dif ' ' in
      Format.fprintf fmt "%s%s " mat.(i).(j) fill
    done;
    Format.fprintf fmt "\n"
  done

(** Allocation of some of the most used printing utilities *)
let pp_sep_newline = fun fmt () -> Format.fprintf fmt "\n"
let pp_sep_semicolon = fun fmt () -> Format.fprintf fmt ";"
let pp_sep_comma = fun fmt () -> Format.fprintf fmt ","
let pp_sep_space = fun fmt () -> Format.fprintf fmt " "
let pp_sep_semicolon_space = fun fmt () -> Format.fprintf fmt "; "
let pp_sep_comma_space = fun fmt () -> Format.fprintf fmt ", "


(** Float printing utility *)
let pp_print_float fmt (f:float) =
  let i = int_of_float f in
  if (float i) = f then Format.pp_print_int fmt i
  else Format.pp_print_float fmt f

(** Mpqf human understandable printing *)
let pp_print_mpqf fmt (m:Mpqf.t) =
  let f = Mpqf.to_float m in
  if Mpqf.of_float f = m then pp_print_float fmt f
  else Mpqf.print fmt m

(******************************)
(** {2} Conveniency functions *)
(******************************)

(** Create a list of integers between `i` and `j` included. *)
let range i j =
  let rec aux n acc =
    if i > n then acc else aux (n-1) (n :: acc)
  in aux j []

let is_some = function
| Some _ -> true
| None -> false

let unwrap = function
| Some x -> x
| None -> failwith "unwrap an option containing `None`."

(* val fold_map: ('a -> 'b -> ('a * 'c)) -> 'a -> 'b list -> ('a * 'c list) *)
let fold_map f a l =
  let f (a, l') x =
    let (a, x') = f a x in
    (a, x'::l') in
  let (a, l') = List.fold_left f (a, []) l in
  (a, List.rev l')

(** True if `s` starts with `head`.
    `start_with` trims `s` and `head`. *)
let start_with s head =
  let s = String.trim s in
  let head = String.trim head in
  let head_len = String.length head in
  if String.length s >= head_len then
    (String.sub s 0 head_len) = head
  else
    false

(* val for_all_pairs: 'a list -> ('a -> 'a -> 'b list) -> 'b list *)
let for_all_pairs l f =
  List.flatten (List.map (fun x ->
    List.flatten (List.map (fun y ->
      f x y
    ) l)
  ) l)

let indexed_list l = List.mapi (fun i x -> (i,x)) l

(* val for_all_distinct_pairs: 'a list -> ('a -> 'a -> 'b list) -> 'b list.
   Two pairs are distinct if they do not have the same index. *)
let for_all_distinct_pairs l f =
  for_all_pairs (indexed_list l) (fun (i,x) (j,y) ->
    if i != j then
      f x y
    else
      [])

(* val for_all_asymmetric_pairs: 'a list -> ('a -> 'a -> 'b list) -> 'b list.
   Two pairs l[i] and l[j] are asymmetric if `i < j`. *)
let for_all_asymmetric_pairs l f =
  for_all_pairs (indexed_list l) (fun (i,x) (j,y) ->
    if i < j then
      f x y
    else
      [])

(* val fold_left_hd: ('a -> 'a -> 'a) -> 'a list -> 'a *)
let fold_left_hd f = function
  | [] -> raise (Failure "`fold_left_hd` on empty list")
  | a::[] -> a
  | a::l -> List.fold_left f a l

(**********************)
(** {3} Map instances *)
(**********************)

(** only one instanciation forall variable maps modules *)
module VarMap = struct
  include Mapext.Make(String)
  (** we add few utilities inside it *)

  (** we define a find_fail that fails directly with an error msg
     when a variable is not found *)
  let find_fail key map =
    try find key map
    with Not_found -> fail_fmt "variable not found: %s" key

  (** we define a find_fail that fails directly with an error msg
     when a variable is not found *)
  let find_opt key map =
    try Some (find key map)
    with Not_found -> None

  (** builds a map from an association list*)
  let of_list (assoc: (string*'a) list) =
    List.fold_left (fun acc (k,m) -> add k m acc) empty assoc
end

(* 4. Utilities for Parray. *)

let empty_parray () = Parray.init 0 (fun _ -> failwith "unreachable")

let extend_parray pa a =
  let n = Parray.length pa in
  Parray.init (n+1) (fun i -> if i < n then Parray.get pa i else a)
