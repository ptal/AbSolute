open Printf
open Bench_desc_t

let json_ext = ".json"
let absolute_ext = ".abs"
let psplib_ext = ".sm"
let patterson_ext = ".rcp"
let pro_gen_ext = ".sch"
let usage = "Usage: absolute_bench <configuration file>\n"

let extension_of_problem_kind = function
  | `PSPlib -> psplib_ext
  | `Patterson -> patterson_ext
  | `ProGenMax -> pro_gen_ext

let print_warning msg =
  eprintf "[Warning] %s\n%!" msg

let eprintf_and_exit msg =
  eprintf "[Error] %s\n%!" msg;
  exit 1

let file_argument fname =
  if Filename.check_suffix fname json_ext
  then
    fname
  else
    eprintf_and_exit (sprintf
      "Unknown file extension: %s. We expect `%s`.\n" fname json_ext)

let find_file fname =
  if Sys.file_exists fname then
    fname
  else begin
    Printf.printf "%s" usage;
    eprintf_and_exit (sprintf
      "Cannot find the file %s\n%!" fname)
  end

let file_to_string fname =
  let fpath = find_file fname in
  begin
    let ic = open_in fpath in
    try
      let content = Std.input_all ic in
      close_in ic;
      content
    with e ->
      close_in ic;
      eprintf_and_exit (sprintf
        "Reading file %s failed.\nError: %s." fname (Printexc.to_string e))
  end

let get_bench_desc () =
  if Array.length Sys.argv < 2 then
  begin
    eprintf_and_exit "Benchmarks description file missing (see benchmark/data/example.json)."
  end
  else file_to_string (Array.get Sys.argv 1)

let get_config_desc () =
  if Array.length Sys.argv > 2 then
    Some (file_to_string (Array.get Sys.argv 2))
  else
    None

let check_problem_set_ext ext =


let check_problem_file_format config ext problem_path =
  if Sys.is_directory problem_path then begin
    print_warning ("subdirectory " ^ problem_path ^ " ignored.");
    false end
  else
    match ext with
    | Some(ext) when (String.lowercase_ascii (Filename.extension problem_path)) <> ext -> begin
          print_warning ("file \"" ^ problem_path ^
          "\" ignored (expected extension `" ^ ext ^ "`).");
          false end
    | _ -> true

let is_digit c = c >= '0' && c <= '9'

(* Extract the longest number in `s` starting at position `i`. *)
let extract_number s i =
  let open String in
  let rec aux l =
    let j = i + l in
    if length s = j then l
    else if is_digit (get s j) then aux (l+1)
    else l
  in
    let l = aux 0 in
    let s' = sub s i l in
    (int_of_string s', i + l)

(* It compares two strings taking into account the number parts.
   For example ["a1"; "a10"; "a2"] is sorted as ["a1"; "a2"; "a10"].  *)
let natural_comparison x y =
  let open String in
  let rec aux i j =
    if (length x) = i then -1
    else if (length y) = j then 1
    else
      let a = get x i in
      let b = get y j in
      match Pervasives.compare a b with
      | _ when is_digit a -> compare_number_sequence i j
      | 0 -> aux (i+1) (j+1)
      | r -> r
  and compare_number_sequence i j =
    let (n, i') = extract_number x i in
    let (m, j') = extract_number y j in
    (* let _ = Printf.printf "%d , %d\n" n m in *)
    match Pervasives.compare n m with
    | 0 -> aux i' j'
    | r -> r
  in
    aux 0 0

let remove_trailing_slash dir1 =
  let l = (String.length dir1) - 1 in
  if dir1.[l] = '/' then String.sub dir1 0 l else dir1

let concat_dir dir1 dir2 =
  let dir1 = remove_trailing_slash dir1 in
  dir1 ^ "/" ^ dir2

let list_of_problems bench =
  let path = concat_dir bench.input_dir bench.problem_set_path in
  if Sys.is_directory path then
    let ext = Filename.extension (remove_trailing_slash path) in
    let _ = check_existing_extension ext in
    let files = Sys.readdir path in
    Array.sort natural_comparison files;
    Array.to_list files |>
    List.map (fun x -> path ^ x) |>
    List.filter (check_problem_file_format bench)
  else
    if check_problem_file_format bench path then
      [path]
    else
      []

let call_command command =
  flush_all ();
  let status = Sys.command command in
  status

let time_of coeff time =
  Mtime.Span.of_uint64_ns (Int64.mul (Int64.of_int coeff) (Int64.of_int time))

let time_of_ms = time_of 1000000
let time_of_sec = time_of 1000000000

let timeout_of_config config = time_of_sec config.timeout
