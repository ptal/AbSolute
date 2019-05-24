open Printf
open Bench_instance_t
open Rcpsp

let json_ext = ".json"
let absolute_ext = ".abs"
let psplib_ext = ".sm"
let patterson_ext = ".rcp"
let pro_gen_ext = ".sch"
let supported_extensions = [psplib_ext; patterson_ext; pro_gen_ext]
let usage = "Usage: absolute_bench <configuration file>\n"

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

let check_problem_file_format config problem_path =
  if Sys.is_directory problem_path then begin
    print_warning ("subdirectory " ^ problem_path ^ " ignored.");
    false end
  else
    let ext = String.lowercase_ascii (Filename.extension problem_path) in
    match List.find_opt (String.equal ext) supported_extensions with
    | Some _ -> true
    | None ->
        (print_warning ("file \"" ^ problem_path ^
        "\" ignored (expected extension `" ^ ext ^ "`).");
        false)

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
  if dir1.[l] = Filename.dir_sep.[0] then String.sub dir1 0 l else dir1

let concat_dir dir1 dir2 =
  let dir1 = remove_trailing_slash dir1 in
  dir1 ^ Filename.dir_sep ^ dir2

let list_of_problems bench =
  let path = concat_dir bench.input_dir bench.problem_set_path in
  if Sys.is_directory path then
    let files = Sys.readdir path in
    Array.sort natural_comparison files;
    Array.to_list files |>
    List.map (fun x -> path ^ x) |>
    List.filter (check_problem_file_format bench)
  else
    eprintf_and_exit ("The problem set path `" ^ path ^ "` must be a directory. The structure of the input database must follow some conventions described in benchmark/README.md")

let call_command command =
  (* Printf.printf "%s\n" command; *)
  flush_all ();
  let status = Sys.command command in
  status

let time_of coeff time =
  Mtime.Span.of_uint64_ns (Int64.mul (Int64.of_int coeff) (Int64.of_int time))

let time_of_ms = time_of 1000000
let time_of_sec = time_of 1000000000

let timeout_of_bench bench = time_of_sec bench.timeout

(* Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
The files from PSPlib are also supposed to be well-formatted. *)
let read_rcpsp problem_path =
  let ext = String.lowercase_ascii (Filename.extension problem_path) in
  if String.equal ext psplib_ext then
    Sm_format.read_sm_file problem_path
  else if String.equal ext patterson_ext then
    Patterson.read_patterson_file problem_path
  else if String.equal ext pro_gen_ext then
    Pro_gen_max.read_pro_gen_file problem_path
  else
    eprintf_and_exit ("Unknown file extension `" ^ ext ^ "`.")
