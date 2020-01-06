(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(*
   Run all the OCaml test suites defined in the project.
*)

let test_suites: unit Alcotest.test list = [
  (* "BoxedOctagon", Test_boxed_octagon.tests; *)
  "Octagon", Test_octagon.tests;
  (* "Octagon_utilities", Test_octagon_utilities.tests; *)
  (* "Box", Test_box.tests; *)
  "Rewriter", Test_rewriter.tests;
  "Split strategies", Test_split_strategies.tests;
]

let () = Alcotest.run "AbSolute" test_suites
