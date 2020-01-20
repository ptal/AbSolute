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
  "Octagon", Test_octagon.tests;
  "Box", Test_box.tests;
  "Logic completion", Test_logic_completion.tests;
  "Octagon rewriter", Test_rewriter.tests;
  "Split strategies", Test_split_strategies.tests;
  "Type inference of formula", Test_typing.tests;
]

let () = Alcotest.run "AbSolute" test_suites
