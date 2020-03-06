# Learn AbSolute

We show step-by-step how to use AbSolute in order to solve a constraint problems.
This code presented in this chapter is available online at [absolute-demo](https://github.com/ptal/absolute-demo/blob/master/src/nqueens.ml).
This project can be forked and used as a template for your own project.

This simple introduction cannot serve as a full introduction to constraint programming, and we refer to [this online class](https://www.coursera.org/learn/basic-modeling) to learn how to model a constraint problem.

In this tutorial, you will learn how to solve the famous n-queens problem in AbSolute.
It encompasses the following steps:

1. Create a model of the N-Queens using the structures of AbSolute.
2. Build a constraint solver by assembling abstract domains together that is able to solve this problem.
3. Type the formula created in (1) into the abstract domain.
4. Run the solver and print the solution.

## Model of the N-Queens problem

The N-Queens problem is a toy constraint problem where we try to place `n` queens on a chess board of size `n*n` such that no queen can attack each other.
A complete description is available on [Wikipedia](https://en.wikipedia.org/wiki/Nqueens).

A simple model for this problem is constituted of `n` variables `x1,...,xN`, one for each queen.
Since, it is only possible to have one queen per line on the chess board, we can already decide that `x1` is on the first line, `x2` on the second, etc.
The following function creates a list of string variables:

```rust
(** [make_vars n] creates `n` variables named `x1`,...,`xn`. *)
let rec make_vars = function
  | 0 -> []
  | n -> ("x" ^ (string_of_int n))::(make_vars (n-1))
```

The value of the variable `xi` will be the column of `xi` on the chess board.
Therefore, a queen variable can take a value between `1` and `n`.
These corresponds to constraints of the form `x1 >= 0 /\ x1 <= n`, which translates in OCaml as:

```rust
(** [var_in_dom l u x] ensures that `x >= l` and `x <= u`. *)
let var_in_dom l u x =
  [Cmp(Var x, GEQ, Cst(Bound_rat.of_int l, Int));
   Cmp(Var x, LEQ, Cst(Bound_rat.of_int u, Int))]

(** [vars_in_dom l u vars] ensures that all variables in `vars` are in the interval [l..u]. *)
let rec vars_in_dom l u = function
  | [] -> []
  | x::vars -> (var_in_dom l u x)@(vars_in_dom l u vars)
```

Finally, we need to ensure that two queens cannot attack each other.
For instance, to check that `xi` and `xj` do not attack each other, we need to fulfill the following constraints (`i < j`):

* `xi != xj`: `xi` and `xj` are not on the same column.
* `qi + i != qj + j`: `xi` and `xj` are not on a same ascending diagonal.
* `qi - i != qj - j`: `xi` and `xj` are not on a same descending diagonal.

This translates into OCaml as follows:

```rust
let noattack vars i j =
  let qi, qj = List.nth vars i, List.nth vars j in
  let ci = Cst(Bound_rat.of_int i, Int) in
  let cj = Cst(Bound_rat.of_int j, Int) in
  [Cmp(Var qi, NEQ, Var qj);
   Cmp(Binary(Var qi, ADD, ci), NEQ, Binary(Var qj, ADD, cj));
   Cmp(Binary(Var qi, SUB, ci), NEQ, Binary(Var qj, SUB, cj))]

let rec noattack_i vars i j =
  if j >= List.length vars then []
  else (noattack vars i j)@(noattack_i vars i (j+1))

let noattack_all vars =
  let rec aux i =
    if i >= List.length vars then []
    else (noattack_i vars i (i+1))@(aux (i+1)) in
  aux 0
```

By assembling all these constraints, we can design the full formula for the n-queens problem:

```rust
let make_nqueens n =
  let vars = List.rev (make_vars n) in
  let domains = List.rev (vars_in_dom 1 n vars) in
  let constraints = List.rev (noattack_all vars) in
  let rec make_formula = function
    | [] -> QFFormula (conjunction (domains@constraints))
    | x::vars -> Exists(x, Concrete Int, make_formula vars) in
  make_formula vars
```

It generates a conjunctive formula with variables existentially quantified, for instance: `Ǝx1.Ǝx2...Ǝxn.x1 >= 1 /\ ... /\ x7 - 6 <> x8 - 7`.

## An abstract domain for the N-Queens problem

AbSolute is designed to be modular, thus you can craft your own constraint solver accordingly to your problem.
We design an abstract domain one supporting integer arithmetic constraints:

```
module Box = Box.Make(Bound_int)(Itv.Itv)(Box_split.First_fail_LB)
module PC = Propagator_completion(Box.Vardom)(Box)
module E = Event_loop(Event_atom(PC))
module A = Direct_product(
  Prod_cons(Box)(
  Prod_cons(PC)(
  Prod_atom(E))))

let box_uid = 1
let pc_uid = 2
let event_uid = 3

let init () : A.t =
  let box = ref (Box.empty box_uid) in
  let pc = ref (PC.init PC.I.{uid=pc_uid; a=box}) in
  let event = ref (E.init event_uid pc) in
  A.init 0 (Owned box, (Owned pc, Owned event))
```

The `Box` abstract domain supports variables defined on interval domain (e.g. `x >= 1 /\ x <= 10`).
The propagator completion `PC` equips `Box` with arbitrary arithmetic constraints (e.g. `x * x + y - 2 > 0`).
The domain `Event_loop` is a meta abstract domain, not supporting any constraint, but useful to compute efficiently the fixpoint of the closure operator of `PC`.
These three domains are next assembled together in a direct product `A`.
`A` is only the type of the abstract domain, thus we need to create an element of `A`, which is realized by the function `init`.
Each abstract element has a dedicated unique identifier (UID), that is useful in the next section.

## Type the formula in the abstract domain

We now have the formula on one side, and the abstract domain on the other side.
The remaining step is to assign each part of the formula to the UID of an abstract element.
We call this process _typing_.

```rust
(** Given a formula, we assign each of its variables and constraints to an abstract domain.
   For the abstract domain `A`, all the variables and interval constraint go into `Box`, and the rest go into `PC`. *)
let type_formula formula =
  let rec aux' = function
    (* Domain constraint go into `Box`. *)
    | Cmp (Var x, LEQ, Cst (i,ty)) -> box_uid, TCmp(Var x, LEQ, Cst (i,ty))
    | Cmp (Var x, GEQ, Cst (i,ty)) -> box_uid, TCmp(Var x, GEQ, Cst (i,ty))
    (* Other constraint go into `PC`. *)
    | Cmp c -> pc_uid, TCmp c
    | And (f1,f2) -> pc_uid, TAnd(aux' f1, aux' f2)
    (* All other constraint are not supported in this abstract domain. *)
    | _ -> raise (Wrong_modelling "Constraint not supported by the current abstract domain.") in
  let rec aux = function
    | Exists(name, ty, qf) -> TExists({name; ty; uid=box_uid}, aux qf)
    | QFFormula f -> TQFFormula (aux' f) in
  aux formula
```

The unary constraint of the variable are directly typed into `box_uid`, while the others are typed into `pc_uid`.
The abstract domain `A` does support any constraint (for instance disjunction), thus we throw an error if we encounter an unsupported constraint.

## Finding a solution

Now that we have a typed formula, the remaining and most important step is to find a solution to this formula.
A solution is an assignment of the variable such that all constraints are satisfied.
We first instantiate the solver with the created abstract domain.

```
module Solver = Fixpoint.Solver.Make(A)
module T = Solver.T
```

The next function is the main and last one, it assembles all the previous step and call the solver on the formula:

```rust
let _ =
  let nqueens = 8 in
  try
    (* (1) Merge the three previous steps. *)
    let qformula = make_nqueens nqueens in
    let a = init () in
    let tf = type_formula qformula in
    (* (1b) Print the typed formula (optional). *)
    print_tformula a tf;
    (* (2) Interpret the typed formula into the abstract element. *)
    let a, cs = A.interpret a Exact tf in
    (* (3) Add the interpreted constraints into the abstract element. *)
    let a = List.fold_left A.weak_incremental_closure a cs in
    (* (4) Create the solver. *)
    let bound_sol = T.BoundSolutions 1 in
    let transformer = T.init a [bound_sol] in
    (* (5) Run the solver. *)
    let (gs,_) =
      try Solver.solve transformer
      with Solver.T.StopSearch t -> t in
    if T.(gs.stats.sols) > 0 then
      print_solution nqueens gs
    else
      Printf.printf "No solution found."
  with e ->
    Printf.printf "Exception: %s\n" (Printexc.to_string e)

```

The project can be compiled and run as follows:

```
git clone https://github.com/ptal/absolute-demo.git
cd absolute-demo
make
./_build/install/default/bin/nqueens
```

You can change the variable `nqueens` to decrease or increase the size of the chess board.
Check for instance with `nqueens = 3` and `nqueens = 4`, and verify the solution on paper.

Many other features are available, and you can explore them in the [developer documentation](ptal.github.io/doc)
