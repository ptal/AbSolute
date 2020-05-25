# Introduction

Hello! You reached the `AbSolute` book.
`AbSolute` is a constraint solver written in OCaml for research purposes, but we hope that it can be used outside of research as well!
The main originality of this solver is to be based on _abstract interpretation_, which is a field studying static analysis of programs.
We and others have discovered similitudes between constraint programming and abstract interpretation at the theoretical level, and this solver is a practical experimentation of these findings.
In this book, you will learn how to [install AbSolute](getting-started.html) and how to [solve your own constraint problems](learn-absolute.html).
We also include a [guide to kobe](benchmarking.html) which is a benchmarking tool with support for AbSolute and other solvers.
For adventurous users, we provide a [guide for contributing](contributing.html) to Absolute, as well as some notes [about the research](research.html) relevant to this project.

This project is available on [github](https://github.com/ptal/absolute/).

## Highlight of AbSolute features

An abstract domain encapsulates a solver for a specific constraint language.
We summarize the supported abstract domains in the table below (`c1,c2` are constraints, `X,Y` variables and `a,b` constants).
Each abstract domain is parametrized by a bound type `Z`, `F` or `Q` if it contains variables over integers, floating point numbers or rational.

| Abstract domain | Constraint language | Propagation | Description |
| --------------- | -------------------- | ----------- | ----------- |
| `Box(V)`        | Interval constraints (`X <= a`, `X >= b`) | None | Simple Cartesian product of variable domains. `Box` is parametric in a variable domain `V` (interval, open/close interval, or BDD). |
| `SAT`    | Propositional formula (`(X \/ Y) /\ (true \/ Z)`) | Conflict driven clause learning | Based on Minisat. This domain is experimental (and probably bugged!). |
| `Octagon(B)`    | Logic difference constraints (`X - Y <= a`) | Incremental Floyd-Warshall algorithm | `B` is the bound type. |


In addition, new abstract domains can be derived from existing ones using _abstract transformers_.

| Abstract transformers | Constraint language | Propagation | Description |
| --------------- | -------------------- | ----------- | ----------- |
| `Direct_product(A1,..,An)` | Union of `A1`,...,`An`. | Component-wise propagation of each subdomains. | In addition to the classical direct product of abstract interpretation, it has some facilities for transformers that shares subdomains. |
| `Logic_completion(A)` | Quantifier-free logic formula where predicates are constraints in `A` | Based on the entailment of constraint in `A`. | Equip an abstract domain `A` with logical formula. Also called "natural domain SMT". |
| `Propagator_completion(A)` | Arithmetic constraints, functions (`sin`, `cos`, `sqrt`,...). |  HC4 algorithm | Equip an abstract domain `A` with propagators (functions implementing arithmetic constraints). |
| `Delayed_product(A,B)` | Union of `A` and `B`. | Transfer over-approximation `A`-constraints into `B`, and then an exact approximation whenever the constraint is instantiated enough. | Can be used to transfer non-linear constraints to a linear solver whenever the non-linear variables are instantiated. |
| `Event_loop(A1,...,An)` | None | Propagation loop of the propagators in `A1`,...,`An` | Meta abstract domain, computing the closure of `A1`...`An` more efficiently than `Direct_product`. `Ai` closure must be decomposable into propagators (i.e. sub-closure operators). |

In addition, each domain is equipped with numerous splitting strategies that can impact the performance.

## What to expect next?

A linear programming solver such as VPL, or the one of Apron should be added.
The next big step is to integrate ideas from _abstract conflict driven learning_ (ACDL) to support conflicts learning.

## AbSolute ecosystem

We have several projects relevant to AbSolute that are worth checking out:

* [kobe](https://github.com/ptal/kobe): Constraint benchmarking tools suite.
It is described in more depth in the [benchmarking chapter](benchmarking.md).
* [minisatml](https://github.com/ptal/minisatml): SAT solver based on MiniSAT, note that this project is a library not directly executable.
* [ptal.github.io](https://github.com/ptal/ptal.github.io): The repository hosting this book.
* [solvers-opam](https://github.com/ptal/solvers-opam): The OPAM packages repository containing libraries and executables surrounding constraint solving.
