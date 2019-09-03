# Introduction

Hello! You reached the `AbSolute` book.
`AbSolute` is a constraint solver written in OCaml for research purposes, but we hope that it can be used outside of research as well!
The main originality of this solver is to be based on _abstract interpretation_, which is a field studying static analysis of programs.
We have discovered similitudes between constraint programming and abstract interpretation at the theoretical level, and this solver is a practical experimentation of these findings.
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
| `Box(V)`        | Arithmetic constraints, functions (`sin`, `cos`, `sqrt`,...). |  HC4 algorithm, propagation loop | `Box` is parametric by a variable domain `V` (interval, open/close interval, or BDD). |
| `Box_reified`   | Box constraint and reified constraint of the form (`X <=> c` where `X` is equal to `0` or `1`). | Same as `Box` + support for reified constraints | `Box_reified` is parametrized by a `Box` domain. |
| `Octagon`       | Logic difference constraints (`X - Y <= a`) | Incremental Floyd-Warshall algorithm | |
| `Box_octagon_disjoint`   | Box and octagon constraints with equivalence constraints (`c1 <=> c2` where `c1` a box constraint and `c2` an octagonal constraint).  | Bridge between box and octagon domains when they are defined on disjoint set of variables. |

In addition, each domain is equipped with numerous splitting strategies that can impact the performance.

## What to expect next?

We currently work on integrating a SAT solver, integrating back some features from the initial version of [AbSolute](https://github.com/mpelleau/) such as support for linear programming through VPL and Apron library, and reduced product to combine domains.

## AbSolute ecosystem

We have several projects relevant to AbSolute that are worth checking out:

* [kobe](https://github.com/ptal/kobe): Constraint benchmarking tools suite.
It is described in more depth in the [benchmarking chapter](benchmarking.md).
* [ptal.github.io](https://github.com/ptal/ptal.github.io): The repository hosting this book.
* [solvers-opam](https://github.com/ptal/solvers-opam): The OPAM packages repository containing libraries and executables surrounding constraint solving.
