# Benchmarking

In this chapter, we present [kobe](https://github.com/ptal/kobe), a constraint benchmarking tools suite for various solvers including GeCode, Chuffed and AbSolute.
The goals of this benchmarking tools suite are:

1. Reproducibility of the research results.
2. Report of the full results whereas in research papers it is often summarized.
3. Automate benchmarking of different solvers on different problems.
4. Analysis of the benchmarking results.

This work is on-going and replicability problems might still be present.
In case of problems, please do not hesitate to contact us on the [issues tracker](https://github.com/ptal/kobe/issues) or by [email](mailto:pierre.talbot@univ-nantes.fr).

## Getting started

First, make sure you followed the instruction of the general [getting started section](getting-started.html).
If so, you can install `kobe` through opam:

```sh
opam install kobe
```

You will have three new binaries in your path: `kobegen`, `kobe` and `kobeview`.

## Sheet cheats

This gives you a list of the valid values for some entries of the benchmarking configuration file (explained below).

* Families of problems supported (directory in the JSON entry `problem_sets.path`): `rcpsp`, `rcpsp-max`.
* Solvers kind (`solvers_kind`): see function `run_bench` in [kobe.ml](https://github.com/ptal/kobe/blob/master/src/kobe/kobe.ml).
* AbSolute domains (`solvers_kind.AbSolute.domains`): see function `bench_absolute` in [kobe.ml](https://github.com/ptal/kobe/blob/master/src/kobe/kobe.ml).
* Box splitting strategies (`solvers_kind.AbSolute.domains.strategies`): see function `make_box_strategy` in [kobe.ml](https://github.com/ptal/kobe/blob/master/src/kobe/kobe.ml).
* Octagon splitting strategies (`solvers_kind.AbSolute.domains.strategies`): see function `make_octagon_strategy` in [kobe.ml](https://github.com/ptal/kobe/blob/master/src/kobe/kobe.ml).
