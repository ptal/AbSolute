# Getting Started

The installation process should be easy, but do not hesitate to reach out for help if needed.

## Requirements

The following is a list of the dependencies to build AbSolute; note that we explain below how to install `OCaml` if you do not have it already.

* An ANSI C compiler
* [OCaml](http://ocaml.org/) >= 4.09.0
* [GMP library](https://gmplib.org/)
* [MPFR library](https://www.mpfr.org/)

## Installation

We install OCaml and AbSolute through the OCaml package manager [opam](http://opam.ocaml.org/).
First, [install opam](http://opam.ocaml.org/doc/Install.html) and the necessary libraries with your package manager and initialize opam:
```sh
sudo apt-get install libgmp3-dev
sudo apt-get install libmpfr-dev
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) # see opam documentation for alternatives.
opam init --comp 4.09.0+flambda # Initialize ~/.opam with a freshly compiled OCaml 4.07.1
```
The version of OPAM must be greater than 2.0.
Note that only the version 1.2 is available in the stable packages of Debian, the version 2 can be installed with the command given above.

The next step is to download and build AbSolute.
If you intent to modify the source code and possibly contribute to the project, jump to the [contributing chapter](contributing.html).
Otherwise, you can install it from `opam`:

```sh
opam repo add solvers https://github.com/ptal/solvers-opam.git
opam install absolute
```

`AbSolute` is currently only a library project, so it must be used in an OCaml project, which we explain in the [next chapter](learn-absolute.html).

To update `absolute` (and any OPAM package) to the latest version, you can simply type:

```sh
opam update
opam install absolute
```
