# Getting Started

The installation process should be easy, but do not hesitate to reach out for help if needed.

## Requirements

The following is a list of the dependencies to build AbSolute; note that we explain below how to install `OCaml` if you do not have it already.

* An ANSI C compiler
* [OCaml](http://ocaml.org/) >= 4.07.1

## Installation

We install OCaml and AbSolute through the OCaml package manager [opam](http://opam.ocaml.org/).
First, [install opam](http://opam.ocaml.org/doc/Install.html) with your package manager and initialize it:
```sh
apt-get install opam # on Debian, see opam documentation for other distributions.
opam init --comp 4.07.1+flambda # Initialize ~/.opam with a freshly compiled OCaml 4.07.1
```

The next step is to download and build AbSolute.
If you intent to modify the source code and possibly contribute to the project, jump to the [contributing chapter](contributing.html).
Otherwise, you can install it from `opam`:

```sh
opam repo add solvers git@github.com:ptal/solvers-opam.git
opam install absolute
```

`AbSolute` is currently only a library project, so it must be used in an OCaml project, which we explain in the [next chapter](learn-absolute.html).
