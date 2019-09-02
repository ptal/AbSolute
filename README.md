# AbSolute

[![ptal on Travis CI][travis-image]][travis]

[travis-image]: https://travis-ci.org/ptal/AbSolute.png
[travis]: https://travis-ci.org/ptal/AbSolute

_This fork is a lighter but cleaned version_ of the initial project.
Features that have been removed might be reintroduced later if we need them.

AbSolute is a constraint solver based on abstract domains from the theory of abstract interpretation.
It implements the solving method presented in: ["A Constraint Solver Based on Abstract Domains"](https://hal.archives-ouvertes.fr/hal-00785604/file/Pelleau_Mine_Truchet_Benhamou.pdf).

AbSolute is still in developpement, and have not been fully tested.
Feel free to fill an [issue](https://github.com/mpelleau/AbSolute/issues) or contact any member of the developpement team if you want to report a bug or suggest a feature.

Contributors: Marie Pelleau, Ghiles Ziat, Alexandre Marechal, Pierre Talbot, Antoine Miné, Charlotte Truchet.
Supported by ANR CoVerif.

## Getting Started

The installation process should be easy, if you have any problem, see the Section `Troubleshooting`, fill an issue or email us directly.

### Requirements

The following is a list of the dependencies to build AbSolute; note that we explain in the next section how to install `OCaml` if you do not have it already.

- An ANSI C compiler
- OCaml >= 4.07.1 : http://ocaml.org/

### Installation

We install OCaml and AbSolute through the OCaml package manager [opam](http://opam.ocaml.org/).
First, [install opam](http://opam.ocaml.org/doc/Install.html) with your package manager and initialize it:
```sh
apt-get install opam # on Debian, see opam documentation for other distributions.
opam init --comp 4.07.1+flambda # Initialize ~/.opam with a freshly compiled OCaml 4.07.1
```

The next step is to download and build AbSolute.
If you intent to modify the source code and possibly contribute to the project, jump to the "Developpers" section.
Otherwise, you can install it from `opam`:

```sh
opam repo add solvers git@github.com:ptal/solvers-opam.git
opam install absolute
```

`AbSolute` is currently only a library project, so it must be used in an OCaml project, see [kobe](https://github.com/ptal/kobe) for an example.

### Developpers

Install dependencies, clone and build AbSolute from the Github repository:

```sh
opam install dune extlib containers mlgmpidl mtime alcotest
git clone https://github.com/ptal/AbSolute
cd AbSolute
make build
make test
```

## Research

AbSolute has generate several research papers.
Currently, the most general one to cite is:

```bibtex
@inproceedings{DBLP:conf/vmcai/PelleauMTB13,
  author    = {Marie Pelleau and
               Antoine Min{\'{e}} and
               Charlotte Truchet and
               Fr{\'{e}}d{\'{e}}ric Benhamou},
  title     = {A Constraint Solver Based on Abstract Domains},
  booktitle = {Verification, Model Checking, and Abstract Interpretation, 14th International
               Conference, {VMCAI} 2013, Rome, Italy, January 20-22, 2013. Proceedings},
  pages     = {434--454},
  year      = {2013},
  crossref  = {DBLP:conf/vmcai/2013},
  url       = {https://doi.org/10.1007/978-3-642-35873-9\_26},
  doi       = {10.1007/978-3-642-35873-9\_26},
  timestamp = {Wed, 24 May 2017 08:30:31 +0200},
  biburl    = {https://dblp.org/rec/bib/conf/vmcai/PelleauMTB13},
  bibsource = {dblp computer science bibliography, https://dblp.org}
}
```
