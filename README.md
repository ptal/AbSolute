# AbSolute

[![ptal on Travis CI][travis-image]][travis]

[travis-image]: https://travis-ci.org/ptal/AbSolute.png?branch=master
[travis]: https://travis-ci.org/ptal/AbSolute

Please consult the [AbSolute book](https://ptal.github.io/absolute/) for *research*, tutorial and general information.

_This fork is a lighter but cleaned version_ of the [initial project](https://github.com/mpelleau/AbSolute).
Features that have been removed might be reintroduced later if we need them.

AbSolute is a constraint solver based on abstract domains from the theory of abstract interpretation.
It implements the solving method presented in: ["A Constraint Solver Based on Abstract Domains"](https://hal.archives-ouvertes.fr/hal-00785604/file/Pelleau_Mine_Truchet_Benhamou.pdf).

AbSolute is still in developpement, and have not been fully tested.
Feel free to fill an [issue](https://github.com/ptal/AbSolute/issues) or contact any member of the developpement team if you want to report a bug or suggest a feature.

Contributors: Marie Pelleau, Ghiles Ziat, Alexandre Marechal, Pierre Talbot, Antoine Miné, Charlotte Truchet.
Supported by ANR CoVerif.

## Build the AbSolute book

You might want to build the book from the repository because you need it to be synchronized with a specific version of AbSolute or simply for offline usage.
Download the utility [mdbook](https://rust-lang-nursery.github.io/mdBook/):

```
curl https://sh.rustup.rs -sSf | sh
cargo install mdbook
```

Once installed, execute `make book`.
The manual is generated inside a local folder named `doc/book` and is directly opened in your browser.
