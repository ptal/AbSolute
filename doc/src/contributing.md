# Contributing to AbSolute

This guide is for anybody who wants to contribute to AbSolute.
Before all, we encourage you to discuss your project with us to be sure your contributions can eventually be integrated.
Make sure you installed [OCaml and Opam](getting-started.html).

## Installing and building AbSolute

Install dependencies, clone and build AbSolute from the Github repository:

```sh
opam install dune extlib containers mlgmpidl mtime alcotest
git clone https://github.com/ptal/AbSolute
cd AbSolute
make build
make test
```

You might also be interested by cloning the following repositories which all relevant to the AbSolute project:

```sh
git clone https://github.com/ptal/kobe
git clone https://github.com/ptal/kobe-rcpsp
git clone https://github.com/ptal/kobe-sat
git clone https://github.com/ptal/kobe-database
git clone https://github.com/ptal/minisatml
git clone https://github.com/ptal/solvers-opam
git clone https://github.com/ptal/ptal.github.io
```

## Development flow

You should first create a branch or a fork, add and test the new feature, and then create a pull request to merge it into the mainstream repository.
In a more detailed way:

1. Make your changes in a branch.
2. Document the `.mli` files of your code.
3. Verify it passes all the tests, and that [Travis](https://travis-ci.org/ptal/AbSolute) succeeds to build and test your branch.
4. Send your changes into master.
5. Publish the new version (see below).

If you modify interfaces that are shared by several abstract domains (such as [abstract_domain.ml](https://github.com/ptal/AbSolute/blob/master/src/domains/abstract_domain.ml)), you should create an issue on Github to discuss about it with the team, since it impacts virtually all the project.

About the coding guidelines, you should try to code in the same way than the rest of the project, please observe how we code and do the same.
One very important rule is that you indent using *2 spaces* and __not tabulations__, and create `.ml` and its documented `.mli` companion for every new file.

In any case, do not hesitate to communicate and ask questions—stupid questions do not exist :-).
Questions and discussions are very important so you do not lose time during development, and do not go in a wrong direction.

## Publishing

We maintain version numbers according to [semantic versioning](http://semver.org/) rules.
We provide a script [publish.sh](https://github.com/ptal/solvers-opam/blob/master/publish.sh) to automatically update the version of your OPAM project, create the tag on git, add the new version in the [solvers-opam](https://github.com/ptal/solvers-opam) repository, and push all the changes online.
It can be used as follows:

```sh
./publish.sh major ../AbSolute ../solvers-opam
```

It releases a new major version of the project `AbSolute` rooted at `../AbSolute`, and where the OPAM packages repository is rooted at `../solvers-opam`.
The new version is installable with `opam update` followed by `opam install absolute`.

For more usage information, you can type:
```sh
./publish.sh
```

This script is only compatible to Linux (not yet tested on MacOSX), so if you are on Windows, you should read this script (which is documented) to know what are the necessary steps.