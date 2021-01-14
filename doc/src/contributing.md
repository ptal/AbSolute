# Contributing to AbSolute

This guide is for anybody who wants to contribute to AbSolute.
Before all, we encourage you to discuss your project with us to be sure your contributions can eventually be integrated.
Make sure you installed [OCaml and Opam](getting-started.html).

## Installing and building AbSolute

Install dependencies, clone and build AbSolute from the Github repository:

```sh
opam install dune extlib containers-data mlgmpidl mtime alcotest
git clone https://github.com/ptal/AbSolute
cd AbSolute
make build
make test
```

You might also be interested by cloning the following repositories which are all relevant to the AbSolute project:

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

If you modify interfaces that are shared by several abstract domains (such as [abstract_domain.mli](https://github.com/ptal/AbSolute/blob/master/src/domains/abstract_domain.mli)), you should create an issue on Github to discuss about it with the team, since it impacts virtually all the project.
It is important to try to keep the number of functions in the signature minimal.
If you notice a function is not used anymore, you should not hesitate to remove it; but please contact us to validate your hypothesis so you don't lose time trying to remove it.

About the coding guidelines, you should try to code in the same way than the rest of the project, please observe how we code and do the same.
One very important rule is that you indent using *2 spaces* and __not tabulations__, and create `.ml` and its documented `.mli` companion for every new file.

In any case, do not hesitate to communicate and ask questions; stupid questions do not exist :-).
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

## Local version of AbSolute in OPAM

It is often convenient to have a local version of AbSolute in OPAM, for instance when moving from `kobe` to `AbSolute` so we can detect bugs before committing and publishing a new version.
We first pin a local version of AbSolute:

```sh
# Remove the currently installed version of AbSolute (not sure if it is necessary though).
opam remove absolute
# Pin a local version of AbSolute where `AbSolute` is the name of the git repository.
opam pin add absolute AbSolute
```

The next step is to edit the OPAM configuration file of AbSolute in order to avoid being synchronized with git (since we want to work solely on the local copy).
Write `opam pin edit absolute` in your terminal, and change the following line:

```sh
url {
  src:
    "git+file:///home/ptalbot/repositories/absolute-project/AbSolute#master"
}
```

by removing the `git+` part of the URL:

```sh
url {
  src:
    "file:///home/ptalbot/repositories/absolute-project/AbSolute#master"
}
```

Each time you make some changes and want to use the newest local version of AbSolute just type:
```sh
opam upgrade .
```
in the repository of AbSolute.
