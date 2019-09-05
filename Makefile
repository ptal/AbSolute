# Frontend to dune.

.PHONY: default build install uninstall test clean

default: build

build:
	dune build

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

book:
	mdbook build -o doc/

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfX
	mdbook clean doc/book
