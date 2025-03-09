MAIN=bin/main.ml
RB_TREE=lib/RBTree.exe

all: main

main: 
	dune build
	ocaml $(MAIN)

rb_tree:
	dune build
	dune exec $(RB_TREE)

build: 
	dune build
tests: test

test: 
	dune runtest

watch: 
	dune runtest -w

clean:
	@dune clean