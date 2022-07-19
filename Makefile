default:
	opam update
	opam install . --deps-only
	dune build

build:
	dune build

install:
	opam update
	opam install --yes . --deps-only

lint:
	dune build @lint
	dune build @fmt
	
test:
	dune runtest 

clean:
	dune clean
	git clean -dfX
	rm -rf docs/

doc:
	make clean
	dune build @doc
	mkdir docs/
	cp -r ./_build/default/_doc/_html/* docs/

format:
	dune build @fmt --auto-promote
