.PHONY: run build clean fmt

run: build
	dune exec minesweeper

build:
	dune build

clean:
	dune clean

fmt:
	dune build @fmt
	@echo 'run "dune promote" to update files'

