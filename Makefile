.PHONY: san

all:
	dune build

san:
	dune build --profile san
