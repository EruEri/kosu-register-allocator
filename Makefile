.PHONY: san clean

all:
	dune build

san:
	dune build --profile san

clean:
	dune clean
