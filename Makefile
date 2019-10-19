NAME = main.byte

all: build

build:
	ocamlbuild -use-ocamlfind ${NAME}

clean:
	ocamlbuild -clean

.PHONY: build clean
