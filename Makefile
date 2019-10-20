NAME = main

all: build_byte build_native

build_native:
	ocamlbuild -r -tag bin_annot -Is board,player,color,parsing,res,option ${NAME}.native

build_byte:
	ocamlbuild -r -tag bin_annot -Is board,player,color,parsing,res,option ${NAME}.byte

clean:
	ocamlbuild -clean

.PHONY: build_native build_byte clean
