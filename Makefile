NAME = main

all: build_byte build_native

build_native:
	ocamlbuild -tag bin_annot -I sources -I interfaces ${NAME}.native

build_byte:
	ocamlbuild -tag bin_annot -I sources -I interfaces ${NAME}.byte

clean:
	ocamlbuild -clean

.PHONY: build_native build_byte clean
