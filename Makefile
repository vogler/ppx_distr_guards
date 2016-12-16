name = ppx_distr_guards
bin = $(name).native

build:
	ocamlbuild -package compiler-libs.common $(bin)

test:
	ocamlbuild -package compiler-libs.common $(bin) && ocamlfind ppx_tools/rewriter ./$(bin) test.ml

clean:
	ocamlbuild -clean

.PHONY: build test clean
