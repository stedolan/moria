all: example
	./$^

example: dwarf.mli dwarf.ml example.ml
	ocamlc dwarf.mli && ocamlc dwarf.ml example.ml -o $@
