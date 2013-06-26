BYTE = _obuild/ocaml-top/ocaml-top.byte

all: $(BYTE)

$(BYTE): configure
	ocp-build -byte ocaml-top

ocp-build.root:
	ocp-build -init -scan

configure: ocp-build.root

clean:
	ocp-build -clean
	rm deps.dot

OCAMLFIND_OPTS = -package unix,lablgtk2,lablgtk2.sourceview2,str,ocp-indent-lexer,ocp-indent-utils,ocp-indent-lib,ocp-index-lib -I src -I _obuild/ocaml-top
SRC = $(wildcard src/*.ml) $(wildcard src/*.mli)
deps.dot: $(BYTE)
	ocamlfind ocamldoc $(OCAMLFIND_OPTS) -thread -dot -dot-reduce -o deps.dot \
	  $$( ocamlfind ocamldep $(OCAMLFIND_OPTS) $(SRC) \
	    | sed 's/^\([^ ]\+\)\.cmo :/\1.ml/; t ok; d; :ok; s/\.cmo/.ml/g; s/[^ ]*\.cmi//g; /\\$$/ { N; s/\\\n//; b ok }' \
	    | awk '{ for(i=2;i<=NF;i++) { print $$1, $$i } } 0' | tsort | tac)

.PHONY: all configure clean
