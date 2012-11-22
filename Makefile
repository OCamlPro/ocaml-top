all: _obuild/ocp-edit-simple/ocp-edit-simple.byte

_obuild/ocp-edit-simple/ocp-edit-simple.byte: configure
	ocp-build -byte ocp-edit-simple

installed.ocp:
	ocp-build-infer-env `ocamlc -where` `opam config -var lib`
ocp-build.root: installed.ocp
	ocp-build -init -scan
configure: ocp-build.root

.PHONY: all configure
