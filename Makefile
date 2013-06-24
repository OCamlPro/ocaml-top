BYTE = _obuild/ocaml-top/ocaml-top.byte

all: $(BYTE)

$(BYTE): configure
	ocp-build -byte ocaml-top

ocp-build.root:
	ocp-build -init -scan

configure: ocp-build.root

clean:
	ocp-build -clean

.PHONY: all configure clean
