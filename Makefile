BYTE = _obuild/ocp-edit-simple/ocp-edit-simple.byte

all: $(BYTE)

$(BYTE): configure
	ocp-build -byte ocp-edit-simple

ocp-build.root:
	ocp-build -init -scan

configure: ocp-build.root

clean:
	ocp-build -clean

.PHONY: all configure clean
