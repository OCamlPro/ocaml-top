include Makefile.config

TARGET = _obuild/ocaml-top/ocaml-top
BYTE = $(TARGET).byte
NATIVE = $(TARGET).asm

all: ocaml-top

ALWAYS:

$(BYTE): ALWAYS _obuild/ocp-build.root
	ocp-build -byte -no-asm ocaml-top

$(NATIVE): ALWAYS _obuild/ocp-build.root
	ocp-build -asm ocaml-top

ocaml-top: $(NATIVE)
	cp $^ $@

.PHONY: clean
clean:
	ocp-build clean
	rm -f ocaml-top deps.dot

configure: _obuild/ocp-build.root ALWAYS

_obuild/ocp-build.root:
	ocp-build init
	ocp-build configure -install-destdir $(PREFIX)

.PHONY: install
RESOURCES = data/ocaml.lang data/light.xml data/dark.xml data/def.lang data/language2.rng data/toplevel_init.ml data/logo.png
ICONS = $(wildcard data/icons/*.png) $(wildcard data/icons/*.gif)
DATADIR = $(PREFIX)/share/ocaml-top
install: $(NATIVE)
	ocp-build install ocaml-top -install-destdir $(PREFIX)
	mkdir -p $(DATADIR)
	cp $(RESOURCES) $(DATADIR)
	mkdir -p $(DATADIR)/icons
	cp $(ICONS) $(DATADIR)/icons

uninstall:
	ocp-build -query-has ocaml-top
	rm -rf $(DATADIR)
	ocp-build uninstall ocaml-top -install-destdir $(PREFIX)

OCAMLFIND_OPTS = -package unix,lablgtk2,lablgtk2.sourceview2,str,ocp-indent.lexer,ocp-indent.utils,ocp-indent.lib,ocp-index.lib -I src -I _obuild/ocaml-top
SRC = $(wildcard src/*.ml) $(wildcard src/*.mli)
deps.dot: $(BYTE)
	ocamlfind ocamldoc $(OCAMLFIND_OPTS) $(SRC) \
	  -thread -dot -dot-reduce -o $@

.PHONY: all configure clean
