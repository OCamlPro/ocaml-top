all: ocaml-top

ALWAYS:

build: ALWAYS
	jbuilder build @install # --dev

clean: ALWAYS
	rm -rf _build
	rm -f ocaml-top deps.dot

ocaml-top: build
	ln -s _build/install/default/bin/ocaml-top $@

install: ALWAYS
	jbuilder install --prefix $(PREFIX)

uninstall: ALWAYS
	jbuilder uninstall
