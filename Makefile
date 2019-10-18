all: ocaml-top

ALWAYS:

build: ALWAYS
	dune build @install # --dev

clean: ALWAYS
	rm -rf _build
	rm -f ocaml-top deps.dot

ocaml-top: build
	ln -s _build/install/default/bin/ocaml-top $@

install: ALWAYS
	dune install --prefix $(PREFIX)

uninstall: ALWAYS
	dune uninstall

WIN_INST = windows.inst
WIN_PFX = $(WIN_INST)/ocaml-top

$(WIN_INST): ALWAYS
	rm -rf $(WIN_INST)
	mkdir -p $(WIN_INST)
	dune build --ignore-promoted-rules ocaml-top-windows.install
	cp -rL _build/install/default.windows/bin $(WIN_PFX)
	cp -rL _build/install/default.windows/doc/ocaml-top $(WIN_PFX)/doc
	cp -rL _build/install/default.windows/share/ocaml-top $(WIN_PFX)/data
	cp data/logo.ico $(WIN_PFX)/data
	mv $(WIN_PFX)/ocaml-top $(WIN_PFX)/ocaml-top.exe
	strip $(WIN_PFX)/ocaml-top.exe
	utils/mkwinapp $(WIN_PFX)/ocaml-top.exe
	mkdir -p $(WIN_PFX)/share/glib-2.0/schemas
	cp /home/lg/ocamlpro/mxe/usr/x86_64-w64-mingw32.static.posix/share/glib-2.0/schemas/gschemas.compiled \
	  $(WIN_PFX)/share/glib-2.0/schemas
	cp -rL windows/make_msi.bat windows/bannrbmp.bmp windows/dlgbmp.bmp windows/ocaml-top.wxs $(WIN_INST)

#	echo 'let rec f x = f x in f ()' >$(WIN_PFX)/loop.ml

WIN_OCAML_PREFIX = $(shell opam var prefix)/windows-sysroot

win-inst-ocaml: ALWAYS
	mkdir -p $(WIN_PFX)/bin
	cp -rL $(WIN_OCAML_PREFIX)/bin/ocaml.exe $(WIN_PFX)/bin
	cp -rL $(WIN_OCAML_PREFIX)/bin/ocamlrun.exe $(WIN_PFX)/bin
	mkdir -p $(WIN_PFX)/lib/ocaml
	cp -rL $(WIN_OCAML_PREFIX)/lib/ocaml/stdlib.cmi $(WIN_PFX)/lib/ocaml
	cp -rL $(WIN_OCAML_PREFIX)/lib/ocaml/stdlib__*.cmi $(WIN_PFX)/lib/ocaml
	cp -rL $(WIN_OCAML_PREFIX)/lib/ocaml/stdlib.cma $(WIN_PFX)/lib/ocaml

# DO NOT USE; see winbuild.sh, install Wix on Windows and run the .bat file.

WIX = wine $(PWD)/windows/wix/

%.wixobj: %.wxs
	$(WIX)candle.exe -d"DataDir=ocaml-top\\data" -d"BinDir=ocaml-top\\bin" -d"DocDir=ocaml-top\\doc" -d"LibDir=ocaml-top\\lib" -d"ShareDir=ocaml-top\\share" -out $@ $^

$(WIN_INST)/%.wxs: $(WIN_INST)/ocaml-top
	cd $(WIN_INST) && $(WIX)heat.exe dir ocaml-top/$* -srd -dr $* -cg $* -gg \
	  -var var.DataDir -out $*.wxs

ocaml-top.msi: $(WIN_INST)/data.wixobj $(WIN_INST)/doc.wixobj $(WIN_INST)/lib.wixobj $(WIN_INST)/bin.wixobj $(WIN_INST)/share.wixobj windows/ocaml-top.wixobj
	cd $(WIN_INST) && $(WIX)light.exe -ext WixUIExtension -out ocaml-top.msi \
	   data.wixobj doc.wixobj lib.wixobj bin.wixobj share.wixobj ../windows/ocaml-top.wixobj
