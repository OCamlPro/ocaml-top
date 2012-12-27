LIBS = unix.cma threads.cma lablgtk.cma
SRC = tools.ml top.ml gui.mli gui.ml main.ml

byte: $(SRC)
	ocamlfind ocamlc -package unix,lablgtk2 -thread $(LIBS) $(SRC) -o byte

native: $(SRC)
	ocamlfind ocamlopt -package unix,lablgtk2 -thread $(LIBS:cma=cmxa) $(SRC) -o native
