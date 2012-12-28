SRC = tools.ml top.mli top.ml gui.mli gui.ml main.ml
OCAMLFIND_OPTS = -package unix,lablgtk2 -linkpkg -thread -g

UNAME := $(shell uname)
ifeq (WIN,$(findstring WIN,$(UNAME)))
  EXEEXT := .exe
else
  EXEEXT :=
endif

byte$(EXEEXT): $(SRC)
	ocamlfind ocamlc $(OCAMLFIND_OPTS) $(SRC) -o $@

native$(EXEEXT): $(SRC)
	ocamlfind ocamlopt $(OCAMLFIND_OPTS) $(SRC) -o $@

.PHONY: clean
clean:
	rm -f *.cm* byte$(EXEEXT) native$(EXEEXT)
