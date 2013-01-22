SRC = tools.ml top.mli top.ml gui.mli gui.ml main.ml
OCAMLFIND_OPTS = -package unix,lablgtk2,lablgtk2.sourceview2 -linkpkg -thread -g

UNAME := $(shell uname)
ifeq (WIN,$(findstring WIN,$(UNAME)))
  EXEEXT := .exe
  SRC += sigint_win.c
else
  EXEEXT :=
  SRC += sigint_unix.c
endif

byte$(EXEEXT): $(SRC)
	ocamlfind ocamlc -custom $(OCAMLFIND_OPTS) $(SRC) -o $@

native$(EXEEXT): $(SRC)
	ocamlfind ocamlopt $(OCAMLFIND_OPTS) $(SRC) -o $@

.PHONY: clean
clean:
	rm -f *.o *.cm* byte$(EXEEXT) native$(EXEEXT)
