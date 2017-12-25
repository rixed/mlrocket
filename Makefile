OCAMLC   = ocamlfind ocamlc -thread
OCAMLOPT = ocamlfind ocamlopt -thread
OCAMLDEP = ocamlfind ocamldep
OCAMLOPTFLAGS = -w Ael -g -annot
OCAMLFLAGS    = -w Ael -g -annot

REQUIRES = geom glop bricabrac

.PHONY: all clean install

PROGRAMS = mlrocket.opt
SOURCES = mlrocket.ml pic.ml weather.ml flower.ml rocket.ml sparkle.ml world.ml game.ml main.ml

all: opt
opt: $(PROGRAMS)
byte: $(PROGRAMS:.opt=.byte)

NAME = mlrocket

install: all
	ocamlfind install $(NAME) META mlrocket.opt

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .opt .byte

mlrocket.byte: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

mlrocket.opt: $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.s *.o *.byte *.opt .depend

# Dependencies
.depend: *.ml
	$(OCAMLDEP) -package "$(REQUIRES)" -I .. $^ > $@

-include .depend
